{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Divider
Description : Integral divider processor unit with pipeline
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Divider (
    Divider (..),
    divider,
    Ports (..),
    IOPorts (..),
) where

import Control.Monad
import Data.Default
import Data.List (partition)
import Data.List qualified as L
import Data.Maybe
import Data.Set qualified as S
import Data.String.Interpolate
import Data.String.ToString
import NITTA.Intermediate.Functions qualified as F
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Time
import NITTA.Project
import NITTA.Utils
import NITTA.Utils.ProcessDescription
import Numeric.Interval.NonEmpty (singleton, sup, (...))

data InputDesc
    = Numer
    | Denom
    deriving (Show, Eq)

data OutputDesc
    = Quotient
    | Remain
    deriving (Show, Eq)

data Divider v x t = Divider
    { jobs :: [Job v x t]
    , remains :: [F v x]
    , process_ :: Process t (StepInfo v x t)
    , pipeline :: t
    , mock :: Bool
    }

instance (Show v, Show t) => Show (Divider v x t) where
    show Divider{jobs} = show jobs

divider pipeline mock =
    Divider
        { jobs = []
        , remains = []
        , process_ = def
        , pipeline
        , mock
        }

instance Time t => Default (Divider v x t) where
    def = divider 4 True

instance Default x => DefaultX (Divider v x t) x

instance Ord t => WithFunctions (Divider v x t) (F v x) where
    functions Divider{process_, remains, jobs} =
        functions process_
            ++ remains
            ++ map function jobs

data Job v x t
    = WaitArguments
        { function :: F v x
        , arguments :: [(InputDesc, v)]
        }
    | WaitResults
        { function :: F v x
        , readyAt :: t
        , restrict :: Maybe t
        , results :: [(OutputDesc, S.Set v)]
        }
    deriving (Eq, Show)

instance Ord v => Variables (Job v x t) v where
    variables WaitArguments{arguments} = S.fromList $ map snd arguments
    variables WaitResults{results} = S.unions $ map snd results

isWaitArguments WaitArguments{} = True
isWaitArguments _ = False

isWaitResults WaitResults{} = True
isWaitResults _ = False

instance VarValTime v x t => ProcessorUnit (Divider v x t) v x t where
    tryBind f pu@Divider{remains}
        | Just (F.Division (I _n) (I _d) (O _q) (O _r)) <- castF f =
            Right pu{remains = f : remains}
        | otherwise = Left $ "Unknown functional block: " ++ show f
    process = process_
    parallelismType _ = Pipeline

instance (Var v, Time t) => Locks (Divider v x t) v where
    locks Divider{jobs, remains} = L.nub $ byArguments ++ byResults
        where
            byArguments
                | Just wa@WaitArguments{function} <- L.find isWaitArguments jobs =
                    [ Lock{lockBy, locked}
                    | lockBy <- S.elems $ variables wa
                    , locked <- S.elems $ unionsMap variables remains
                    ]
                        ++ [ Lock{lockBy, locked}
                           | lockBy <- S.elems $ variables wa
                           , locked <- S.elems $ outputs function
                           ]
                | otherwise = concatMap locks remains
            byResults
                | Just wr <- firstWaitResults jobs =
                    let blocked = filter (\j -> isWaitResults j && j /= wr) jobs
                     in [ Lock{lockBy, locked}
                        | lockBy <- S.elems $ variables wr
                        , locked <- S.elems $ unionsMap variables blocked
                        ]
                | otherwise = []

instance BreakLoopProblem (Divider v x t) v x
instance ConstantFoldingProblem (Divider v x t) v x
instance OptimizeAccumProblem (Divider v x t) v x
instance OptimizeLutProblem (Divider v x t) v x
instance ResolveDeadlockProblem (Divider v x t) v x

function2WaitArguments f
    | Just F.Division{F.denom = I denom, F.numer = I numer} <- castF f =
        WaitArguments
            { function = f
            , arguments = [(Denom, denom), (Numer, numer)]
            }
    | otherwise = error $ "internal divider error: " <> show f

function2WaitResults readyAt f
    | Just F.Division{F.quotient = O quotient, F.remain = O remain} <- castF f =
        WaitResults
            { function = f
            , readyAt
            , restrict = Nothing
            , results = filterEmptyResults [(Quotient, quotient), (Remain, remain)]
            }
    | otherwise = error "internal error"

filterEmptyResults rs = filter (not . null . snd) rs

firstWaitResults jobs =
    let jobs' = filter isWaitResults jobs
     in if null jobs'
            then Nothing
            else Just $ minimumOn readyAt jobs'

instance VarValTime v x t => EndpointProblem (Divider v x t) v t where
    endpointOptions pu@Divider{remains, jobs} =
        let executeNewFunction
                | any isWaitArguments jobs = []
                | otherwise = concatMap (map target . S.elems . inputs) remains
            waitingArguments =
                maybe [] (map target . S.elems . variables) $ L.find isWaitArguments jobs
            waitResults
                | Just WaitResults{readyAt, results, restrict} <- firstWaitResults jobs =
                    let at = max readyAt (nextTick pu) ... fromMaybe maxBound restrict
                     in map (sources at . snd) results
                | otherwise = []
         in concat [executeNewFunction, waitingArguments, waitResults]
        where
            target v = EndpointSt (Target v) $ TimeConstraint (nextTick pu ... maxBound) (singleton 1)
            sources at vs = EndpointSt (Source vs) $ TimeConstraint at (singleton 1)

    endpointDecision pu@Divider{jobs, remains, pipeline} d@EndpointSt{epRole = Target v, epAt}
        | ([f], remains') <- partition (S.member v . inputs) remains =
            let pu' =
                    pu
                        { jobs = function2WaitArguments f : jobs
                        , remains = remains'
                        }
             in endpointDecision pu' d
        | ([WaitArguments{function, arguments}], jobs') <- partition (S.member v . variables) jobs =
            let (tag, arguments') = case partition ((== v) . snd) arguments of
                    ([(tag', _v)], other) -> (tag', other)
                    _ -> error "Divider: endpointDecision: internal error"
                nextTick' = sup epAt + 1
             in case arguments' of
                    [] ->
                        let job' = function2WaitResults (nextTick' + pipeline + 1) function
                            restrictResults =
                                map
                                    ( \case
                                        wa@WaitResults{restrict = Nothing} -> wa{restrict = Just (nextTick' + pipeline)}
                                        other -> other
                                    )
                         in pu
                                { jobs = job' : restrictResults jobs'
                                , process_ = execSchedule pu $ do
                                    scheduleEndpoint_ d $ scheduleInstructionUnsafe epAt $ Load tag
                                    scheduleInstructionUnsafe_ (singleton nextTick') Do
                                }
                    _arguments' ->
                        pu
                            { jobs = WaitArguments{function, arguments = arguments'} : jobs'
                            , process_ = execSchedule pu $ do
                                scheduleEndpoint_ d $ scheduleInstructionUnsafe epAt $ Load tag
                            }
    endpointDecision pu@Divider{jobs} d@EndpointSt{epRole = Source vs, epAt}
        | ([job@WaitResults{results, function}], jobs') <- partition ((vs `S.isSubsetOf`) . variables) jobs =
            let ((tag, allVs), results') = case partition ((vs `S.isSubsetOf`) . snd) results of
                    ([(tag_, allVs_)], other) -> ((tag_, allVs_), other)
                    _ -> error "Divider: endpointDecision: internal error"
                allVs' = allVs S.\\ vs
                results'' = filterEmptyResults $ (tag, allVs') : results'
                jobs'' =
                    if null results''
                        then jobs'
                        else job{results = results''} : jobs'
             in pu
                    { jobs = jobs''
                    , process_ = execSchedule pu $ do
                        scheduleEndpoint_ d $ scheduleInstructionUnsafe epAt $ Out tag
                        when (null jobs') $ do
                            scheduleFunctionFinish_ [] function $ 0 ... sup epAt
                    }
    endpointDecision _pu d = error [i|incorrect decision #{ d } for Divider|]

instance Controllable (Divider v x t) where
    data Instruction (Divider v x t)
        = Load InputDesc
        | Do
        | Out OutputDesc
        deriving (Show)

    data Microcode (Divider v x t) = Microcode
        { selSignal :: Bool
        , wrSignal :: Bool
        , oeSignal :: Bool
        }
        deriving (Show, Eq, Ord)

    zipSignalTagsAndValues DividerPorts{..} Microcode{..} =
        [ (sel, Bool selSignal)
        , (wr, Bool wrSignal)
        , (oe, Bool oeSignal)
        ]

    usedPortTags DividerPorts{sel, wr, oe} = [sel, wr, oe]

    takePortTags (sel : wr : oe : _) _ = DividerPorts sel wr oe
    takePortTags _ _ = error "can not take port tags, tags are over"

instance Default (Microcode (Divider v x t)) where
    def =
        Microcode
            { selSignal = False
            , wrSignal = False
            , oeSignal = False
            }
instance UnambiguouslyDecode (Divider v x t) where
    decodeInstruction (Load Numer) = def{wrSignal = True, selSignal = False}
    decodeInstruction (Load Denom) = def{wrSignal = True, selSignal = True}
    decodeInstruction Do = def{wrSignal = True, oeSignal = True}
    decodeInstruction (Out Quotient) = def{oeSignal = True, selSignal = False}
    decodeInstruction (Out Remain) = def{oeSignal = True, selSignal = True}

instance Connected (Divider v x t) where
    data Ports (Divider v x t) = DividerPorts {sel, wr, oe :: SignalTag}
        deriving (Show)

instance IOConnected (Divider v x t) where
    data IOPorts (Divider v x t) = DividerIO
        deriving (Show)

instance (Val x, Show t) => TargetSystemComponent (Divider v x t) where
    moduleName _ _ = "pu_div"
    software _ _ = Empty
    hardware _tag Divider{mock} =
        Aggregate
            Nothing
            [ if mock
                then FromLibrary "div/div_mock.v"
                else FromLibrary "div/div.v"
            , FromLibrary "div/pu_div.v"
            ]
    hardwareInstance
        tag
        _pu@Divider{mock, pipeline}
        UnitEnv
            { sigClk
            , sigRst
            , valueIn = Just (dataIn, attrIn)
            , valueOut = Just (dataOut, attrOut)
            , ctrlPorts = Just DividerPorts{sel, wr, oe}
            } =
            [__i|
                pu_div \#
                        ( .DATA_WIDTH( #{ dataWidth (def :: x) } )
                        , .ATTR_WIDTH( #{ attrWidth (def :: x) } )
                        , .INVALID( 0 )
                        , .PIPELINE( #{ pipeline } )
                        , .SCALING_FACTOR_POWER( #{ fractionalBitSize (def :: x) } )
                        , .MOCK_DIV( #{ bool2verilog mock } )
                        ) #{ tag }
                    ( .clk( #{ sigClk } )
                    , .rst( #{ sigRst } )
                    , .signal_sel( #{ sel } )
                    , .signal_wr( #{ wr } )
                    , .data_in( #{ dataIn } )
                    , .attr_in( #{ attrIn } )
                    , .signal_oe( #{ oe } )
                    , .data_out( #{ dataOut } )
                    , .attr_out( #{ attrOut } )
                    );
            |]
    hardwareInstance _title _pu _env = error "internal error"

instance IOTestBench (Divider v x t) v x

instance VarValTime v x t => Testable (Divider v x t) v x where
    testBenchImplementation prj@Project{pName, pUnit} =
        Immediate (toString $ moduleName pName pUnit <> "_tb.v") $
            snippetTestBench
                prj
                SnippetTestBenchConf
                    { tbcSignals = ["sel", "wr", "oe"]
                    , tbcPorts =
                        DividerPorts
                            { sel = SignalTag "sel"
                            , wr = SignalTag "wr"
                            , oe = SignalTag "oe"
                            }
                    , tbcMC2verilogLiteral = \Microcode{selSignal, wrSignal, oeSignal} ->
                        [i|oe <= #{ bool2verilog oeSignal }; sel <= #{ bool2verilog selSignal }; wr <= #{ bool2verilog wrSignal }; |]
                    }
