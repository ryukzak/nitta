{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Accum
Description : Accumulator processor unit implementation
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Accum (
    Accum,
    Ports (..),
    IOPorts (..),
) where

import Control.Monad (when)
import Data.Bifunctor
import Data.Default
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Data.String.Interpolate
import Data.String.ToString
import Data.Text qualified as T
import NITTA.Intermediate.Functions qualified as F
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Time
import NITTA.Project
import NITTA.Utils
import NITTA.Utils.ProcessDescription
import Numeric.Interval.NonEmpty (inf, singleton, sup, (...))
import Prettyprinter

{- | Type that contains expression:

@a + b = c@ is exression and it equals:
    @[[(False, "a"), (False, "b")], [(False, "c")]]@

@a + b = c; d - e = f@ is one expression too and it equals:
    @[[(False, "a"), (False, "b")], [(False, "c")], [(False, "d"), (True, "d")], [(False, "f")]]@
-}
data Job v x = Job
    { tasks :: [[(Bool, v)]]
    -- ^ Contains future parts expression to eval (c + d = e)
    , func :: F v x
    -- ^ Func of this expression
    , state :: JobState
    }

data JobState
    = Initialize
    | WaitArguments
    | Calculate
    | WaitResults
    | ArgumentAfterResult
    deriving (Show)

taskVars lst = S.fromList $ map snd lst

instance Var v => Show (Job v x) where
    show Job{tasks, func, state} =
        [i|Job{tasks=#{ show' tasks }, func=#{ func }, state=#{ state }}|]
        where
            show' = map (map (second toString))

data Accum v x t = Accum
    { remainJobs :: [Job v x]
    -- ^ List of jobs (expressions)
    , currentJob :: Maybe (Job v x)
    -- ^ Current job
    , process_ :: Process t (StepInfo v x t)
    -- ^ Process
    }

instance VarValTime v x t => Pretty (Accum v x t) where
    pretty a =
        [__i|
            Accum:
                remainJobs: #{ remainJobs a }
                currentJob: #{ currentJob a }
                #{ indent 4 $ pretty $ process_ a }
            |]

instance VarValTime v x t => Show (Accum v x t) where
    show = show . pretty

instance VarValTime v x t => Default (Accum v x t) where
    def =
        Accum
            { remainJobs = []
            , currentJob = Nothing
            , process_ = def
            }

instance Default x => DefaultX (Accum v x t) x

registerAcc f@F.Acc{actions} pu@Accum{remainJobs} =
    pu
        { remainJobs =
            Job
                { tasks = concat $ actionGroups actions
                , func = packF f
                , state = Initialize
                }
                : remainJobs
        }

actionGroups [] = []
actionGroups as =
    let (pushs, as') = span F.isPush as
        (pulls, as'') = span F.isPull as'
     in [ map
            ( \case
                (F.Push sign (I v)) -> (sign == F.Minus, v)
                _ -> error "actionGroups: internal error"
            )
            pushs
        , concatMap
            ( \case
                (F.Pull (O vs)) -> map (True,) $ S.elems vs
                _ -> error "actionGroups: internal error"
            )
            pulls
        ]
            : actionGroups as''

targetTask tasks
    | even $ length tasks = Just $ head tasks
    | otherwise = Nothing

sourceTask tasks
    | odd $ length tasks = Just $ head tasks
    | otherwise = Nothing

instance VarValTime v x t => ProcessorUnit (Accum v x t) v x t where
    tryBind f pu
        | Just (F.Add a b c) <- castF f =
            Right $ registerAcc (F.Acc [F.Push F.Plus a, F.Push F.Plus b, F.Pull c]) pu
        | Just (F.Sub a b c) <- castF f =
            Right $ registerAcc (F.Acc [F.Push F.Plus a, F.Push F.Minus b, F.Pull c]) pu
        | Just (F.Neg a b) <- castF f =
            Right $ registerAcc (F.Acc [F.Push F.Minus a, F.Pull b]) pu
        | Just f'@F.Acc{} <- castF f =
            Right $ registerAcc f' pu
        | otherwise = Left $ "The function is unsupported by Accum: " ++ show f

    process = process_

instance VarValTime v x t => EndpointProblem (Accum v x t) v t where
    endpointOptions pu@Accum{currentJob = Just Job{tasks, state}}
        | Just task <- targetTask tasks =
            let from = case state of
                    ArgumentAfterResult -> nextTick pu + 1
                    Initialize -> nextTick pu `withShift` 1
                    _ -> nextTick pu
             in map
                    (\v -> EndpointSt (Target v) $ TimeConstraint (from ... maxBound) (singleton 1))
                    $ S.elems
                    $ taskVars task
        | Just task <- sourceTask tasks =
            let from = case state of
                    Calculate -> nextTick pu + 2
                    WaitResults -> nextTick pu + 1
                    _ -> nextTick pu
             in [EndpointSt (Source $ taskVars task) $ TimeConstraint (from ... maxBound) (1 ... maxBound)]
    endpointOptions pu@Accum{remainJobs, currentJob = Nothing} =
        concatMap (\j -> endpointOptions pu{currentJob = Just j}) remainJobs
    endpointOptions pu = error [i|incorrect state for #{ pretty pu }|]

    endpointDecision pu@Accum{remainJobs, currentJob = Nothing} d
        | ([job], jobs') <- L.partition ((oneOf (variables d) `S.member`) . taskVars . head . tasks) remainJobs =
            endpointDecision
                pu
                    { remainJobs = jobs'
                    , currentJob = Just job
                    }
                d
    endpointDecision
        pu@Accum{currentJob = Just job@Job{tasks, state}}
        d@EndpointSt{epRole = Target v, epAt}
            | Just task <- targetTask tasks =
                let ((neg, _v), task') = case L.partition ((== v) . snd) task of
                        ([negAndVar], ts) -> (negAndVar, ts)
                        _ -> error "Accum: endpointDecision: internal error"
                    instr = case state of
                        Initialize -> ResetAndLoad neg
                        _ -> Load neg
                    process_' = execSchedule pu $ do
                        scheduleEndpoint d $ scheduleInstructionUnsafe epAt instr
                 in pu
                        { process_ = process_'
                        , currentJob = case (task', tail tasks) of
                            ([], []) -> Nothing
                            ([], tasks') -> Just job{tasks = tasks', state = Calculate}
                            (_task', tasks') -> Just job{tasks = task' : tasks', state = WaitArguments}
                        }
    endpointDecision
        pu@Accum{currentJob = Just job@Job{tasks, func}, process_}
        d@EndpointSt{epRole = Source vs, epAt}
            | Just task <- sourceTask tasks =
                let (_, task') = L.partition ((`S.member` vs) . snd) task
                    process_' = execSchedule pu $ do
                        endpoints <- scheduleEndpoint d $ scheduleInstructionUnsafe (epAt - 1) Out
                        when (null task' && length tasks == 1) $ do
                            let endpoints' = relatedEndpoints process_ $ variables func
                                a = inf $ stepsInterval endpoints'
                                low = endpoints ++ map pID endpoints'
                            high <- scheduleFunction (a ... sup epAt) func
                            establishVerticalRelations high low
                 in pu
                        { process_ = process_'
                        , currentJob = case (task', tail tasks) of
                            ([], []) -> Nothing
                            ([], tasks') -> Just job{tasks = tasks', state = ArgumentAfterResult}
                            (_task', tasks') -> Just job{tasks = task' : tasks', state = WaitResults}
                        }
    endpointDecision pu d = error [i|incorrect decision #{ d } for #{ pretty pu }|]

instance Connected (Accum v x t) where
    data Ports (Accum v x t) = AccumPorts {resetAcc, load, neg, oe :: SignalTag}
        deriving (Show)

instance IOConnected (Accum v x t) where
    data IOPorts (Accum v x t) = AccumIO deriving (Show)

instance Controllable (Accum v x t) where
    data Instruction (Accum v x t) = ResetAndLoad Bool | Load Bool | Out deriving (Show)

    data Microcode (Accum v x t) = Microcode
        { oeSignal :: Bool
        , resetAccSignal :: Bool
        , loadSignal :: Bool
        , negSignal :: Maybe Bool
        }
        deriving (Show, Eq, Ord)

    zipSignalTagsAndValues AccumPorts{..} Microcode{..} =
        [ (resetAcc, Bool resetAccSignal)
        , (load, Bool loadSignal)
        , (oe, Bool oeSignal)
        , (neg, maybe Undef Bool negSignal)
        ]

    usedPortTags AccumPorts{resetAcc, load, neg, oe} = [resetAcc, load, neg, oe]

    takePortTags (resetAcc : load : neg : oe : _) _ = AccumPorts resetAcc load neg oe
    takePortTags _ _ = error "can not take port tags, tags are over"

instance Default (Microcode (Accum v x t)) where
    def =
        Microcode
            { oeSignal = False
            , resetAccSignal = False
            , loadSignal = False
            , negSignal = Nothing
            }

instance UnambiguouslyDecode (Accum v x t) where
    decodeInstruction (ResetAndLoad neg) = def{resetAccSignal = True, loadSignal = True, negSignal = Just neg}
    decodeInstruction (Load neg) = def{resetAccSignal = False, loadSignal = True, negSignal = Just neg}
    decodeInstruction Out = def{oeSignal = True}

instance Var v => Locks (Accum v x t) v where
    locks Accum{currentJob = Nothing, remainJobs} = concatMap (locks . func) remainJobs
    locks Accum{currentJob = Just Job{tasks = []}} = error "Accum locks: internal error"
    locks Accum{currentJob = Just Job{tasks = t : ts}, remainJobs} =
        let current =
                [ Lock{locked, lockBy}
                | locked <- S.elems $ unionsMap taskVars ts
                , lockBy <- S.elems $ taskVars t
                ]
            remain =
                [ Lock{locked, lockBy}
                | locked <- S.elems $ unionsMap (variables . func) remainJobs
                , lockBy <- S.elems $ taskVars t
                ]
         in current ++ remain

instance VarValTime v x t => TargetSystemComponent (Accum v x t) where
    moduleName _ _ = "pu_accum"
    hardware _tag _pu = FromLibrary "pu_accum.v"
    software _ _ = Empty
    hardwareInstance
        tag
        _pu
        UnitEnv
            { sigClk
            , sigRst
            , ctrlPorts = Just AccumPorts{..}
            , valueIn = Just (dataIn, attrIn)
            , valueOut = Just (dataOut, attrOut)
            } =
            [__i|
                pu_accum \#
                        ( .DATA_WIDTH( #{ dataWidth (def :: x) } )
                        , .ATTR_WIDTH( #{ attrWidth (def :: x) } )
                        ) #{ tag }
                    ( .clk( #{ sigClk } )
                    , .rst( #{ sigRst } )
                    , .signal_resetAcc( #{ resetAcc } )
                    , .signal_load( #{ load } )
                    , .signal_neg( #{ neg } )
                    , .signal_oe( #{ oe } )
                    , .data_in( #{ dataIn } )
                    , .attr_in( #{ attrIn } )
                    , .data_out( #{ dataOut } )
                    , .attr_out( #{ attrOut } )
                    );
            |]
    hardwareInstance _title _pu _env = error "internal error"

instance Ord t => WithFunctions (Accum v x t) (F v x) where
    functions Accum{process_, remainJobs} =
        functions process_ ++ map func remainJobs

instance VarValTime v x t => Testable (Accum v x t) v x where
    testBenchImplementation prj@Project{pName, pUnit} =
        let tbcSignalsConst = ["resetAcc", "load", "oe", "neg"]

            showMicrocode Microcode{resetAccSignal, loadSignal, oeSignal, negSignal} =
                ([i|resetAcc <= #{ bool2verilog resetAccSignal };|] :: String)
                    <> [i| load <= #{ bool2verilog loadSignal };|]
                    <> [i| oe <= #{ bool2verilog oeSignal };|]
                    <> [i| neg <= #{ bool2verilog $ fromMaybe False negSignal };|]

            conf =
                SnippetTestBenchConf
                    { tbcSignals = tbcSignalsConst
                    , tbcPorts =
                        AccumPorts
                            { resetAcc = SignalTag "resetAcc"
                            , load = SignalTag "load"
                            , oe = SignalTag "oe"
                            , neg = SignalTag "neg"
                            }
                    , tbcMC2verilogLiteral = T.pack . showMicrocode
                    }
         in Immediate (toString $ moduleName pName pUnit <> "_tb.v") $ snippetTestBench prj conf

instance IOTestBench (Accum v x t) v x

instance BreakLoopProblem (Accum v x t) v x
instance ConstantFoldingProblem (Accum v x t) v x
instance OptimizeAccumProblem (Accum v x t) v x
instance OptimizeLutProblem (Accum v x t) v x
instance ResolveDeadlockProblem (Accum v x t) v x
