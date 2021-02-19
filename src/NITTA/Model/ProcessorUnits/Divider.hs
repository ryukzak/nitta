{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Divider
Description : Integral divider processor unit with pipeline
Copyright   : (c) Aleksandr Penskoi, 2019
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

import Control.Monad (void, when)
import Data.Default
import Data.List (partition, sortBy)
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (Set, member)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified NITTA.Intermediate.Functions as F
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Types
import NITTA.Project
import NITTA.Utils
import NITTA.Utils.ProcessDescription
import Numeric.Interval.NonEmpty (Interval, inf, intersection, singleton, sup, (...))
import Text.InterpolatedString.Perl6 (qc)

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
    , targetIntervals :: [Interval t]
    , sourceIntervals :: [Interval t]
    , process_ :: Process t (StepInfo v x t)
    , latency :: t
    , pipeline :: t
    , mock :: Bool
    }

divider pipeline mock =
    Divider
        { jobs = []
        , remains = []
        , targetIntervals = []
        , sourceIntervals = []
        , process_ = def
        , latency = 1
        , pipeline
        , mock
        }

instance (Time t) => Default (Divider v x t) where
    def = divider 4 True

instance Default x => DefaultX (Divider v x t) x

instance (Ord t) => WithFunctions (Divider v x t) (F v x) where
    functions Divider{process_, remains, jobs} =
        functions process_
            ++ remains
            ++ map function jobs

data Job v x t
    = Input
        { function :: F v x
        , startAt :: t
        , inputSeq :: [(InputDesc, v)]
        }
    | InProgress
        { function :: F v x
        , startAt :: t
        , finishAt :: t
        }
    | Output
        { function :: F v x
        , startAt :: t
        , rottenAt :: Maybe t
        , finishAt :: t
        , outputRnd :: [(OutputDesc, Set v)]
        }
    deriving (Eq, Show)

nextTargetTick Divider{targetIntervals = []} = 0
nextTargetTick Divider{targetIntervals = i : _} = sup i + 1

nextSourceTick Divider{sourceIntervals = []} = 0
nextSourceTick Divider{sourceIntervals = i : _} = sup i + 1

findJob f jobs =
    case partition f jobs of
        ([i], other) -> Just (i, other)
        ([], _) -> Nothing
        _ -> error "findInput internal error"

findInput = findJob (\case Input{} -> True; _ -> False)
findOutput = findJob (\case Output{} -> True; _ -> False)

findNextInProgress jobs
    | let (inProgress, other) = partition (\case InProgress{} -> True; _ -> False) jobs
      , let inProgress' =
                sortBy
                    ( \InProgress{finishAt = a} InProgress{finishAt = b} ->
                        b `compare` a
                    )
                    inProgress =
        case inProgress' of
            [] -> Nothing
            (j : js) -> Just (j, js ++ other)

remain2input nextTick f
    | Just (F.Division (I n) (I d) (O _q) (O _r)) <- castF f =
        Input{function = f, startAt = nextTick, inputSeq = [(Numer, n), (Denom, d)]}
remain2input _ _ = error "divider inProgress2Output internal error"

inProgress2Output rottenAt InProgress{function, startAt, finishAt}
    | Just (F.Division _ _ (O q) (O r)) <- castF function =
        Output{function, rottenAt, startAt, finishAt, outputRnd = filter (not . null . snd) [(Quotient, q), (Remain, r)]}
inProgress2Output _ _ = error "divider inProgress2Output internal error"

resolveColisions [] opt = [opt]
resolveColisions intervals opt@EndpointSt{epAt = tc@TimeConstrain{tcAvailable}}
    | all (isNothing . intersection tcAvailable) intervals =
        [opt]
    | otherwise -- FIXME: we must prick out work point from intervals
      , let from = maximum $ map sup intervals =
        [opt{epAt = tc{tcAvailable = from ... inf tcAvailable}}]

rottenTime Divider{pipeline, latency} jobs
    | Just (InProgress{startAt}, _) <- findNextInProgress jobs =
        Just (startAt + pipeline + latency)
    | Just (Input{startAt}, _) <- findOutput jobs =
        Just (startAt + pipeline + latency)
    | otherwise = Nothing

pushOutput pu@Divider{jobs}
    | Just _ <- findOutput jobs = pu
    | Just (ij, other) <- findNextInProgress jobs =
        pu{jobs = inProgress2Output (rottenTime pu other) ij : other}
    | otherwise = pu

instance (VarValTime v x t) => ProcessorUnit (Divider v x t) v x t where
    tryBind f pu@Divider{remains}
        | Just (F.Division (I _n) (I _d) (O _q) (O _r)) <- castF f =
            Right
                pu
                    { remains = f : remains
                    }
        | otherwise = Left $ "Unknown functional block: " ++ show f
    process = process_

instance (Var v) => Locks (Divider v x t) v where
    -- FIXME:
    locks _ = []

instance BreakLoopProblem (Divider v x t) v x
instance OptimizeAccumProblem (Divider v x t) v x
instance ResolveDeadlockProblem (Divider v x t) v x

instance (VarValTime v x t) => EndpointProblem (Divider v x t) v t where
    endpointOptions pu@Divider{targetIntervals, sourceIntervals, remains, jobs} =
        concatMap (resolveColisions sourceIntervals) targets
            ++ concatMap (resolveColisions targetIntervals) sources
        where
            target v =
                EndpointSt
                    (Target v)
                    $ TimeConstrain (nextTargetTick pu ... maxBound) (singleton 1)
            targets
                | Just (Input{inputSeq = (_tag, v) : _}, _) <- findInput jobs =
                    [target v]
                | otherwise = map (target . snd . head . inputSeq . remain2input nextTick) remains

            source Output{outputRnd, rottenAt, finishAt} =
                map
                    ( \(_tag, vs) ->
                        EndpointSt
                            (Source vs)
                            $ TimeConstrain
                                (max finishAt (nextSourceTick pu) ... fromMaybe maxBound rottenAt)
                                (singleton 1)
                    )
                    outputRnd
            source _ = error "Divider internal error: source."

            sources
                | Just (out, _) <- findOutput jobs = source out
                | Just (ij, other) <- findNextInProgress jobs =
                    source $ inProgress2Output (rottenTime pu other) ij
                | otherwise = []

    -- FIXME: vertical relations
    endpointDecision
        pu@Divider{jobs, targetIntervals, remains, pipeline, latency}
        d@EndpointSt{epRole = Target v, epAt}
            | ([f], fs) <- partition (\f -> v `member` variables f) remains =
                endpointDecision
                    pu
                        { remains = fs
                        , jobs = remain2input (sup epAt) f : jobs
                        }
                    d
            | Just (i@Input{inputSeq = ((tag, nextV) : vs), function, startAt}, other) <- findInput jobs
              , v == nextV
              , let finishAt = sup epAt + pipeline + latency =
                pushOutput
                    pu
                        { targetIntervals = epAt : targetIntervals
                        , jobs =
                            if null vs
                                then InProgress{function, startAt, finishAt} : other
                                else i{inputSeq = vs} : other
                        , process_ = execSchedule pu $ do
                            _endpoints <- scheduleEndpoint d $ scheduleInstruction epAt $ Load tag
                            updateTick (sup epAt)
                        }
    endpointDecision
        pu@Divider{jobs, sourceIntervals}
        d@EndpointSt{epRole = Source vs, epAt}
            | Just (out@Output{outputRnd, startAt, function}, other) <- findOutput jobs
              , (vss, [(tag, vs')]) <- partition (\(_tag, vs') -> null (vs `S.intersection` vs')) outputRnd
              , let vss' =
                        let tmp = vs' `S.difference` vs
                         in if S.null tmp
                                then vss
                                else (tag, tmp) : vss =
                pushOutput
                    pu
                        { sourceIntervals = epAt : sourceIntervals
                        , jobs =
                            if null vss'
                                then other
                                else out{outputRnd = vss'} : other
                        , process_ = execSchedule pu $ do
                            _endpoints <- scheduleEndpoint d $ scheduleInstruction epAt $ Out tag
                            when (null vss') $ void $ scheduleFunction (startAt ... sup epAt) function
                            updateTick (sup epAt)
                        }
    endpointDecision _ _ = error "divider decision internal error"

instance Controllable (Divider v x t) where
    data Instruction (Divider v x t)
        = Load InputDesc
        | Out OutputDesc
        deriving (Show)

    data Microcode (Divider v x t) = Microcode
        { wrSignal :: Bool
        , wrSelSignal :: Bool
        , oeSignal :: Bool
        , oeSelSignal :: Bool
        }
        deriving (Show, Eq, Ord)

    zipSignalTagsAndValues DividerPorts{..} Microcode{..} =
        [ (wr, Bool wrSignal)
        , (wrSel, Bool wrSelSignal)
        , (oe, Bool oeSignal)
        , (oeSel, Bool oeSelSignal)
        ]

    usedPortTags DividerPorts{wr, wrSel, oe, oeSel} = [wr, wrSel, oe, oeSel]

    takePortTags (wr : wrSel : oe : oeSel : _) _ = DividerPorts wr wrSel oe oeSel
    takePortTags _ _ = error "can not take port tags, tags are over"

instance Default (Microcode (Divider v x t)) where
    def =
        Microcode
            { wrSignal = False
            , wrSelSignal = False
            , oeSignal = False
            , oeSelSignal = False
            }
instance UnambiguouslyDecode (Divider v x t) where
    decodeInstruction (Load Numer) = def{wrSignal = True, wrSelSignal = False}
    decodeInstruction (Load Denom) = def{wrSignal = True, wrSelSignal = True}
    decodeInstruction (Out Quotient) = def{oeSignal = True, oeSelSignal = False}
    decodeInstruction (Out Remain) = def{oeSignal = True, oeSelSignal = True}

instance Connected (Divider v x t) where
    data Ports (Divider v x t) = DividerPorts {wr, wrSel, oe, oeSel :: SignalTag}
        deriving (Show)

instance IOConnected (Divider v x t) where
    data IOPorts (Divider v x t) = DividerIO
        deriving (Show)

instance (Val x, Show t) => TargetSystemComponent (Divider v x t) where
    moduleName _ _ = "pu_div"
    software _ _ = Empty
    hardware tag pu@Divider{mock} =
        Aggregate
            Nothing
            [ if mock
                then FromLibrary "div/div_mock.v"
                else FromLibrary "div/div.v"
            , FromLibrary $ "div/" <> moduleName tag pu <> ".v"
            ]
    hardwareInstance
        tag
        _pu@Divider{mock, pipeline}
        UnitEnv
            { sigClk
            , sigRst
            , valueIn = Just (dataIn, attrIn)
            , valueOut = Just (dataOut, attrOut)
            , ctrlPorts = Just DividerPorts{oe, oeSel, wr, wrSel}
            } =
            codeBlock
                [qc|
            pu_div #
                    ( .DATA_WIDTH( { dataWidth (def :: x) } )
                    , .ATTR_WIDTH( { attrWidth (def :: x) } )
                    , .INVALID( 0 )
                    , .PIPELINE( { pipeline } )
                    , .SCALING_FACTOR_POWER( { fractionalBitSize (def :: x) } )
                    , .MOCK_DIV( { bool2verilog mock } )
                    ) { tag }
                ( .clk( { sigClk } )
                , .rst( { sigRst } )
                , .signal_wr( { wr } )
                , .signal_wr_sel( { wrSel } )
                , .data_in( { dataIn } )
                , .attr_in( { attrIn } )
                , .signal_oe( { oe } )
                , .signal_oe_sel( { oeSel } )
                , .data_out( { dataOut } )
                , .attr_out( { attrOut } )
                );
            |]
    hardwareInstance _title _pu _env = error "internal error"

instance IOTestBench (Divider v x t) v x

instance (VarValTime v x t) => Testable (Divider v x t) v x where
    testBenchImplementation prj@Project{pName, pUnit} =
        Immediate (moduleName pName pUnit <> "_tb.v") $
            snippetTestBench
                prj
                SnippetTestBenchConf
                    { tbcSignals = ["oe", "oeSel", "wr", "wrSel"]
                    , tbcPorts =
                        DividerPorts
                            { oe = SignalTag "oe"
                            , oeSel = SignalTag "oeSel"
                            , wr = SignalTag "wr"
                            , wrSel = SignalTag "wrSel"
                            }
                    , tbcMC2verilogLiteral = \Microcode{oeSignal, oeSelSignal, wrSignal, wrSelSignal} ->
                        [qc|oe <= {bool2verilog oeSignal}; oeSel <= {bool2verilog oeSelSignal}; wr <= {bool2verilog wrSignal}; wrSel <= {bool2verilog wrSelSignal};|]
                    }
