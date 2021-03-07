{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Broken
Description : Process Unit for negative tests
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Broken (
    Broken (..),
    Ports (..),
    IOPorts (..),
) where

import Control.Monad (when)
import Data.Default
import Data.List (find, (\\))
import Data.Set (elems, fromList, member)
import qualified NITTA.Intermediate.Functions as F
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Types
import NITTA.Project
import NITTA.Utils
import NITTA.Utils.ProcessDescription
import Numeric.Interval.NonEmpty (sup, (...))
import Text.InterpolatedString.Perl6 (qc)

data Broken v x t = Broken
    { remain :: [F v x]
    , targets :: [v]
    , sources :: [v]
    , doneAt :: Maybe t
    , currentWork :: Maybe (t, F v x)
    , currentWorkEndpoints :: [ProcessStepID]
    , process_ :: Process t (StepInfo v x t)
    , -- |generate verilog code with syntax error
      brokeVerilog :: Bool
    , -- |use process unit HW implementation with error
      wrongVerilogSimulationValue :: Bool
    , -- |wrong control sequence for data push (receiving data to PU)
      wrongControlOnPush :: Bool
    , -- |wrong control sequence for data pull (sending data from PU)
      wrongControlOnPull :: Bool
    , -- |lost target endpoint due synthesis
      lostEndpointTarget :: Bool
    , -- |lost source endpoint due synthesis
      lostEndpointSource :: Bool
    , wrongAttr :: Bool
    , unknownDataOut :: Bool
    }

deriving instance (VarValTime v x t) => Show (Broken v x t)

instance (Var v) => Locks (Broken v x t) v where
    locks Broken{remain, sources, targets} =
        [ Lock{lockBy, locked}
        | locked <- sources
        , lockBy <- targets
        ]
            ++ [ Lock{lockBy, locked}
               | locked <- concatMap (elems . variables) remain
               , lockBy <- sources ++ targets
               ]

instance BreakLoopProblem (Broken v x t) v x
instance OptimizeAccumProblem (Broken v x t) v x
instance ResolveDeadlockProblem (Broken v x t) v x

instance (VarValTime v x t) => ProcessorUnit (Broken v x t) v x t where
    tryBind f pu@Broken{remain}
        | Just F.BrokenBuffer{} <- castF f = Right pu{remain = f : remain}
        | otherwise = Left $ "The function is unsupported by Broken: " ++ show f
    process = process_

execution pu@Broken{targets = [], sources = [], remain, process_} f
    | Just (F.BrokenBuffer (I x) (O y)) <- castF f =
        pu
            { targets = [x]
            , sources = elems y
            , currentWork = Just (nextTick process_, f)
            , remain = remain \\ [f]
            }
execution _ _ = error "Broken: internal execution error."

instance (VarValTime v x t) => EndpointProblem (Broken v x t) v t where
    endpointOptions Broken{targets = [_], lostEndpointTarget = True} = []
    endpointOptions Broken{targets = [v], process_} =
        let start = nextTick process_ + 1 ... maxBound
            dur = 1 ... maxBound
         in [EndpointSt (Target v) $ TimeConstrain start dur]
    endpointOptions Broken{doneAt = Just _, lostEndpointSource = True} = []
    endpointOptions Broken{sources, doneAt = Just at, process_}
        | not $ null sources =
            let start = max at (nextTick process_) ... maxBound
                dur = 1 ... maxBound
             in [EndpointSt (Source $ fromList sources) $ TimeConstrain start dur]
    endpointOptions pu@Broken{remain, lostEndpointTarget = True}
        | not $ null remain = concatMap (endpointOptions . execution pu) $ tail remain
    endpointOptions pu@Broken{remain} = concatMap (endpointOptions . execution pu) remain

    endpointDecision pu@Broken{targets = [v], currentWorkEndpoints, wrongControlOnPush} d@EndpointSt{epRole = Target v', epAt}
        | v == v'
          , let (newEndpoints, process_') = runSchedule pu $ do
                    updateTick (sup epAt)
                    scheduleEndpoint d $ scheduleInstruction (shiftI (if wrongControlOnPush then 1 else 0) epAt) Load =
            pu
                { process_ = process_'
                , targets = []
                , currentWorkEndpoints = newEndpoints ++ currentWorkEndpoints
                , doneAt = Just $ sup epAt + 3
                }
    endpointDecision
        pu@Broken{targets = [], sources, doneAt, currentWork = Just (a, f), currentWorkEndpoints, wrongControlOnPull}
        d@EndpointSt{epRole = Source v, epAt}
            | not $ null sources
              , let sources' = sources \\ elems v
              , sources' /= sources
              , let (newEndpoints, process_') = runSchedule pu $ do
                        endpoints <- scheduleEndpoint d $ scheduleInstruction (shiftI (if wrongControlOnPull then 0 else -1) epAt) Out
                        when (null sources') $ do
                            high <- scheduleFunction (a ... sup epAt) f
                            let low = endpoints ++ currentWorkEndpoints
                            establishVerticalRelations high low
                        updateTick (sup epAt + 1)
                        return endpoints =
                pu
                    { process_ = process_'
                    , sources = sources'
                    , doneAt = if null sources' then Nothing else doneAt
                    , currentWork = if null sources' then Nothing else Just (a, f)
                    , currentWorkEndpoints = if null sources' then [] else newEndpoints ++ currentWorkEndpoints
                    }
    endpointDecision pu@Broken{targets = [], sources = [], remain} d
        | let v = oneOf $ variables d
          , Just f <- find (\f -> v `member` variables f) remain =
            endpointDecision (execution pu f) d
    endpointDecision pu d = error $ "Broken decision error\npu: " ++ show pu ++ ";\n decison:" ++ show d

instance Controllable (Broken v x t) where
    data Instruction (Broken v x t)
        = Load
        | Out
        deriving (Show)

    data Microcode (Broken v x t) = Microcode
        { wrSignal :: Bool
        , oeSignal :: Bool
        }
        deriving (Show, Eq, Ord)

    zipSignalTagsAndValues BrokenPorts{..} Microcode{..} =
        [ (wr, Bool wrSignal)
        , (oe, Bool oeSignal)
        ]

    usedPortTags BrokenPorts{wr, oe} = [wr, oe]

    takePortTags (wr : oe : _) _ = BrokenPorts wr oe
    takePortTags _ _ = error "can not take port tags, tags are over"

instance Default (Microcode (Broken v x t)) where
    def =
        Microcode
            { wrSignal = False
            , oeSignal = False
            }

instance (Time t) => Default (Broken v x t) where
    def =
        Broken
            { remain = []
            , targets = []
            , sources = []
            , doneAt = Nothing
            , currentWork = Nothing
            , currentWorkEndpoints = []
            , process_ = def
            , brokeVerilog = False
            , wrongVerilogSimulationValue = False
            , wrongControlOnPush = False
            , wrongControlOnPull = False
            , lostEndpointTarget = False
            , lostEndpointSource = False
            , wrongAttr = False
            , unknownDataOut = False
            }

instance Default x => DefaultX (Broken v x t) x

instance UnambiguouslyDecode (Broken v x t) where
    decodeInstruction Load = def{wrSignal = True}
    decodeInstruction Out = def{oeSignal = True}

instance Connected (Broken v x t) where
    data Ports (Broken v x t) = BrokenPorts
        { wr :: SignalTag
        , oe :: SignalTag
        }
        deriving (Show)

instance IOConnected (Broken v x t) where
    data IOPorts (Broken v x t) = BrokenIO
        deriving (Show)

instance (VarValTime v x t) => TargetSystemComponent (Broken v x t) where
    moduleName _title _pu = "pu_broken"
    software _ _ = Empty
    hardware tag pu =
        Aggregate
            Nothing
            [ FromLibrary $ moduleName tag pu <> ".v"
            ]

    hardwareInstance
        tag
        pu@Broken{brokeVerilog, wrongVerilogSimulationValue, wrongAttr, unknownDataOut}
        UnitEnv
            { sigClk
            , ctrlPorts = Just BrokenPorts{..}
            , valueIn = Just (dataIn, attrIn)
            , valueOut = Just (dataOut, attrOut)
            } =
            codeBlock
                [qc|
            {  moduleName tag pu } #
                    ( .DATA_WIDTH( { dataWidth (def :: x) } )
                    , .ATTR_WIDTH( { attrWidth (def :: x) } )
                    , .IS_BROKEN( { bool2verilog wrongVerilogSimulationValue } )
                    , .WRONG_ATTR( { bool2verilog wrongAttr } )
                    , .UNKNOWN_DATA_OUT( { bool2verilog unknownDataOut } )
                    ) { tag }
                ( .clk( { sigClk } )

                , .signal_wr( { wr } )
                , .data_in( { dataIn } ), .attr_in( { attrIn } )

                , .signal_oe( { oe } )
                , .data_out( { dataOut } ), .attr_out( { attrOut } )
                { if brokeVerilog then "WRONG VERILOG" else ""  }
                );
            |]
    hardwareInstance _title _pu _env = error "internal error"

instance IOTestBench (Broken v x t) v x

instance (Ord t) => WithFunctions (Broken v x t) (F v x) where
    functions Broken{process_, remain, currentWork} =
        functions process_
            ++ remain
            ++ case currentWork of
                Just (_, f) -> [f]
                Nothing -> []

instance (VarValTime v x t) => Testable (Broken v x t) v x where
    testBenchImplementation prj@Project{pName, pUnit} =
        Immediate (moduleName pName pUnit ++ "_tb.v") $
            snippetTestBench
                prj
                SnippetTestBenchConf
                    { tbcSignals = ["oe", "wr"]
                    , tbcPorts =
                        BrokenPorts
                            { oe = SignalTag "oe"
                            , wr = SignalTag "wr"
                            }
                    , tbcMC2verilogLiteral = \Microcode{oeSignal, wrSignal} ->
                        [qc|oe <= {bool2verilog oeSignal}; wr <= {bool2verilog wrSignal};|]
                    }
