{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.Dataflow
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Dataflow (
    DataflowMetrics (..),
) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Dataflow
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Model.Types
import NITTA.Synthesis.Types
import Numeric.Interval (Interval, inf, sup)

data DataflowMetrics = DataflowMetrics
    { pWaitTime :: Float
    , pRestrictedTime :: Bool
    , -- |number of variables, which is not transferable for affected
      -- functions.
      pNotTransferableInputs :: [Float]
    }
    deriving (Generic)

instance
    (UnitTag tag, VarValTime v x t) =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) v x) tag v x t)
        (TargetSystem (BusNetwork tag v x t) v x)
        (DataflowSt tag v (TimeConstrain t))
        (DataflowSt tag v (Interval t))
        DataflowMetrics
    where
    decisions SynthesisState{sTarget} o = let d = dataflowOption2decision o in [(d, dataflowDecision sTarget d)]

    parameters
        SynthesisState
            { transferableVars
            , sTarget
            }
        (DataflowSt (_, TimeConstrain{tcAvailable, tcDuration}) target)
        _ =
            DataflowMetrics
                { pWaitTime = fromIntegral (inf tcAvailable)
                , pRestrictedTime = fromEnum (sup tcDuration) /= maxBound
                , pNotTransferableInputs =
                    let fs = functions $ mUnit sTarget
                        vs = S.fromList [v | (v, Just _) <- M.assocs target]
                        affectedFunctions = filter (\f -> not $ null (inputs f `S.intersection` vs)) fs
                        notTransferableVars = map (\f -> inputs f S.\\ transferableVars) affectedFunctions
                     in map (fromIntegral . length) notTransferableVars
                }

    estimate SynthesisState{numberOfDataflowOptions} _o _d DataflowMetrics{pWaitTime, pNotTransferableInputs, pRestrictedTime} =
        2000
            + (numberOfDataflowOptions >= threshold) <?> 1000
            + pRestrictedTime <?> 200
            - sum pNotTransferableInputs * 5
            - pWaitTime

threshold = 20
