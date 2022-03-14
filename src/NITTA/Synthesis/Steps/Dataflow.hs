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
module NITTA.Synthesis.Steps.Dataflow (
    DataflowMetrics (..),
) where

import Data.Aeson (ToJSON)
import qualified Data.Set as S
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Dataflow
import NITTA.Model.Problems.Endpoint
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Synthesis.Types
import NITTA.Utils
import Numeric.Interval.NonEmpty (Interval, inf, sup)

data DataflowMetrics = DataflowMetrics
    { pWaitTime :: Float
    , pRestrictedTime :: Bool
    , -- |number of variables, which is not transferable for affected
      -- functions.
      pNotTransferableInputs :: [Float]
    }
    deriving (Generic)

instance ToJSON DataflowMetrics

instance
    (UnitTag tag, VarValTime v x t) =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) tag v x t) tag v x t)
        (TargetSystem (BusNetwork tag v x t) tag v x t)
        (DataflowSt tag v (TimeConstraint t))
        (DataflowSt tag v (Interval t))
        DataflowMetrics
    where
    decisions SynthesisState{sTarget} o = let d = dataflowOption2decision o in [(d, dataflowDecision sTarget d)]

    parameters SynthesisState{transferableVars, sTarget} DataflowSt{dfSource, dfTargets} _ =
        let TimeConstraint{tcAvailable, tcDuration} = epAt $ snd dfSource
         in DataflowMetrics
                { pWaitTime = fromIntegral (inf tcAvailable)
                , pRestrictedTime = fromEnum (sup tcDuration) /= maxBound
                , pNotTransferableInputs =
                    let fs = functions $ mUnit sTarget
                        vs = unionsMap (variables . snd) dfTargets
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
