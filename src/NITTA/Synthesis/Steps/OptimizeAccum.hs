{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.Steps.OptimizeAccum
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Steps.OptimizeAccum (
    OptimizeAccumMetrics (..),
) where

import Data.Aeson (ToJSON)
import GHC.Generics
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Refactor
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.TargetSystem
import NITTA.Model.Time
import NITTA.Synthesis.Types

data OptimizeAccumMetrics = OptimizeAccumMetrics
    deriving (Generic)

instance ToJSON OptimizeAccumMetrics

instance
    (UnitTag tag, VarValTime v x t) =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) tag v x t) tag v x t)
        (TargetSystem (BusNetwork tag v x t) tag v x t)
        (OptimizeAccum v x)
        (OptimizeAccum v x)
        OptimizeAccumMetrics
    where
    decisions SynthesisState{sTarget} o = [(o, optimizeAccumDecision sTarget o)]

    parameters SynthesisState{} OptimizeAccum{} _ = OptimizeAccumMetrics

    estimate _ctx _o _d OptimizeAccumMetrics = 5000
