{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.Steps.OptimizeLut
Description :
Copyright   : (c) Boris Novoselov, 2025
License     : BSD3
Maintainer  : borisnovoselov602@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Steps.OptimizeLut (
    OptimizeLutMetrics (..),
) where

import Data.Aeson (ToJSON)
import GHC.Generics
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Refactor
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.TargetSystem
import NITTA.Model.Time
import NITTA.Synthesis.Types

data OptimizeLutMetrics = OptimizeLutMetrics
    deriving (Generic)

instance ToJSON OptimizeLutMetrics

instance
    (UnitTag tag, VarValTime v x t) =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) tag v x t) tag v x t)
        (TargetSystem (BusNetwork tag v x t) tag v x t)
        (OptimizeLut v x)
        (OptimizeLut v x)
        OptimizeLutMetrics
    where
    decisions SynthesisState{sTarget} o = [(o, optimizeLutDecision sTarget o)]

    parameters SynthesisState{} OptimizeLut{} _ = OptimizeLutMetrics

    estimate _ctx _o _d OptimizeLutMetrics = 6000
