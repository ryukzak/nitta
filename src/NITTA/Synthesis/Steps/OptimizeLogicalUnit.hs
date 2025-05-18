{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.Steps.OptimizeLogicalUnit
Description :
Copyright   : (c) Boris Novoselov, 2025
License     : BSD3
Maintainer  : borisnovoselov602@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Steps.OptimizeLogicalUnit (
    OptimizeLogicalUnitMetrics (..),
) where

import Data.Aeson (ToJSON)
import GHC.Generics
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Refactor
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.TargetSystem
import NITTA.Model.Time
import NITTA.Synthesis.Types

newtype OptimizeLogicalUnitMetrics = OptimizeLogicalUnitMetrics {mergedFuncNumber :: Int}
    deriving (Generic)

instance ToJSON OptimizeLogicalUnitMetrics

instance
    (UnitTag tag, VarValTime v x t) =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) tag v x t) tag v x t)
        (TargetSystem (BusNetwork tag v x t) tag v x t)
        (OptimizeLogicalUnit v x)
        (OptimizeLogicalUnit v x)
        OptimizeLogicalUnitMetrics
    where
    decisions SynthesisState{sTarget} o = [(o, optimizeLogicalUnitDecision sTarget o)]

    parameters SynthesisState{} OptimizeLogicalUnit{} _ = OptimizeLogicalUnitMetrics 0

    estimate _ctx _o _d OptimizeLogicalUnitMetrics{} = 6000
