{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.Steps.ConstantFolding
Description :
Copyright   : (c) Daniil Prohorov, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Steps.ConstantFolding (
    ConstantFoldingMetrics (..),
) where

import Data.Aeson (ToJSON)
import GHC.Generics
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Refactor
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.TargetSystem
import NITTA.Model.Time
import NITTA.Synthesis.Types

data ConstantFoldingMetrics = ConstantFoldingMetrics
    deriving (Generic)

instance ToJSON ConstantFoldingMetrics

instance
    (UnitTag tag, VarValTime v x t) =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) tag v x t) tag v x t)
        (TargetSystem (BusNetwork tag v x t) tag v x t)
        (ConstantFolding v x)
        (ConstantFolding v x)
        ConstantFoldingMetrics
    where
    decisions SynthesisState{sTarget} o = [(o, constantFoldingDecision sTarget o)]

    parameters SynthesisState{} ConstantFolding{} _ = ConstantFoldingMetrics

    estimate _ctx _o _d ConstantFoldingMetrics = 5050
