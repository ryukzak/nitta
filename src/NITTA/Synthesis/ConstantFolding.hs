{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.ConstantFolding
Description :
Copyright   : (c) Daniil Prohorov, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.ConstantFolding (
    ConstantFoldingMetrics (..),
) where

import Data.Aeson (ToJSON)
import GHC.Generics
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Refactor
import NITTA.Model.TargetSystem
import NITTA.Model.Time
import NITTA.Synthesis.Types

data ConstantFoldingMetrics = ConstantFoldingMetrics
    deriving (Generic)

instance ToJSON ConstantFoldingMetrics

instance
    (VarValTime v x t) =>
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
