{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.BreakLoop
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.BreakLoop (
    BreakLoopMetrics (..),
) where

import Data.Aeson (ToJSON)
import GHC.Generics
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Refactor
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Model.Types
import NITTA.Synthesis.Types

data BreakLoopMetrics = BreakLoopMetrics
    deriving (Generic)

instance ToJSON BreakLoopMetrics

instance
    (UnitTag tag, VarValTime v x t) =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) v x) tag v x t)
        (TargetSystem (BusNetwork tag v x t) v x)
        (BreakLoop v x)
        (BreakLoop v x)
        BreakLoopMetrics
    where
    decisions SynthesisState{sTarget} o = [(o, breakLoopDecision sTarget o)]

    parameters SynthesisState{} BreakLoop{} _ = BreakLoopMetrics

    estimate _ctx _o _d BreakLoopMetrics = 5000
