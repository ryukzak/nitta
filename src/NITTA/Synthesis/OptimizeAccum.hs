{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.OptimizeAccum
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.OptimizeAccum (
    OptimizeAccumMetrics (..),
) where

import Data.Aeson (ToJSON)
import GHC.Generics
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Refactor
import NITTA.Model.TargetSystem
import NITTA.Model.Types
import NITTA.Synthesis.Types

data OptimizeAccumMetrics = OptimizeAccumMetrics
    deriving (Generic)

instance ToJSON OptimizeAccumMetrics

instance
    (VarValTime v x t) =>
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
