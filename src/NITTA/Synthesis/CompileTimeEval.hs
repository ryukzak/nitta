
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.CompileTimeEval
Description :
Copyright   : (c) Daniil Prohorov, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.CompileTimeEval (
    CompileTimeEvalMetrics (..),
) where

import Data.Aeson (ToJSON)
import GHC.Generics
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Refactor
import NITTA.Model.TargetSystem
import NITTA.Model.Types
import NITTA.Synthesis.Types

data CompileTimeEvalMetrics = CompileTimeEvalMetrics
    deriving (Generic)

instance ToJSON CompileTimeEvalMetrics

instance
    (VarValTime v x t) =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) v x) tag v x t)
        (TargetSystem (BusNetwork tag v x t) v x)
        (CompileTimeEval v x)
        (CompileTimeEval v x)
        CompileTimeEvalMetrics
    where
    decisions SynthesisState{sTarget} o = [(o, compileTimeEvalDecision sTarget o)]

    parameters SynthesisState{} CompileTimeEval{} _ = CompileTimeEvalMetrics

    estimate _ctx _o _d CompileTimeEvalMetrics = 5050
