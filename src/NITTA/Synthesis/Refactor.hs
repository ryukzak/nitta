{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.Refactor
Description : Synthesis tree representation
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Refactor (
    BreakLoopMetrics (..),
    OptimizeAccumMetrics (..),
    ResolveDeadlockMetrics (..),
    isRefactor,
) where

import Data.Aeson (ToJSON)
import Data.Default
import Data.Maybe
import qualified Data.Set as S
import Data.Typeable
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Refactor
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Model.Types
import NITTA.Synthesis.Types

data OptimizeAccumMetrics = OptimizeAccumMetrics
    deriving (Generic)

instance ToJSON OptimizeAccumMetrics

instance
    (VarValTime v x t) =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) v x) tag v x t)
        (TargetSystem (BusNetwork tag v x t) v x)
        (OptimizeAccum v x)
        (OptimizeAccum v x)
        OptimizeAccumMetrics
    where
    decisions SynthesisState{sTarget} o = [(o, optimizeAccumDecision sTarget o)]

    parameters SynthesisState{} OptimizeAccum{} _ = OptimizeAccumMetrics

    estimate _ctx _o _d OptimizeAccumMetrics = 5000

data ResolveDeadlockMetrics = ResolveDeadlockMetrics
    { pNumberOfLockedVariables :: Float
    , pBufferCount :: Float
    , pNStepBackRepeated :: Int
    , pNumberOfTransferableVariables :: Float
    }
    deriving (Generic)

instance ToJSON ResolveDeadlockMetrics

instance
    (UnitTag tag, VarValTime v x t) =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) v x) tag v x t)
        (TargetSystem (BusNetwork tag v x t) v x)
        (ResolveDeadlock v x)
        (ResolveDeadlock v x)
        ResolveDeadlockMetrics
    where
    decisions SynthesisState{sTarget} o = [(o, resolveDeadlockDecision sTarget o)]

    parameters SynthesisState{transferableVars} ResolveDeadlock{buffer} _ =
        let buffered = outputs buffer
         in ResolveDeadlockMetrics
                { pNumberOfLockedVariables = fromIntegral $ S.size buffered
                , pBufferCount = fromIntegral $ sum $ map countSuffix $ S.elems buffered
                , pNStepBackRepeated = def -- FIXME:
                , pNumberOfTransferableVariables = fromIntegral (S.size $ buffered `S.intersection` transferableVars)
                }

    estimate SynthesisState{sParent} _o d _ | 0 < decisionRepeats d sParent = -2
    estimate SynthesisState{} _o _d ResolveDeadlockMetrics{pNumberOfLockedVariables, pBufferCount, pNumberOfTransferableVariables} =
        1000 + pNumberOfLockedVariables - pBufferCount * 1000
            - 20 * pNumberOfTransferableVariables -- + trace (show (decisionRepeats d sParent) <> "  " <> show d <> "   " <> show (toRootDecisionStrings sParent)) 0

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

decisionRepeats d parent =
    let ds = toRootDecisionStrings parent
     in length $ takeWhile (== show d) ds

toRootDecisionStrings parent =
    map
        ( ( \case
                SynthesisDecision{decision} -> show decision
                _ -> ""
          )
            . sDecision
        )
        $ toRoot parent

toRoot (Just tree@Tree{sState = SynthesisState{sParent}}) = tree : toRoot sParent
toRoot _ = []

isRefactor SynthesisDecision{metrics}
    | isJust (cast metrics :: Maybe BreakLoopMetrics) = True
    | isJust (cast metrics :: Maybe OptimizeAccumMetrics) = True
    | isJust (cast metrics :: Maybe ResolveDeadlockMetrics) = True
isRefactor _ = False
