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
    RefactorMetrics (..),
    RefactorType (..),
) where

import Data.Aeson (ToJSON)
import Data.Default
import qualified Data.Set as S
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Refactor
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Model.Types
import NITTA.Synthesis.Types

-- TODO: split to separated constructor or separated refactor type?
data RefactorMetrics = RefactorMetrics
    { pRefactorType :: RefactorType
    , pNumberOfLockedVariables :: Float
    , pBufferCount :: Float
    , pNStepBackRepeated :: Int
    , pNumberOfTransferableVariables :: Float
    }
    deriving (Generic)

instance ToJSON RefactorMetrics

data RefactorType = ResolveDeadlockT | BreakLoopT | OptimizeAccumT deriving (Generic)

instance ToJSON RefactorType

instance
    (UnitTag tag, VarValTime v x t) =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) v x) tag v x t)
        (TargetSystem (BusNetwork tag v x t) v x)
        (Refactor v x)
        (Refactor v x)
        RefactorMetrics
    where
    decisions SynthesisState{sTarget} o = [(o, refactorDecision sTarget o)]

    parameters SynthesisState{} BreakLoop{} _ =
        RefactorMetrics
            { pRefactorType = BreakLoopT
            , pNumberOfLockedVariables = 0
            , pBufferCount = 0
            , pNStepBackRepeated = def
            , pNumberOfTransferableVariables = 0
            }
    parameters SynthesisState{transferableVars} (ResolveDeadlock vs) _ =
        RefactorMetrics
            { pRefactorType = ResolveDeadlockT
            , pNumberOfLockedVariables = fromIntegral $ S.size vs
            , pBufferCount = fromIntegral $ sum $ map countSuffix $ S.elems vs
            , pNStepBackRepeated = def
            , pNumberOfTransferableVariables = fromIntegral (S.size $ vs `S.intersection` transferableVars)
            }
    -- FIXME: we can better
    parameters SynthesisState{} OptimizeAccum{} _ =
        RefactorMetrics
            { pRefactorType = OptimizeAccumT
            , pNumberOfLockedVariables = def
            , pBufferCount = def
            , pNStepBackRepeated = def
            , pNumberOfTransferableVariables = def
            }

    estimate SynthesisState{sParent} _o d RefactorMetrics{}
        | 0 < decisionRepeats d sParent = -2
    estimate _ctx _o _d RefactorMetrics{pRefactorType = BreakLoopT} =
        5000
    estimate SynthesisState{} _o _d RefactorMetrics{pRefactorType = ResolveDeadlockT, pNumberOfLockedVariables, pBufferCount, pNumberOfTransferableVariables} =
        1000 + pNumberOfLockedVariables - pBufferCount * 1000
            - 20 * pNumberOfTransferableVariables -- + trace (show (decisionRepeats d sParent) <> "  " <> show d <> "   " <> show (toRootDecisionStrings sParent)) 0
    estimate _ctx _o _d RefactorMetrics{pRefactorType = OptimizeAccumT} =
        5000

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
