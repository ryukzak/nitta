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
Module      : NITTA.Synthesis.ResolveDeadlock
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.ResolveDeadlock (
    ResolveDeadlockMetrics (..),
) where

import Data.Aeson (ToJSON)
import qualified Data.Set as S
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Refactor
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Model.Types
import NITTA.Synthesis.Types

data ResolveDeadlockMetrics = ResolveDeadlockMetrics
    { pNumberOfLockedVariables :: Float
    , pBufferCount :: Float
    , pNumberOfTransferableVariables :: Float
    }
    deriving (Generic)

instance ToJSON ResolveDeadlockMetrics

instance
    (UnitTag tag, VarValTime v x t) =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) tag v x t) tag v x t)
        (TargetSystem (BusNetwork tag v x t) tag v x t)
        (ResolveDeadlock v x)
        (ResolveDeadlock v x)
        ResolveDeadlockMetrics
    where
    decisions SynthesisState{sTarget} o = [(o, resolveDeadlockDecision sTarget o)]

    parameters SynthesisState{transferableVars} ResolveDeadlock{newBuffer} _ =
        let buffered = outputs newBuffer
         in ResolveDeadlockMetrics
                { pNumberOfLockedVariables = fromIntegral $ S.size buffered
                , pBufferCount = fromIntegral $ sum $ map countSuffix $ S.elems buffered
                , pNumberOfTransferableVariables = fromIntegral (S.size $ buffered `S.intersection` transferableVars)
                }

    estimate SynthesisState{sParent} _o d _ | 0 < decisionRepeats d sParent = -2
    estimate SynthesisState{} _o _d ResolveDeadlockMetrics{pNumberOfLockedVariables, pBufferCount, pNumberOfTransferableVariables} =
        1000 + pNumberOfLockedVariables - pBufferCount * 1000
            - 20 * pNumberOfTransferableVariables

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
