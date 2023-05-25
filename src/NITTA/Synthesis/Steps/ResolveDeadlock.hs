{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.Steps.ResolveDeadlock
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Steps.ResolveDeadlock (
    ResolveDeadlockMetrics (..),
) where

import Data.Aeson (ToJSON)
import Data.Set qualified as S
import GHC.Generics
import Data.Map.Strict qualified as M
import Control.Concurrent.STM.TVar
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Refactor
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
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
        1000
            + pNumberOfLockedVariables
            - pBufferCount * 1000
            - 20 * pNumberOfTransferableVariables

decisionRepeats d parent =
    let ds = toRootDecisionStrings parent
     in length $ takeWhile (== show d) ds

toRootDecisionStrings tree sid = do
    nodePath <- toRoot tree sid
    return $ map ((\case
          SynthesisDecision {decision} -> show decision
          _ -> "") . sDecision) nodePath

toRoot :: TVar (M.Map Sid (Node m tag v x t)) -> Maybe Sid -> IO [Node m tag v x t]
toRoot _ Nothing = return []
toRoot tvar (Just sid) = do
    map <- readTVarIO tvar
    case M.lookup sid map of
        Nothing -> return []
        Just node -> do
            let SynthesisState{sParent} = sState node
            (node:) <$> toRoot tvar sParent

-- getNodeFromTVar :: TVar (M.Map Sid (Node m tag v x t)) -> Sid -> IO (Maybe (Node m tag v x t))
-- getNodeFromTVar tvar sid = atomically $ do
--     nodeMap <- readTVar tvar
--     return $ M.lookup sid nodeMap
