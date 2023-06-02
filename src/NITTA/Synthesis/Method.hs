{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.Synthesis.Method
Description : Synthesis method implementation.
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Method (
    DefTree,
    SynthesisMethod,
    simpleSynthesisIO,
    smartBindSynthesisIO,
    obviousBindThreadIO,
    topDownScoreSynthesisIO,
    allBestThreadIO,
    stateOfTheArtSynthesisIO,
    allBindsAndRefsIO,
    bestStepIO,
    SynthesisMethodConstraints,
) where

import Data.Aeson (ToJSON)
import Data.Heap qualified as H
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Typeable
import Debug.Trace
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Synthesis.Analysis
import NITTA.Synthesis.Explore
import NITTA.Synthesis.Steps
import NITTA.Synthesis.Types
import NITTA.UIBackend.Types
import NITTA.UIBackend.ViewHelper
import NITTA.Utils (maximumOn, minimumOn)
import Safe
import System.Log.Logger
import Text.Printf (printf)

{- | The constant, which restricts the maximum number of synthesis steps. Avoids
the endless synthesis process.
-}
stepLimit = 750 :: Int

-- | The most complex synthesis method, which embedded all another. That all.
stateOfTheArtSynthesisIO :: (SynthesisMethodConstraints tag v x t) => BackendCtx tag v x t -> SynthesisMethod tag v x t
stateOfTheArtSynthesisIO ctx tree = do
    infoM "NITTA.Synthesis" $ "stateOfTheArtSynthesisIO: " <> show (sID tree)
    l1 <- simpleSynthesisIO ctx tree
    l2 <- smartBindSynthesisIO ctx tree
    l3 <- bestThreadIO ctx stepLimit tree
    l4 <- bestThreadIO ctx stepLimit =<< allBindsAndRefsIO ctx tree
    return $ bestLeaf tree [l1, l2, l3, l4]

-- | Schedule process by simple synthesis.
simpleSynthesisIO :: (SynthesisMethodConstraints tag v x t) => BackendCtx tag v x t -> SynthesisMethod tag v x t
simpleSynthesisIO ctx root = do
    infoM "NITTA.Synthesis" $ "simpleSynthesisIO: " <> show (sID root)
    lastObliviousNode <- obviousBindThreadIO ctx root
    allBestThreadIO ctx 1 lastObliviousNode

smartBindSynthesisIO :: (SynthesisMethodConstraints tag v x t) => BackendCtx tag v x t -> SynthesisMethod tag v x t
smartBindSynthesisIO ctx tree = do
    infoM "NITTA.Synthesis" $ "smartBindSynthesisIO: " <> show (sID tree)
    tree' <- smartBindThreadIO ctx tree
    allBestThreadIO ctx 1 tree'

bestThreadIO :: (SynthesisMethodConstraints tag v x t) => BackendCtx tag v x t -> Int -> SynthesisMethod tag v x t
bestThreadIO _ 0 node = return $ trace "bestThreadIO've reached the step limit!" node
bestThreadIO ctx limit tree = do
    subForest <- positiveSubForestIO ctx tree
    case subForest of
        [] -> return tree
        _ -> bestThreadIO ctx (limit - 1) $ maximumOn (defScore . sDecision) subForest

bestStepIO :: (SynthesisMethodConstraints tag v x t) => BackendCtx tag v x t -> SynthesisMethod tag v x t
bestStepIO ctx tree = do
    subForest <- positiveSubForestIO ctx tree
    case subForest of
        [] -> error "all step is over"
        _ -> return $ maximumOn (defScore . sDecision) subForest

obviousBindThreadIO :: (SynthesisMethodConstraints tag v x t) => BackendCtx tag v x t -> SynthesisMethod tag v x t
obviousBindThreadIO ctx tree = do
    subForest <- positiveSubForestIO ctx tree
    maybe (return tree) (obviousBindThreadIO ctx) $
        L.find
            ( ( \case
                    Just BindMetrics{pPossibleDeadlock = True} -> False
                    Just BindMetrics{pAlternative = 1} -> True
                    _ -> False
              )
                . cast
                . sDecision
            )
            subForest

allBindsAndRefsIO :: (SynthesisMethodConstraints tag v x t) => BackendCtx tag v x t -> SynthesisMethod tag v x t
allBindsAndRefsIO ctx tree = do
    subForest <-
        filter ((\d -> isBind d || isRefactor d) . sDecision)
            <$> positiveSubForestIO ctx tree
    case subForest of
        [] -> return tree
        _ -> allBindsAndRefsIO ctx $ minimumOn (defScore . sDecision) subForest

refactorThreadIO ctx tree = do
    subForest <- positiveSubForestIO ctx tree
    maybe (return tree) (refactorThreadIO ctx) $
        L.find (isRefactor . sDecision) subForest

smartBindThreadIO :: (SynthesisMethodConstraints tag v x t) => BackendCtx tag v x t -> SynthesisMethod tag v x t
smartBindThreadIO ctx tree = do
    subForest <-
        filter ((\d -> isBind d || isRefactor d) . sDecision)
            <$> (positiveSubForestIO ctx =<< refactorThreadIO ctx tree)
    case subForest of
        [] -> return tree
        _ -> smartBindThreadIO ctx $ maximumOn (defScore . sDecision) subForest

allBestThreadIO :: (SynthesisMethodConstraints tag v x t) => BackendCtx tag v x t -> Int -> SynthesisMethod tag v x t
allBestThreadIO ctx (0 :: Int) tree = bestThreadIO ctx stepLimit tree
allBestThreadIO ctx n tree = do
    subForest <- positiveSubForestIO ctx tree
    leafs <- mapM (allBestThreadIO ctx (n - 1)) subForest
    return $ bestLeaf tree leafs

bestLeaf :: (SynthesisMethodConstraints tag v x t) => DefTree tag v x t -> [DefTree tag v x t] -> DefTree tag v x t
bestLeaf tree leafs =
    let successLeafs = filter (\node -> isComplete node && isLeaf node) leafs
     in case successLeafs of
            [] -> headDef tree leafs
            _ : _ ->
                minimumOn
                    (\Tree{sState = SynthesisState{sTarget}} -> (processDuration sTarget, puSize sTarget))
                    successLeafs

topDownScoreSynthesisIO :: (SynthesisMethodConstraints tag v x t) => Float -> Int -> Maybe Text -> BackendCtx tag v x t -> SynthesisMethod tag v x t
topDownScoreSynthesisIO = topDownScoreSynthesisIO' (H.empty :: H.MaxPrioHeap Float (DefTree tag v x t)) 0

topDownScoreSynthesisIO' heap step depthCoeffBase limit scoreKey ctx currentNode = do
    if step > limit
        then do
            infoM "NITTA.Synthesis" $ "topDownScoreSynthesisIO - STEP LIMIT REACHED: " <> show (sID currentNode)
            return currentNode
        else do
            -- currentNode should not be in the heap at this point, but all its children will be
            subForest <- positiveSubForestIO ctx currentNode
            let getNodeDepth node = (\(Sid sidParts) -> length sidParts) $ sID node
                -- priority calculation should prefer nodes that are closer to the leafs
                depthCoeff = depthCoeffBase ** fromIntegral (getNodeDepth currentNode)
                getScore node = case scoreKey of
                    Nothing -> defScore . sDecision $ node
                    Just key -> scores (sDecision node) M.! key
                getPriority node = depthCoeff * getScore node
                -- heapWithSubforest = foldl (\acc child -> H.insert (getPriority child, child) acc) heap subForest
                -- the version above (fold + insert) takes 1.7x more time than below (fromList + union)
                subForestOnlyHeap = H.fromList [(getPriority child, child) | child <- subForest]
                heapWithSubforest = H.union heap subForestOnlyHeap

            case H.viewHead heapWithSubforest of
                Nothing -> do
                    infoM "NITTA.Synthesis" $ "topDownScoreSynthesisIO - TREE EXHAUSTED: " <> show (sID currentNode)
                    return currentNode
                Just (_, nextBestScoreNode) -> do
                    if isComplete nextBestScoreNode
                        then do
                            infoM "NITTA.Synthesis" $ "topDownScoreSynthesisIO - DONE: " <> show (sID nextBestScoreNode)
                            return nextBestScoreNode
                        else do
                            let prio = getPriority nextBestScoreNode
                                depth = getNodeDepth nextBestScoreNode
                                score = getScore nextBestScoreNode
                                -- the more steps we make, the more aggressively we drop nodes to find new ones
                                aggressiveness = 0.5 :: Float -- [0.0; +inf)
                                maxDrops = 5
                                aggressiveDropPerSteps = 2500.0
                                aggressiveDropCount = min (maxDrops - 1) $ fromIntegral step / aggressiveDropPerSteps * aggressiveness
                                dropCount = round $ 1 + aggressiveDropCount * aggressiveness

                            infoM
                                "NITTA.Synthesis"
                                ( printf
                                    "topDownScoreSynthesisIO: prio=%-15s depth=%-5s score=%-10s drops=%-3s %s -> %s"
                                    (show prio)
                                    (show depth)
                                    (show score)
                                    (show dropCount)
                                    (show $ sID currentNode)
                                    (show $ sID nextBestScoreNode)
                                )

                            topDownScoreSynthesisIO' (H.drop dropCount heapWithSubforest) (step + 1) depthCoeffBase limit scoreKey ctx nextBestScoreNode

{- | Shortcut for constraints in signatures of synthesis method functions.
This used to be (VarValTime v x t, UnitTag tag). See below for more info.
-}
type SynthesisMethodConstraints tag v x t = (VarValTimeJSON v x t, ToJSON tag, UnitTag tag)

-- FIXME: Validate the type above, its usages and meaning in the context of changes described below.
--
--      Ilya Burakov is not sure why signatures of synthesis method functions were explicitly defined
--      (not inferred) and why they are what they are, but introduction of JSON body formatting
--      for ML backend node scoring requests in NITTA.Synthesis.Explore module forced to add JSON-related
--      constraints to them.
--
--      Also, it has spilled to Default interface in NITTA.Synthesis. See usages of
--      SynthesisMethodConstraints for all related changes.
--
--      Effectvely, those constraints were added:
--          - ToJSONKey v, ToJSON v, ToJSON x, ToJSON t (via ValValTime -> ValValTimeJSON)
--          - ToJSON tag (explicitly)
--
--      Related chain of dependencies:
--      stateOfTheArtSynthesisIO -> bestThreadIO (or others) -> positiveSubForestIO -> subForestIO ->
--      predictScoresIO -> ScoringInput -> NodeView
--
--      Not sure if it's the right way to do it, but it works for now. Please, validate and fix if needed.
