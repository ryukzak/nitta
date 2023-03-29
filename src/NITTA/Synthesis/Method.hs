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
    allBestThreadIO,
    stateOfTheArtSynthesisIO,
    allBindsAndRefsIO,
    bestStepIO,
    SynthesisMethodConstraints,
) where

import Data.Aeson (ToJSON)
import Data.List qualified as L
import Data.Typeable
import Debug.Trace
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Synthesis.Analysis
import NITTA.Synthesis.Explore
import NITTA.Synthesis.Steps
import NITTA.Synthesis.Types
import NITTA.UIBackend.ViewHelper
import NITTA.Utils (maximumOn, minimumOn)
import Safe
import System.Log.Logger

{- | The constant, which restricts the maximum number of synthesis steps. Avoids
the endless synthesis process.
-}
stepLimit = 750 :: Int

-- | The most complex synthesis method, which embedded all another. That all.
stateOfTheArtSynthesisIO :: (SynthesisMethodConstraints tag v x t) => SynthesisMethod tag v x t
stateOfTheArtSynthesisIO tree = do
    infoM "NITTA.Synthesis" $ "stateOfTheArtSynthesisIO: " <> show (sID tree)
    l1 <- simpleSynthesisIO tree
    l2 <- smartBindSynthesisIO tree
    l3 <- bestThreadIO stepLimit tree
    l4 <- bestThreadIO stepLimit =<< allBindsAndRefsIO tree
    return $ bestLeaf tree [l1, l2, l3, l4]

-- | Schedule process by simple synthesis.
simpleSynthesisIO :: (SynthesisMethodConstraints tag v x t) => SynthesisMethod tag v x t
simpleSynthesisIO root = do
    infoM "NITTA.Synthesis" $ "simpleSynthesisIO: " <> show (sID root)
    lastObliviousNode <- obviousBindThreadIO root
    allBestThreadIO 1 lastObliviousNode

smartBindSynthesisIO :: (SynthesisMethodConstraints tag v x t) => SynthesisMethod tag v x t
smartBindSynthesisIO tree = do
    infoM "NITTA.Synthesis" $ "smartBindSynthesisIO: " <> show (sID tree)
    tree' <- smartBindThreadIO tree
    allBestThreadIO 1 tree'

bestThreadIO :: (SynthesisMethodConstraints tag v x t) => Int -> SynthesisMethod tag v x t
bestThreadIO 0 node = return $ trace "bestThreadIO reach step limit!" node
bestThreadIO limit tree = do
    subForest <- positiveSubForestIO tree
    case subForest of
        [] -> return tree
        _ -> bestThreadIO (limit - 1) $ maximumOn (score . sDecision) subForest

bestStepIO :: (SynthesisMethodConstraints tag v x t) => SynthesisMethod tag v x t
bestStepIO tree = do
    subForest <- positiveSubForestIO tree
    case subForest of
        [] -> error "all step is over"
        _ -> return $ maximumOn (score . sDecision) subForest

obviousBindThreadIO :: (SynthesisMethodConstraints tag v x t) => SynthesisMethod tag v x t
obviousBindThreadIO tree = do
    subForest <- positiveSubForestIO tree
    maybe (return tree) obviousBindThreadIO $
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

allBindsAndRefsIO :: (SynthesisMethodConstraints tag v x t) => SynthesisMethod tag v x t
allBindsAndRefsIO tree = do
    subForest <-
        filter ((\d -> isBind d || isRefactor d) . sDecision)
            <$> positiveSubForestIO tree
    case subForest of
        [] -> return tree
        _ -> allBindsAndRefsIO $ minimumOn (score . sDecision) subForest

refactorThreadIO tree = do
    subForest <- positiveSubForestIO tree
    maybe (return tree) refactorThreadIO $
        L.find (isRefactor . sDecision) subForest

smartBindThreadIO :: (SynthesisMethodConstraints tag v x t) => SynthesisMethod tag v x t
smartBindThreadIO tree = do
    subForest <-
        filter ((\d -> isBind d || isRefactor d) . sDecision)
            <$> (positiveSubForestIO =<< refactorThreadIO tree)
    case subForest of
        [] -> return tree
        _ -> smartBindThreadIO $ maximumOn (score . sDecision) subForest

allBestThreadIO :: (SynthesisMethodConstraints tag v x t) => Int -> SynthesisMethod tag v x t
allBestThreadIO (0 :: Int) tree = bestThreadIO stepLimit tree
allBestThreadIO n tree = do
    subForest <- positiveSubForestIO tree
    leafs <- mapM (allBestThreadIO (n - 1)) subForest
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
