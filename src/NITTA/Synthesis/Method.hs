{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
) where

import qualified Data.List as L
import Data.Typeable
import Debug.Trace
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Synthesis.Explore
import NITTA.Synthesis.Steps
import NITTA.Synthesis.Types
import NITTA.Utils (maximumOn, minimumOn)
import Safe
import System.Log.Logger

{- |The constant, which restricts the maximum number of synthesis steps. Avoids
the endless synthesis process.
-}
stepLimit = 750 :: Int

-- |The most complex synthesis method, which embedded all another. That all.
stateOfTheArtSynthesisIO :: (VarValTime v x t, UnitTag tag) => SynthesisMethod tag v x t
stateOfTheArtSynthesisIO tree = do
    infoM "NITTA.Synthesis" $ "stateOfTheArtSynthesisIO: " <> show (sID tree)
    l1 <- simpleSynthesisIO tree
    l2 <- smartBindSynthesisIO tree
    l3 <- bestThreadIO stepLimit tree
    l4 <- bestThreadIO stepLimit =<< allBindsAndRefsIO tree
    return $ bestLeaf tree [l1, l2, l3, l4]

-- |Schedule process by simple synthesis.
simpleSynthesisIO :: (VarValTime v x t, UnitTag tag) => SynthesisMethod tag v x t
simpleSynthesisIO root = do
    infoM "NITTA.Synthesis" $ "simpleSynthesisIO: " <> show (sID root)
    lastObliviousNode <- obviousBindThreadIO root
    allBestThreadIO 1 lastObliviousNode

smartBindSynthesisIO :: (VarValTime v x t, UnitTag tag) => SynthesisMethod tag v x t
smartBindSynthesisIO tree = do
    infoM "NITTA.Synthesis" $ "smartBindSynthesisIO: " <> show (sID tree)
    tree' <- smartBindThreadIO tree
    allBestThreadIO 1 tree'

bestThreadIO :: (VarValTime v x t, UnitTag tag) => Int -> SynthesisMethod tag v x t
bestThreadIO 0 node = return $ trace "bestThreadIO reach step limit!" node
bestThreadIO limit tree = do
    subForest <- positiveSubForestIO tree
    case subForest of
        [] -> return tree
        _ -> bestThreadIO (limit -1) $ maximumOn (score . sDecision) subForest

bestStepIO :: (VarValTime v x t, UnitTag tag) => SynthesisMethod tag v x t
bestStepIO tree = do
    subForest <- positiveSubForestIO tree
    case subForest of
        [] -> error "all step is over"
        _ -> return $ maximumOn (score . sDecision) subForest

obviousBindThreadIO :: (VarValTime v x t, UnitTag tag) => SynthesisMethod tag v x t
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

allBindsAndRefsIO :: (VarValTime v x t, UnitTag tag) => SynthesisMethod tag v x t
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

smartBindThreadIO :: (VarValTime v x t, UnitTag tag) => SynthesisMethod tag v x t
smartBindThreadIO tree = do
    subForest <-
        filter ((\d -> isBind d || isRefactor d) . sDecision)
            <$> (positiveSubForestIO =<< refactorThreadIO tree)
    case subForest of
        [] -> return tree
        _ -> smartBindThreadIO $ maximumOn (score . sDecision) subForest

allBestThreadIO :: (VarValTime v x t, UnitTag tag) => Int -> SynthesisMethod tag v x t
allBestThreadIO (0 :: Int) tree = bestThreadIO stepLimit tree
allBestThreadIO n tree = do
    subForest <- positiveSubForestIO tree
    leafs <- mapM (allBestThreadIO (n -1)) subForest
    return $ bestLeaf tree leafs

bestLeaf :: (VarValTime v x t, UnitTag tag) => DefTree tag v x t -> [DefTree tag v x t] -> DefTree tag v x t
bestLeaf tree leafs =
    let successLeafs = filter (\node -> isComplete node && isLeaf node) leafs
     in case successLeafs of
            _ : _ -> minimumOn (processDuration . sTarget . sState) successLeafs
            [] -> headDef tree leafs
