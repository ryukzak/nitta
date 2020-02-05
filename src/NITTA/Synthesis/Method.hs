{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Synthesis.Method
Description : Synthesis method implementation.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Method
    ( simpleSynthesisIO
    , smartBindSynthesisIO
    , obviousBindThreadIO
    , allBestThreadIO
    , stateOfTheArtSynthesisIO
    , allBindsAndRefsIO
    , bestStepIO
    ) where

import           Data.List                (find, sortOn)
import           Data.Ord                 (Down (..))
import           Debug.Trace
import           NITTA.Synthesis.Estimate
import           NITTA.Synthesis.Tree
import           NITTA.Synthesis.Utils    (targetProcessDuration)
import           NITTA.Utils              (maximumOn, minimumOn)
import           Safe


-- |The constant, which restricts the maximum number of synthesis steps. Avoids
-- the endless synthesis process.
stepLimit = 100 :: Int


-- |The most complex synthesis method, which embedded all another. That all.
stateOfTheArtSynthesisIO node = do
    n1 <- simpleSynthesisIO node
    n2 <- smartBindSynthesisIO node
    n3 <- bestThreadIO stepLimit node
    n4 <- bestThreadIO stepLimit =<< allBindsAndRefsIO node
    return $ getBestNode node [ n1, n2, n3, n4 ]


-- |Schedule process by simple synthesis.
simpleSynthesisIO root = do
    lastObliviousNode <- obviousBindThreadIO root
    allBestThreadIO 1 lastObliviousNode


smartBindSynthesisIO root = do
    node <- smartBindThreadIO root
    allBestThreadIO 1 node


bestThreadIO 0 node = return $ trace "bestThreadIO reach step limit!" node
bestThreadIO limit node = do
    edges <- getPositiveEdgesIO node
    case edges of
        [] -> return node
        _  -> bestThreadIO (limit - 1) $ eTarget $ maximumOn eObjectiveFunctionValue edges

bestStepIO node = do
    edges <- getPositiveEdgesIO node
    case edges of
        [] -> error "all step is over"
        _  -> return $ eTarget $ maximumOn eObjectiveFunctionValue edges


obviousBindThreadIO node = do
    edges <- getPositiveEdgesIO node
    let obliousBind = find
            ((\case
                BindEdgeParameter{ pPossibleDeadlock=True } -> False
                BindEdgeParameter{ pAlternative=1 } -> True
                _ -> False
            ) . eParameters)
            edges
    case obliousBind of
        Just Edge{ eTarget } -> obviousBindThreadIO eTarget
        Nothing              -> return node


allBindsAndRefsIO node = do
    edges <- filter
        ( \Edge{ eParameters } -> case eParameters of BindEdgeParameter{} -> True; RefactorEdgeParameter{} -> True; _ -> False )
        <$> getPositiveEdgesIO node
    if null edges
        then return node
        else allBindsAndRefsIO $ eTarget $ minimumOn eObjectiveFunctionValue edges


refactorThreadIO node = do
    edges <- getPositiveEdgesIO node
    let refEdge = find
            ((\case
                RefactorEdgeParameter{} -> True
                _ -> False
            ) . eParameters)
            edges
    case refEdge of
        Just Edge{ eTarget } -> refactorThreadIO eTarget
        Nothing              -> return node


smartBindThreadIO node = do
    node' <- refactorThreadIO node
    edges <- getPositiveEdgesIO node'
    let binds = sortOn (Down . eObjectiveFunctionValue) $ filter
            ((\case
                BindEdgeParameter{} -> True
                _ -> False
            ) . eParameters)
            edges
    case binds of
        Edge{ eTarget }:_ -> smartBindThreadIO eTarget
        []                -> return node'


allBestThreadIO (0 :: Int) node = bestThreadIO stepLimit node
allBestThreadIO n node = do
    edges <- getPositiveEdgesIO node
    sythesizedNodes <- mapM (\Edge{ eTarget } -> allBestThreadIO (n-1) eTarget) edges
    return $ getBestNode node sythesizedNodes


getBestNode node nodes = let
        successNodes = filter nIsComplete nodes
    in case successNodes of
        _:_ -> minimumOn (targetProcessDuration . nModel) successNodes
        []  -> headDef node nodes
