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
    ) where

import           Data.List             (find, sortOn)
import           Data.Ord              (Down (..))
import           NITTA.Synthesis.Types
import           NITTA.Synthesis.Utils (targetProcessDuration)
import           NITTA.Utils           (maximumOn, minimumOn)


-- |Schedule process by simple synthesis.
simpleSynthesisIO root = do
    lastObliviousNode <- obviousBindThreadIO root
    allBestThreadIO 1 lastObliviousNode


smartBindSynthesisIO root = do
    node <- smartBindThreadIO root
    allBestThreadIO 1 node


bestThreadIO node = do
    edges <- filter ((> 0) . eObjectiveFunctionValue) <$> getEdgesIO node
    case edges of
        [] -> return node
        _  -> bestThreadIO $ eNode $ maximumOn eObjectiveFunctionValue edges


obviousBindThreadIO node = do
    edges <- getEdgesIO node
    let obliousBind = find
            ((\case
                BindEdgeParameter{ pPossibleDeadlock=True } -> False
                BindEdgeParameter{ pAlternative=1 } -> True
                _ -> False
            ) . eParameters)
            edges
    case obliousBind of
        Just Edge{ eNode } -> obviousBindThreadIO eNode
        Nothing            -> return node


refactorThreadIO node = do
    edges <- getEdgesIO node
    let refEdge = find
            ((\case
                RefactorEdgeParameter{} -> True
                _ -> False
            ) . eParameters)
            edges
    case refEdge of
        Just Edge{ eNode } -> refactorThreadIO eNode
        Nothing            -> return node


smartBindThreadIO node = do
    node' <- refactorThreadIO node
    edges <- getEdgesIO node'
    let binds = sortOn (Down . eObjectiveFunctionValue) $ filter
            ((\case
                BindEdgeParameter{} -> True
                _ -> False
            ) . eParameters)
            edges
    case binds of
        Edge{ eNode }:_ -> smartBindThreadIO eNode
        []              -> return node'


allBestThreadIO (0 :: Int) node = bestThreadIO node
allBestThreadIO n node = do
    edges <- getEdgesIO node
    lastNodes <- mapM (\Edge{ eNode } -> allBestThreadIO (n-1) eNode) edges
    let 
        outNode [] = node
        outNode ln = case filter nIsComplete ln of
            []             -> head ln
            completedNodes -> minimumOn (targetProcessDuration . nModel) completedNodes
                           
    return $ outNode lastNodes 
         
