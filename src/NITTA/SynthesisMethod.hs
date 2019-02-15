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
Module      : NITTA.SynthesisMethod
Description :
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.SynthesisMethod
    ( simpleSynthesisIO
    , smartBindSynthesisIO
    , obviousBindThreadIO
    , allBestThreadIO
    ) where

import           Data.List             (find, sortOn)
import           Data.Ord              (Down (..))
import           NITTA.DataFlow        (targetProcessDuration)
import           NITTA.Types.Synthesis
import           NITTA.Utils           (maximumOn, minimumOn)


-- |Schedule process by simple synthesis.
simpleSynthesisIO root = do
    lastObliviusNode <- obviousBindThreadIO root
    allBestThreadIO 1 lastObliviusNode


smartBindSynthesisIO root = do
    node <- smartBindThreadIO root
    allBestThreadIO 1 node


bestThreadIO node = do
    edges <- getEdgesIO node
    case edges of
        [] -> return node
        _  -> bestThreadIO $ eNode $ maximumOn eCharacteristic edges



obviousBindThreadIO node = do
    edges <- getEdgesIO node
    let obliousBind = find
            ((\case
                BindCh{ possibleDeadlock=True } -> False
                BindCh{ alternative=1 } -> True
                _ -> False
            ) . eCharacteristics)
            edges
    case obliousBind of
        Just Edge{ eNode } -> obviousBindThreadIO eNode
        Nothing            -> return node



refactorThreadIO node = do
    edges <- getEdgesIO node
    let refEdge = find
            ((\case
                RefactorCh{} -> True
                _ -> False
            ) . eCharacteristics)
            edges
    case refEdge of
        Just Edge{ eNode } -> refactorThreadIO eNode
        Nothing            -> return node



smartBindThreadIO node = do
    node' <- refactorThreadIO node
    edges <- getEdgesIO node'
    let binds = sortOn (Down . eCharacteristic) $ filter
            ((\case
                BindCh{} -> True
                _ -> False
            ) . eCharacteristics)
            edges
    case binds of
        Edge{ eNode }:_ -> smartBindThreadIO eNode
        []              -> return node'



allBestThreadIO (0 :: Int) node = bestThreadIO node
allBestThreadIO n node = do
    edges <- getEdgesIO node
    lastNodes <- mapM (\Edge{ eNode } -> allBestThreadIO (n-1) eNode) edges
    let completedNodes = filter nIsComplete lastNodes
    return $ if null completedNodes
        then head lastNodes
        else minimumOn (targetProcessDuration . nModel) completedNodes
