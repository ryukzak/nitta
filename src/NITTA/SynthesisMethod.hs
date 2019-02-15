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
    , obviousBindThreadIO
    , allBestThreadIO
    ) where

import           Data.List             (find)
import           NITTA.DataFlow        (targetProcessDuration)
import           NITTA.Types.Synthesis
import           NITTA.Utils           (maximumOn, minimumOn)


-- |Schedule process by simple synthesis.
simpleSynthesisIO root = do
    lastObliviusNode <- obviousBindThreadIO root
    allBestThreadIO 1 lastObliviusNode



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



allBestThreadIO (0 :: Int) node = bestThreadIO node
allBestThreadIO n node = do
    edges <- getEdgesIO node
    lastNodes <- mapM (\Edge{ eNode } -> allBestThreadIO (n-1) eNode) edges
    let completedNodes = filter nIsComplete lastNodes
    return $ if null completedNodes
        then head lastNodes
        else minimumOn (targetProcessDuration . nModel) completedNodes
