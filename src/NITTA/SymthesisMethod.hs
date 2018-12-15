{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.SymthesisMethod
Description : Synthesis methods and its parts implementation.
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.SymthesisMethod
    ( simpleSynthesis, simpleSynthesisIO
    , obliousBindThread, obliousBindThreadIO
    , allBestThread, allBestThreadIO
    ) where

import           Control.Concurrent.STM
import           Data.List              (find)
import           NITTA.Types.Synthesis
import           NITTA.Utils


-- |Schedule process by simple synthesis.
simpleSynthesisIO m = atomically $ simpleSynthesis m

simpleSynthesis root
    =   return root
    >>= obliousBindThread
    >>= allBestThread 1



bestThread node = do
    edges <- getEdges node
    case edges of
        [] -> return node
        _  -> bestThread $ eNode $ maximumOn eCharacteristic edges



obliousBindThreadIO node = atomically $ obliousBindThread node

obliousBindThread node = do
    edges <- getEdges node
    let obliousBind = find
            ((\case
                BindingMetrics{ alternative } -> alternative == 1
                _                             -> False
            ) . eCharacteristics)
            edges
    case obliousBind of
        Just Edge{ eNode } -> obliousBindThread eNode
        Nothing            -> return node



allBestThreadIO n node = atomically $ allBestThread n node

allBestThread (0 :: Int) node = bestThread node
allBestThread n node = do
    edges <- getEdges node
    lastNodes <- mapM (\Edge{ eNode } -> allBestThread (n-1) eNode) edges
    return $ minimumOn (targetProcessDuration . nModel)
        $ filter nIsComplete lastNodes
