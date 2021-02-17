{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

-- Before compile-time eval optimization

--   +------------------+
--   |                  |         +-------------------------+
--   | Constant 2 "2#1" |         |                         |
--   |                  +-------->+                         |      +--------------+
--   +------------------+         |                         |      |              |
--                                | Add "2#1" "3#1" ["res"] +----->+    ......    |
--   +------------------+         |                         |      |              |
--   |                  +-------->+                         |      +--------------+
--   | Constant 3 "3#1" |         |                         |
--   |                  |         +-------------------------+
--   +------------------+

-- +--------------------------------------------------------------------------------+

-- After compile-time eval optimization

--   +------------------+         +--------------+
--   |                  |         |              |
--   | Constant 5 "res" +-------->+    ......    |
--   |                  |         |              |
--   +------------------+         +--------------+

{- |
Module      : NITTA.Model.Problems.Refactor.CompileTimeEval
Description : Optimize an algorithm for Accum processor unit
Copyright   : (c) Daniil Prohorov, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Refactor.CompileTimeEval (
    CompileTimeEval (..),
    CompileTimeEvalProblem (..),
) where

import Data.Default
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace
import GHC.Generics
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types

data CompileTimeEval v x = CompileTimeEval
    { cRefOld :: [F v x]
    , cRefNew :: [F v x]
    }
    deriving (Generic, Show, Eq)

class CompileTimeEvalProblem u v x | u -> v x where
    -- |Function takes algorithm in 'DataFlowGraph' and return list of optimizations that can be done
    compileTimeEvalOptions :: u -> [CompileTimeEval v x]
    compileTimeEvalOptions _ = []

    -- |Function takes 'CompileTimeEval' and modify 'DataFlowGraph'
    compileTimeEvalDecision :: u -> CompileTimeEval v x -> u
    compileTimeEvalDecision _ _ = error "not implemented"

instance (Var v, Val x) => CompileTimeEvalProblem [F v x] v x where
    compileTimeEvalOptions fs =
        let clusters = selectClusters fs
            evaluatedClusters = map evalCluster clusters
            zipOfClusters = zip clusters evaluatedClusters
            filteredZip = filter (\case ([_], _) -> False; _ -> True) zipOfClusters
            options = [CompileTimeEval{cRefOld = c, cRefNew = ec} | (c, ec) <- filteredZip, c /= ec]
            optionsFiltered = filter blankOptions options
            blankOptions (compileTimeEvalDecision fs -> []) = False
            blankOptions (compileTimeEvalDecision fs -> _) = True
         in optionsFiltered

    compileTimeEvalDecision fs CompileTimeEval{cRefOld, cRefNew}
        | cRefOld == cRefNew = cRefNew
        | otherwise = deleteExtraF $ (fs L.\\ cRefOld) <> cRefNew

isConst f
    | Just Constant{} <- castF f = True
    | otherwise = False

selectClusters fs =
    let consts = filter isConst fs
        inputsAreConst f = inputs f `S.isSubsetOf` S.unions (map outputs consts)
        getInputConsts f = filter (\c -> outputs c `S.isSubsetOf` inputs f) consts
        createCluster f
            | inputsAreConst f = f : getInputConsts f
            | otherwise = [f]
     in map createCluster fs

evalCluster [f] = [f]
evalCluster fs = outputResult
    where
        (consts, [f]) = L.partition isConst fs
        cntx = CycleCntx $ M.fromList $ concatMap (simulate def) consts
        outputResult
            | Just Send{} <- castF f = fs
            | otherwise = map (\(v, x) -> constant x [v]) (simulate cntx f) ++ consts

deleteExtraF fs =
    L.nub
        [ f1
        | f1 <- fs
        , f2 <- fs
        , f1 /= f2
        , not $ null (variables f1 `S.intersection` variables f2)
        ]

apply [] _ = []
apply ((cluster, evCluster) : clustersTuple) clusters = deleteExtraF newFs : apply clustersTuple clusters
    where
        (h, _ : t) = L.span (/= cluster) clusters
        newFs = deleteExtraF $ concat $ evCluster : h ++ t
