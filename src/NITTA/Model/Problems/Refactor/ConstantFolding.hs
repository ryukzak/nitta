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
Module      : NITTA.Model.Problems.Refactor.ConstantFolding
Description : Optimize an algorithm for Accum processor unit
Copyright   : (c) Daniil Prohorov, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Refactor.ConstantFolding (
    ConstantFolding (..),
    ConstantFoldingProblem (..),
) where

import Data.Default
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import GHC.Generics
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types

data ConstantFolding v x = ConstantFolding
    { cRefOld :: [F v x]
    , cRefNew :: [F v x]
    }
    deriving (Generic, Show, Eq)

class ConstantFoldingProblem u v x | u -> v x where
    -- |Function takes algorithm in 'DataFlowGraph' and return list of optimizations that can be done
    compileTimeEvalOptions :: u -> [ConstantFolding v x]
    compileTimeEvalOptions _ = []

    -- |Function takes 'ConstantFolding' and modify 'DataFlowGraph'
    compileTimeEvalDecision :: u -> ConstantFolding v x -> u
    compileTimeEvalDecision _ _ = error "not implemented"

instance (Var v, Val x) => ConstantFoldingProblem [F v x] v x where
    compileTimeEvalOptions fs =
        let clusters = selectClusters fs
            evaluatedClusters = map evalCluster clusters
            zipOfClusters = zip clusters evaluatedClusters
            filteredZip = filter (\case ([_], _) -> False; _ -> True) zipOfClusters
            options = [ConstantFolding{cRefOld = c, cRefNew = ec} | (c, ec) <- filteredZip, c /= ec]
            optionsFiltered = filter blankOptions options
            blankOptions (compileTimeEvalDecision fs -> []) = False
            blankOptions (compileTimeEvalDecision fs -> _) = True
         in optionsFiltered

    compileTimeEvalDecision fs ConstantFolding{cRefOld, cRefNew}
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
