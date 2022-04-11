{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      : NITTA.Model.Problems.Refactor.ConstantFolding
Description : Constant folding optimization
Copyright   : (c) Daniil Prohorov, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

== ASCII digram

Before compile-time eval optimization

@
    +------------------+
    |                  |         +-------------------------+
    | Constant 2 "a"   |         |                         |
    |                  +-------->+                         |      +--------------+
    +------------------+         |                         |      |              |
                                 |   Add "a" "b" ["res"]   +----->+    ......    |
    +------------------+         |                         |      |              |
    |                  +-------->+                         |      +--------------+
    | Constant 3 "b1"  |         |                         |
    |                  |         +-------------------------+
    +------------------+
@

After compile-time eval optimization

@
    +------------------+         +--------------+
    |                  |         |              |
    | Constant 5 "res" +-------->+    ......    |
    |                  |         |              |
    +------------------+         +--------------+
@

== Example from ASCII diagram

>>> let a = constant 1 ["a"]
>>> let b = constant 2 ["b"]
>>> let res = add "a" "b" ["res"]
>>> loopRes = loop 1 "e" ["res"]
>>> let fs = [a, b, res, loopRes] :: [F String Int]
>>> constantFoldingDecision fs $ head $ constantFoldingOptions fs
[loop(1, e) = res,const(3) = res]
-}
module NITTA.Model.Problems.Refactor.ConstantFolding (
    ConstantFolding (..),
    ConstantFoldingProblem (..),
) where

import Data.Default
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
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
    constantFoldingOptions :: u -> [ConstantFolding v x]
    constantFoldingOptions _ = []

    -- |Function takes 'ConstantFolding' and modify 'DataFlowGraph'
    constantFoldingDecision :: u -> ConstantFolding v x -> u
    constantFoldingDecision _ _ = error "not implemented"

instance (Var v, Val x) => ConstantFoldingProblem [F v x] v x where
    constantFoldingOptions fs =
        let clusters = selectClusters fs
            evaluatedClusters = map evalCluster clusters
            zipOfClusters = zip clusters evaluatedClusters
            filteredZip = filter (\case ([_], _) -> False; _ -> True) zipOfClusters
            options = [ConstantFolding{cRefOld = c, cRefNew = ec} | (c, ec) <- filteredZip, c /= ec]
            optionsFiltered = filter isBlankOptions options
            isBlankOptions = not . null . constantFoldingDecision fs
         in optionsFiltered

    constantFoldingDecision fs ConstantFolding{cRefOld, cRefNew}
        | cRefOld == cRefNew = cRefNew
        | otherwise = deleteExtraF $ (fs L.\\ cRefOld) <> cRefNew

selectClusters fs =
    let consts = filter isConst fs
        isIntersection a b = not . S.null $ S.intersection a b
        inputsAreConst f = inputs f `S.isSubsetOf` S.unions (map outputs consts)
        getInputConsts f = filter (\c -> outputs c `isIntersection` inputs f) consts
        createCluster f
            | inputsAreConst f = f : getInputConsts f
            | otherwise = [f]
     in map createCluster fs

evalCluster [f] = [f]
evalCluster fs = outputResult
    where
        (consts, [f]) = L.partition isConst fs
        cntx = CycleCntx $ HM.fromList $ concatMap (simulate def) consts
        outputResult
            | null $ outputs f = fs
            | otherwise = map (\(v, x) -> constant x [v]) (simulate cntx f) <> consts

deleteExtraF fs =
    L.nub
        [ f1
        | f1 <- fs
        , f2 <- fs
        , f1 /= f2
        , not $ null (variables f1 `S.intersection` variables f2)
        ]
