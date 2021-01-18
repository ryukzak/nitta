{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      : NITTA.Model.Problems.Refactor.CompileTimeEval
Description : Optimize an algorithm for Accum processor unit
Copyright   : (c) Daniil Prohorov, 2020
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
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types

data CompileTimeEval v x = CompileTimeEval
    { cRefOld :: [F v x]
    , cRefNew :: [F v x]
    }

class CompileTimeEvalProblem u v x | u -> v x where
    -- |Function takes algorithm in 'DataFlowGraph' and return list of 'Refactor' that can be done
    compileTimeEvalOptions :: u -> [CompileTimeEval v x]
    compileTimeEvalOptions _ = []

    -- |Function takes 'CompileTimeEval' and modify 'DataFlowGraph'
    compileTimeEvalDecision :: u -> CompileTimeEval v x -> u
    compileTimeEvalDecision _ _ = error "not implemented"

instance (Var v, Val x) => CompileTimeEvalProblem [F v x] v x where
    compileTimeEvalOptions fs = map (\fsNew -> CompileTimeEval{cRefOld = fs, cRefNew = fsNew}) newFsListFiltered
        where
            clusters = createClusters fs
            evClusters = map evalCluster clusters
            newFsList = L.nub $ apply (zip clusters evClusters) clusters
            newFsListFiltered = filter (\fs' -> not (null fs') && S.fromList fs' /= S.fromList fs) newFsList

    compileTimeEvalDecision _ (CompileTimeEval _ cRefNew) = cRefNew

isConst f
    | Just Constant{} <- castF f = True
    | otherwise = False

createClusters fs =
    [ if inputsAreConst f
        then f : getInputConsts f
        else [f]
    | f <- fs
    ]
    where
        consts = filter isConst fs
        inputsAreConst f = inputs f `S.isSubsetOf` S.unions (map outputs consts)
        getInputConsts f = filter (\c -> outputs c `S.isSubsetOf` inputs f) consts

evalCluster [f] = [f]
evalCluster fs = constant x [v] : consts
    where
        (consts, [f]) = L.partition isConst fs
        cntx = CycleCntx $ M.fromList $ concatMap (simulate def) consts
        [(v, x)] = simulate cntx f

deleteExtraF fs =
    L.nub
        [ f1
        | f1 <- fs
        , f2 <- fs
        , f1 /= f2
        , not $ null (variables f1 `S.intersection` variables f2)
        ]

apply [] clusters = []
apply ((cluster, evCluster) : clustersTuple) clusters = deleteExtraF newFs : apply clustersTuple clusters
    where
        (h, (_ : t)) = L.span (/= cluster) clusters
        newFs = deleteExtraF $ concat $ evCluster : h ++ t

-- compileEvalDecisions CompileTimeEvaluation {cRefOld, cRefNew } = fsToDataFlowGraph cRefNew

-- compileEvalOptions dfg = map (\fsNew -> CompileTimeEvaluation {cRefOld = fs, cRefNew = fsNew}) newFsListFiltered
--     where
--         fs = functions dfg
--         clusters = createClusters fs
--         evClusters = map evalCluster clusters
--         evClusters' = traceShow evClusters evClusters
--         newFsList = L.nub $ apply (zip clusters evClusters') clusters
--         newFsListFiltered = filter (\fs' -> not (null fs') && S.fromList fs' /=  S.fromList fs) $ trace ("newfsList = " ++ show newFsList) newFsList
