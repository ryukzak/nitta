-- All extensions should be enabled explicitly due to doctest in this module.
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
The optimization consists of two parts:
1) Replacing logic functions with lookup tables
2) Searching and merging lookup tables, if possible

>>> let a = constant 1 ["a"]
>>> let b = constant 2 ["b"]
>>> let c = constant 3 ["c"]
>>> let f1 = logicAnd "a" "b" ["f1"]
>>> let f2 = logicOr "f1" "c" ["f2"]
>>> let loopRes = loop 1 "e" ["f2"]
>>> let fs = [a, b, c, f1, f2, loopRes] :: [F String Int]
>>> optimizeLogicalUnitDecision fs $ head $ optimizeLogicalUnitOptions fs
[const(1) = a,const(2) = b,const(3) = c,loop(1, e) = f2,TruthTable fromList [([False,False,False],False),([False,False,True],True),([False,True,False],False),([False,True,True],True),([True,False,False],False),([True,False,True],True),([True,True,False],True),([True,True,True],True)] [a,b,c] = f2]
-}
module NITTA.Model.Problems.Refactor.OptimizeLogicalUnit (
    OptimizeLogicalUnit (..),
    OptimizeLogicalUnitProblem (..),
)
where

import Control.Monad (replicateM)
import Data.Foldable (foldl')
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import GHC.Generics
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types

data OptimizeLogicalUnit v x = OptimizeLogicalUnit
    { rOld :: [F v x]
    , rNew :: [F v x]
    }
    deriving (Generic, Show, Eq)

class OptimizeLogicalUnitProblem u v x | u -> v x where
    optimizeLogicalUnitOptions :: u -> [OptimizeLogicalUnit v x]
    optimizeLogicalUnitOptions _ = []

    -- | Function takes 'OptimizeLogicalUnit' and modify 'DataFlowGraph'
    optimizeLogicalUnitDecision :: u -> OptimizeLogicalUnit v x -> u
    optimizeLogicalUnitDecision _ _ = error "not implemented"

instance (Var v, Val x) => OptimizeLogicalUnitProblem [F v x] v x where
    optimizeLogicalUnitOptions fs =
        let supportedFunctions = filter isSupportedByLogicalUnit fs

            rNew =
                if not (null supportedFunctions)
                    && isOptimizationNeeded supportedFunctions
                    then optimizeCluster supportedFunctions fs
                    else []
            result =
                [ OptimizeLogicalUnit{rOld = supportedFunctions, rNew}
                | not (null rNew) && S.fromList supportedFunctions /= S.fromList rNew
                ]
         in result

    optimizeLogicalUnitDecision fs OptimizeLogicalUnit{rOld, rNew} =
        deleteExtraLogicalUnits $ (fs L.\\ rOld) <> rNew

deleteExtraLogicalUnits fs =
    L.nub
        [ f1
        | f1 <- fs
        , f2 <- fs
        , f1 /= f2
        , not $ S.null (variables f1 `S.intersection` variables f2)
        ]

isOptimizationNeeded fs = countLogicalUnits fs > 1 || hasLogicFunctions fs
    where
        hasLogicFunctions fns = any isSupportedByLogicalUnit fns

        isLogicalUnit f = case castF f of
            Just (TruthTable{}) -> True
            _ -> False

        countLogicalUnits fns = length $ filter isLogicalUnit fns

isSupportedByLogicalUnit f
    | Just LogicAnd{} <- castF f = True
    | Just LogicOr{} <- castF f = True
    | Just LogicNot{} <- castF f = True
    | otherwise = False

optimizeCluster allFunctions _ =
    let clusters = findMergeClusters allFunctions
        mergedLogicalUnits = mapMaybe mergeCluster clusters

        singleFunctions = filter (\f -> isSupportedByLogicalUnit f && S.size (outputs f) /= 1) allFunctions
        singleLogicalUnits = mapMaybe convertToLOGICALUNIT singleFunctions

        remainingFunctions = allFunctions L.\\ (concat clusters ++ singleFunctions)
     in mergedLogicalUnits ++ singleLogicalUnits ++ remainingFunctions
    where
        mergeCluster cluster
            | isSingleOutputChain cluster = mergeLogicCluster M.empty cluster
            | otherwise = Nothing

        convertToLOGICALUNIT f = case castF f of
            Just (LogicAnd (I a) (I b) (O out)) ->
                buildCombinedLOGICALUNIT
                    [a, b]
                    out
                    ( \case
                        [x, y] -> x && y
                        _ -> error "Unexpected pattern"
                    )
            Just (LogicOr (I a) (I b) (O out)) ->
                buildCombinedLOGICALUNIT
                    [a, b]
                    out
                    ( \case
                        [x, y] -> x || y
                        _ -> error "Unexpected pattern"
                    )
            Just (LogicNot (I a) (O out)) ->
                buildCombinedLOGICALUNIT
                    [a]
                    out
                    ( \case
                        [x] -> not x
                        _ -> error "Unexpected pattern"
                    )
            _ -> Nothing

mergeLogicCluster _ fs =
    let (inputVars, finalOutput) = analyzeClusterIO fs
        evalFn = buildCombinedLogic fs inputVars
     in buildCombinedLOGICALUNIT inputVars finalOutput evalFn

isSingleOutputChain fs =
    all (\f -> S.size (outputs f) == 1) fs
        && all (== 1) [S.size (outputs (fs !! i) `S.intersection` inputs (fs !! (i + 1))) | i <- [0 .. length fs - 2]]

analyzeClusterIO fs =
    let allInputs = S.unions $ map inputs fs
        allOutputs = S.unions $ map outputs fs
        externalInputs = S.difference allInputs allOutputs
        finalOutput = outputs $ last fs
     in (S.toList externalInputs, finalOutput)

buildCombinedLogic fs inputVars =
    let evalCombination comb =
            let varMap = M.fromList $ zip inputVars comb
                resultMap = foldl' (\vm f -> applyLogicGate f vm) varMap fs
             in resultMap M.! S.elemAt 0 (outputs $ last fs)
     in evalCombination

applyLogicGate f varMap = case castF f of
    Just (LogicAnd (I a) (I b) (O out)) ->
        case S.toList out of
            [outVar] -> M.insert outVar (varMap M.! a && varMap M.! b) varMap
            _ -> error "LogicAnd must have exactly one output: 1"
    Just (LogicOr (I a) (I b) (O out)) ->
        case S.toList out of
            [outVar] -> M.insert outVar (varMap M.! a || varMap M.! b) varMap
            _ -> error "LogicOr must have exactly one output: 2"
    Just (LogicNot (I a) (O out)) ->
        case S.toList out of
            [outVar] -> M.insert outVar (not $ varMap M.! a) varMap
            _ -> error "LogicNot must have exactly one output: 3"
    _ -> varMap

buildCombinedLOGICALUNIT :: (Var v, Val x) => [v] -> S.Set v -> ([Bool] -> Bool) -> Maybe (F v x)
buildCombinedLOGICALUNIT inputVars outputSet evalFn =
    let logicalunitInputs = map I inputVars
        logicalunitOutput = O outputSet
        inputCombinations = replicateM (length inputVars) [False, True]
        tbl = M.fromList [(comb, evalFn comb) | comb <- inputCombinations]
     in Just $ packF $ TruthTable tbl logicalunitInputs logicalunitOutput

topSort :: Eq a => [(a, [a])] -> [a]
topSort [] = []
topSort g = case L.partition (null . snd) g of
    ([], _) -> []
    (ready, rest) ->
        map fst ready
            ++ topSort
                [ (x, filter (`notElem` readyNodes) ys)
                | (x, ys) <- rest
                ]
        where
            readyNodes = map fst ready

findMergeClusters :: Var v => [F v x] -> [[F v x]]
findMergeClusters fs =
    let deps = buildDependencyGraph fs
        sorted = reverse $ topSort deps
        clusters = groupChains sorted
     in clusters
    where
        buildDependencyGraph fns =
            [ (f, [g | g <- fns, sharesDependency f g])
            | f <- fns
            ]

        sharesDependency f g =
            not $ S.null (outputs f `S.intersection` inputs g)

        groupChains [] = []
        groupChains (x : xs) =
            let (chain, rest) = collectChain [x] xs
             in chain : groupChains rest
            where
                collectChain acc' [] = (acc', [])
                collectChain acc' (y : ys)
                    | sharesDependency (last acc') y
                        && isSingleOutputChain (acc' ++ [y]) =
                        collectChain (acc' ++ [y]) ys
                    | otherwise = (acc', y : ys)
