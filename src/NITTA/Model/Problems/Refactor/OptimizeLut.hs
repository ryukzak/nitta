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

module NITTA.Model.Problems.Refactor.OptimizeLut (
    OptimizeLut (..),
    OptimizeLutProblem (..),
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

data OptimizeLut v x = OptimizeLut
    { rOld :: [F v x]
    , rNew :: [F v x]
    }
    deriving (Generic, Show, Eq)

class OptimizeLutProblem u v x | u -> v x where
    -- | Function takes algorithm in 'DataFlowGraph' and return list of 'Refactor' that can be done
    optimizeLutOptions :: u -> [OptimizeLut v x]
    optimizeLutOptions _ = []

    -- | Function takes 'OptimizeLut' and modify 'DataFlowGraph'
    optimizeLutDecision :: u -> OptimizeLut v x -> u
    optimizeLutDecision _ _ = error "not implemented"

instance (Var v, Val x) => OptimizeLutProblem [F v x] v x where
    optimizeLutOptions fs =
        let supportedFunctions = filter isSupportedByLut fs

            rNew =
                if not (null supportedFunctions)
                    && isOptimizationNeeded supportedFunctions
                    then optimizeCluster supportedFunctions fs
                    else []
            result =
                [ OptimizeLut{rOld = supportedFunctions, rNew}
                | not (null rNew) && S.fromList supportedFunctions /= S.fromList rNew
                ]
         in result

    optimizeLutDecision fs OptimizeLut{rOld, rNew} =
        deleteExtraLuts $ (fs L.\\ rOld) <> rNew

deleteExtraLuts fs =
    L.nub
        [ f1
        | f1 <- fs
        , f2 <- fs
        , f1 /= f2
        , not $ S.null (variables f1 `S.intersection` variables f2)
        ]

isOptimizationNeeded fs = countLuts fs > 1 || hasLogicFunctions fs
    where
        hasLogicFunctions fns = any isLogicFunction fns

        isLogicFunction f = case castF f of
            Just LogicAnd{} -> True
            Just LogicOr{} -> True
            Just LogicNot{} -> True
            _ -> False

        isLut f = case castF f of
            Just (Lut{}) -> True
            _ -> False

        countLuts f = length $ filter isLut f

isSupportedByLut f
    | Just LogicAnd{} <- castF f = True
    | Just LogicOr{} <- castF f = True
    | Just LogicNot{} <- castF f = True
    | otherwise = False

optimizeCluster allFunctions _ =
    let clusters = findMergeClusters allFunctions
        mergedLuts = mapMaybe mergeCluster clusters

        singleFunctions = filter (\f -> isSupportedByLut f && S.size (outputs f) /= 1) allFunctions
        singleLuts = mapMaybe convertToLUT singleFunctions

        remainingFunctions = allFunctions L.\\ (concat clusters ++ singleFunctions)
     in mergedLuts ++ singleLuts ++ remainingFunctions
    where
        mergeCluster cluster
            | isSingleOutputChain cluster = mergeLogicCluster M.empty cluster
            | otherwise = Nothing

        convertToLUT f = case castF f of
            Just (LogicAnd (I a) (I b) (O out)) ->
                buildCombinedLUT
                    [a, b]
                    out
                    ( \case
                        [x, y] -> x && y
                        _ -> error "Unexpected pattern"
                    )
            Just (LogicOr (I a) (I b) (O out)) ->
                buildCombinedLUT
                    [a, b]
                    out
                    ( \case
                        [x, y] -> x || y
                        _ -> error "Unexpected pattern"
                    )
            Just (LogicNot (I a) (O out)) ->
                buildCombinedLUT
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
     in buildCombinedLUT inputVars finalOutput evalFn

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

buildCombinedLUT :: (Var v, Val x) => [v] -> S.Set v -> ([Bool] -> Bool) -> Maybe (F v x)
buildCombinedLUT inputVars outputSet evalFn =
    let lutInputs = map I inputVars
        lutOutput = O outputSet
        inputCombinations = replicateM (length inputVars) [False, True]
        tbl = M.fromList [(comb, evalFn comb) | comb <- inputCombinations]
     in Just $ packF $ Lut tbl lutInputs lutOutput

topSort :: Eq a => [(a, [a])] -> [a]
topSort [] = []
topSort g =
    let (ready, notReady) = L.partition (\(_, ds) -> null ds) g
     in if null ready
            then []
            else map fst ready ++ topSort [(x, ys L.\\ map fst ready) | (x, ys) <- notReady]

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
