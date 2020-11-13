{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module      : NITTA.Model.Problems.Refactor
Description : Optimize an algorithm for Accum processor unit
Copyright   : (c) Daniil Prohorov, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Refactor.Accum
    ( optimizeAccumOptions
    , optimizeAccumDecision
    ) where

import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems
import           NITTA.Model.Types


instance ( Var v, Val x
        ) => RefactorProblem (DataFlowGraph v x) v x where
    refactorOptions dfg = optimizeAccumOptions dfg

    refactorDecision dfg r@ResolveDeadlock{} = let
            ( buffer, diff ) = prepareBuffer r
            fs' = buffer : map (patch diff) (functions dfg)
        in fsToDataFlowGraph fs'

    refactorDecision (DFCluster leafs) bl@BreakLoop{} = let
            origin = recLoop bl
        in DFCluster
            $ DFLeaf (recLoopIn bl){ funHistory=[origin] }
            : DFLeaf (recLoopOut bl){ funHistory=[origin] }
            : ( leafs L.\\ [ DFLeaf origin ] )

    refactorDecision dfg ref@OptimizeAccum{} = optimizeAccumDecision dfg ref

    refactorDecision _ _ = error "DataFlowGraph "


-- |Function takes algorithm in 'DataFlowGraph' and return list of 'Refactor' that can be done
optimizeAccumOptions dfg = refactorContainers $ filter ((> 1) . length) $ createContainers $ dataFlowGraphToFs dfg

-- |Function takes 'OptimizeAccum' and modify 'DataFlowGraph'
optimizeAccumDecision dfg OptimizeAccum{ refOld, refNew } = fsToDataFlowGraph refactoredFs
        where
            refactoredFs = filtered ++ refNew
            filtered = filter (`notElem` refOld) fs
            fs = dataFlowGraphToFs dfg


optimizeAccumDecision _ _ = error "In this function we can make decision only with OptimizeAccum option"

toOptimizeAccum lst = map (uncurry OptimizeAccum) $ filterSameListsTuple lst

filterSameListsTuple lst = filter (uncurry conditionSameLists) lst

conditionSameLists lst1 lst2
    | S.fromList lst1 == S.fromList lst2 = False
    | otherwise   = True

refactorContainers containers = toOptimizeAccum $ zip containers refContainers
    where
        refContainers = map refactorContainer containers

createContainers fs
    | length filtered == 1 = containered
    | otherwise              = map S.toList listOfSets
    where
        listOfSets = S.toList $ S.fromList [toOneContainer fs1 fs2 | fs1 <- containered , fs2 <- containered, fs1 /= fs2]
        filtered = filter isSupportByAccum fs
        containered = map (:[]) filtered

isSupportByAccum f
    | Just Add{} <- castF f = True
    | Just Sub{} <- castF f = True
    | Just Acc{} <- castF f = True
    | otherwise             = False


toOneContainer fs fs'
    | not $ null $ S.intersection
        (foldl1 S.union (map inputs fs'))
        (foldl1 S.union (map outputs fs)) = S.fromList $ fs ++ fs'
    | otherwise                           = S.fromList fs


-- |Create Map String (HistoryTree (F v x)), where Key is input label and Value is FU that contain this input label
containerMapCreate fs = M.unions $
    map
    (\f ->
       foldl
       (\dataMap k ->
          M.insertWith (++) k [f] dataMap
       ) M.empty (S.toList $ inputs f)
    ) fs


-- |Takes container and refactor it, if it can be
refactorContainer [f] = [f]
refactorContainer fs = concatMap refactored fs
    where
        containerMap = containerMapCreate fs

        refactored f = concatMap
            (\o ->
                case M.findWithDefault [] o containerMap of
                    []         -> []
                    matchedFUs -> concatMap (refactorFunction f) matchedFUs
            ) (S.toList $ outputs f)


refactorFunction f' f
    | Just (Acc lst') <- castF f'
    , Just (Acc lst ) <- castF f
    , let
        multipleOutBool = (1 <) $ length $ outputs f'
        isOutInpIntersect = any
            (\case
                Push _ (I v) -> elem v $ outputs f'
                _            -> False
            ) lst
        makeRefactor = not multipleOutBool && isOutInpIntersect
    in
        makeRefactor = let
                subs _ (Push Minus _) (Push Plus v)   = Just $ Push Minus v
                subs _ (Push Minus _) (Push Minus v)  = Just $ Push Plus v
                subs _ (Push Plus _)  push@(Push _ _) = Just push
                subs v _              pull@(Pull _)   = deleteFromPull v pull
                subs _ _              _               = error "Pull can not be here"

                refactorAcc _ _ (Pull o) = [Pull o]
                refactorAcc accList accNew accOld@(Push s i@(I v))
                    | elem v $ outputs accNew = mapMaybe ( subs v accOld ) accList

                    | s == Minus = [Push Minus i]
                    | s == Plus = [Push Plus i]
                refactorAcc _ _ (Push _ (I _)) = undefined
            in
                [ packF $ Acc $ concatMap (refactorAcc lst' f') lst ]

    | Just f1 <- fromAddSub f'
    , Just f2 <- fromAddSub f = case refactorFunction f1 f2 of
            [fNew] -> [fNew]
            _      -> [f, f']

    | otherwise = [f, f']

deleteFromPull v (Pull (O s))
    | S.null deleted = Nothing
    | otherwise      = Just $ Pull $ O deleted
        where
            deleted = S.delete v s

deleteFromPull _ (Push _ _) = error "delete only Pull"


fromAddSub f
    | Just (Add in1 in2 (O out)) <- castF f = Just $ acc $
        [Push Plus in1, Push Plus in2] ++ [Pull $ O $ S.fromList [o] | o <- S.toList out]
    | Just (Sub in1 in2 (O out)) <- castF f = Just $ acc $
        [Push Plus in1, Push Minus in2] ++ [Pull $ O $ S.fromList [o] | o <- S.toList out]
    | otherwise = Nothing
