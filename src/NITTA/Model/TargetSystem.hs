{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : NITTA.Model.TargetSystem
Description : Model of target system for synthesis and so on.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.TargetSystem
-- TODO: rename to ModelState
    ( ModelState(..)
    , DataFlowGraph(..), fsToDataFlowGraph
    ) where

import           Control.Exception ( assert )
import qualified Data.List as L
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import           Debug.Trace
import           GHC.Generics
import           NITTA.Intermediate.Functions ( reg )
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Problems
import           NITTA.Model.ProcessorUnits.Time
import           NITTA.Model.Types
import           NITTA.Utils


-- |Model of target unit, which is a main subject of synthesis process and
-- synthesis graph.
data ModelState u v x
    = ModelState
        { mUnit          :: u -- ^model of target unit
        , mDataFlowGraph :: DataFlowGraph v x -- ^whole application algorithm
        }
    deriving ( Generic )

instance WithFunctions (ModelState (BusNetwork tag v x t) v x) (F v x) where
    functions ModelState{ mUnit, mDataFlowGraph }
        = assert (S.fromList (functions mUnit) == S.fromList (functions mDataFlowGraph)) -- inconsistent ModelState
            $ functions mUnit

instance ( UnitTag tag, VarValTime v x t
        ) => BindProblem (ModelState (BusNetwork tag v x t) v x) tag v x where
    bindOptions ModelState{ mUnit }      = bindOptions mUnit
    bindDecision f@ModelState{ mUnit } d = f{ mUnit=bindDecision mUnit d }

instance ( UnitTag tag, VarValTime v x t
        ) => DataflowProblem (ModelState (BusNetwork tag v x t) v x) tag v t
        where
    dataflowOptions ModelState{ mUnit }      = dataflowOptions mUnit
    dataflowDecision f@ModelState{ mUnit } d = f{ mUnit=dataflowDecision mUnit d }

instance ( UnitTag tag, VarValTime v x t, Num x
        ) => RefactorProblem (ModelState (BusNetwork tag v x t) v x) v x where
    refactorOptions ModelState{ mUnit } = refactorOptions mUnit

    refactorDecision ModelState{ mUnit, mDataFlowGraph } r@ResolveDeadlock{}
        = ModelState
            { mDataFlowGraph=refactorDecision mDataFlowGraph r
            , mUnit=refactorDecision mUnit r
            }

    refactorDecision ModelState{ mUnit, mDataFlowGraph } bl@BreakLoop{}
        = ModelState
            { mDataFlowGraph=refactorDecision mDataFlowGraph bl
            , mUnit=refactorDecision mUnit bl
            }


-- |Data flow graph - intermediate representation of application algorithm.
-- Right now can be replaced by @[F v x]@, but for future features like
-- conduction statement, we don't do that.
data DataFlowGraph v x
    = DFLeaf (F v x)
    | DFCluster [ DataFlowGraph v x ]
    deriving ( Show, Generic )

instance Eq ( DataFlowGraph v x) where
    (DFCluster c1) == (DFCluster c2) = S.fromList (map show c1) == S.fromList (map show c2)
    (DFLeaf f1) == (DFLeaf f2) = f1 == f2
    _ == _ = False

instance ( Var v, Val x ) => Patch (DataFlowGraph v x) (v, v) where
    patch diff@(v, v') (DFCluster cluster) = let
            newReg = DFLeaf $ reg v [v']
            cluster' = map (patch diff) cluster
        in assert (all (\case DFLeaf _ -> True; _ -> False) cluster) -- patch DataFlowGraph with subclusters is not support
            $ DFCluster $ newReg : cluster'
    patch diff@(v, _) n@(DFLeaf f)
        | v `S.member` inputs f = DFLeaf $ patch diff f
        | otherwise = n

instance ( Var v ) => Variables (DataFlowGraph v x) v where
    variables (DFLeaf fb)   = variables fb
    variables (DFCluster g) = unionsMap variables g

instance WithFunctions (DataFlowGraph v x) (F v x) where
    functions (DFLeaf f)    = [ f ]
    functions (DFCluster g) = concatMap functions g

instance ( Var v, Val x, Num x
        ) => RefactorProblem (DataFlowGraph v x) v x where
    refactorOptions dfg = [AlgSub $ dataFlowGraphToFs $ trace (show $ refactorDfg dfg) (refactorDfg dfg)]

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

    refactorDecision _ (AlgSub fList) = fsToDataFlowGraph fList

    refactorDecision _ _ = error "DataFlowGraph "

instance ( UnitTag tag, VarValTime v x t, Num x
         ) => SynthesisProblem (ModelState (BusNetwork tag v x t) v x) tag v x t where
    synthesisOptions m@ModelState{ mUnit } = concat
        [ map generalizeBinding $ bindOptions m
        , map generalizeDataflow $ dataflowOptions mUnit
        , map Refactor $ refactorOptions m
        ]

    synthesisDecision m (Binding f tag) = bindDecision m $ Bind f tag
    synthesisDecision m@ModelState{ mUnit } (Dataflow src trg) = m{ mUnit=dataflowDecision mUnit $ DataflowSt src trg }
    synthesisDecision m (Refactor d) = refactorDecision m d


-- |Convert @[ F v x ]@ to 'DataFlowGraph'.
fsToDataFlowGraph alg = DFCluster $ map DFLeaf alg

-- |Convert 'DataFlowGraph' to @[ F v x ]@.
dataFlowGraphToFs (DFCluster leafs) = map
    (\case
        DFLeaf f -> f
        _        -> error "Data flow graph structure error"
    )
    leafs
dataFlowGraphToFs _ = error "Data flow graph structure error"

filterAddSub []             = []
filterAddSub (f:fs)
    | Just Add{} <- castF f = Just f : filterAddSub fs
    | Just Sub{} <- castF f = Just f : filterAddSub fs
    | Just Acc{} <- castF f = Just f : filterAddSub fs
    | otherwise             = Nothing : filterAddSub fs

toOneContainer fs fs'
    | not $ null $ S.intersection
      (foldl1 S.union (map inputs fs))
      (foldl1 S.union (map outputs fs')) = S.fromList $ fs ++ fs'
    | not $ null $ S.intersection
      (foldl1 S.union (map inputs fs'))
      (foldl1 S.union (map outputs fs)) = S.fromList $ fs ++ fs'
    | otherwise = S.fromList fs


createContainers fs
    | (length filtered) == 1 = containered
    | otherwise              = map S.toList listOfSets
    where
        listOfSets = S.toList $ S.fromList [toOneContainer f1 f2 | f1 <- containered , f2 <- containered, f1 /= f2]
        filtered = catMaybes $ filterAddSub fs
        containered = map (\x -> [x]) filtered -- TODO: ADD HERE DIVIDING

refactorContainers containers = L.nub $ concatMap refactorContainer containers

data DataflowGraphSubstitute v x = DataflowGraphSubstitute{ oldSubGraph :: [F v x], newSubGraph :: Maybe [ F v x ] } deriving (Show, Eq)

containerMapCreate lstOf = M.unions $ map (\f -> foldl (\b k -> M.insertWith (++) k [f] b) M.empty $ S.toList $ inputs f) lstOf

refactorContainer [f] = [f]
refactorContainer lst = refactoredFuncs
    where
        -- Map (String (output)) (F v x)
        containerMap = containerMapCreate lst

        refactoredFuncs = refactored lst


        refactored [] = []
        refactored (f:fs) = let
            outputList = S.toList $ outputs f
            in
            concatMap (\o ->
                case M.findWithDefault [] o containerMap of
                    [] -> []
                    lst -> concat $ catMaybes $ map ( newSubGraph . (refactorFunctions f)) lst
            ) outputList
            ++ refactored fs

-- FIXME: rewrite logic
deleteFromPull v (Pull (O s))
    | deleted == S.empty = Nothing
    | otherwise        = Just $ Pull $ O $ deleted
        where
            deleted = S.delete v s
deleteFromPull _ (Push _ _) = error "delete only Pull"

refactorFunctions f' f
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
                newFList = [ packF
                    ( Acc $ concatMap
                        (\case
                            Push Plus i@(I v) -> if elem v $ outputs f'
                                then mapMaybe (\case
                                                pull@(Pull _) -> deleteFromPull v pull;
                                                inp -> Just inp
                                        ) lst'
                                else [Push Plus i]
                            Push Minus i@(I v) -> if elem v $ outputs f'
                                then mapMaybe
                                    (\case
                                        Push Plus x -> Just $ Push Minus x
                                        Push Minus x -> Just $ Push Plus x
                                        pull@(Pull _) -> deleteFromPull v pull
                                    ) lst'
                                else [Push Minus i]
                            Pull vs  -> [Pull vs]
                        ) lst
                    ) `asTypeOf` f
                    ]
            in
                DataflowGraphSubstitute{ oldSubGraph = [f', f], newSubGraph = Just newFList }
    | otherwise = DataflowGraphSubstitute{ oldSubGraph = [f', f], newSubGraph = Nothing }

refactorDfg dfg = if startFS == newFS
    then fsToDataFlowGraph $ newFS L.\\ funcsForDelete'
    else refactorDfg $ fsToDataFlowGraph newFS
    where
        startFS = dataFlowGraphToFs dfg
        newFS = refactorContainers $ createContainers startFS

        containerMapRefactored = trace ("CONTAINER MAP : " ++ (show $ containerMapCreate newFS)) (containerMapCreate newFS)

        funcsForDelete = findFuncForDelete newFS containerMapRefactored

        funcsForDelete' = trace ("FOR DELETE : " ++ show funcsForDelete) funcsForDelete

        findFuncForDelete [] _ = []
        findFuncForDelete (f:fs) outToFuncsMap =
            concatMap (\o ->
                case M.findWithDefault [] o outToFuncsMap of
                    [] -> []
                    lst -> f : lst
            ) (S.toList $ outputs f)
                ++ findFuncForDelete fs outToFuncsMap


