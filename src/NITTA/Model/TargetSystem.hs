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
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           GHC.Generics
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

    refactorDecision (ModelState _ _) (AlgSub _) = error "not implemented"

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
    refactorOptions dfg = result
       where
         result = [AlgSub $ fromHistoryTree refactored]
         refactored = refactorHfs $ toHistoryTree $ dataFlowGraphToFs dfg

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
filterAddSub (hf:hfs)
    | Just Add{} <- castF $ getF hf = Just hf : filterAddSub hfs
    | Just Sub{} <- castF $ getF hf = Just hf : filterAddSub hfs
    | Just Acc{} <- castF $ getF hf = Just hf : filterAddSub hfs
    | otherwise             = Nothing : filterAddSub hfs


-- |Check intersections between input and output, and create container
toOneContainer hfs hfs'
    | not $ null $ S.intersection
        (foldl1 S.union (map inputs fs))
        (foldl1 S.union (map outputs fs')) = S.fromList $ hfs ++ hfs'
    | not $ null $ S.intersection
        (foldl1 S.union (map inputs fs'))
        (foldl1 S.union (map outputs fs)) = S.fromList $ hfs ++ hfs'
    | otherwise                           = S.fromList hfs
        where
            fs = getFS hfs
            fs' = getFS hfs'


createContainers hfs
    | (length filtered) == 1 = containered
    | otherwise              = map S.toList listOfSets
    where
        listOfSets = S.toList $ S.fromList [toOneContainer hfs1 hfs2 | hfs1 <- containered , hfs2 <- containered, hfs1 /= hfs2]
        filtered = catMaybes $ filterAddSub hfs
        containered = map (\x -> [x]) filtered
        -- TODO: ADD HERE DIVIDING SUM https://nitta.io/nitta-corp/nitta/-/issues/75

refactorContainers containers = L.nub $ concatMap refactorContainer containers


-- |Create Map String (HistoryTree (F v x)), where Key is input label and Value is FU that contain this input label
containerMapCreate hfs = M.unions $
    map
    (\hf ->
       foldl
       (\dataMap k ->
          M.insertWith (++) k [hf] dataMap
       ) M.empty (S.toList $ inputs $ getF hf)
    ) $ hfs

-- |Takes container and refactor it, if it can be
refactorContainer [hf] = [hf]
refactorContainer hfs = concatMap refactored hfs
    where
        containerMap = containerMapCreate hfs

        refactored hf = concatMap
            (\o ->
                case M.findWithDefault [] o containerMap of
                    []         -> []
                    matchedFUs -> map (refactorFunction hf) matchedFUs
            ) (S.toList $ outputs $ getF hf)

deleteFromPull v (Pull (O s))
    | deleted == S.empty = Nothing
    | otherwise          = Just $ Pull $ O $ deleted
        where
            deleted = S.delete v s

deleteFromPull _ (Push _ _) = error "delete only Pull"

refactorFunction hf' hf
    | Just (Acc lst') <- castF $ getF hf'
    , Just (Acc lst ) <- castF $ getF hf
    , let
        multipleOutBool = (1 <) $ length $ outputs $ getF hf'
        isOutInpIntersect = any
            (\case
                Push _ (I v) -> elem v $ outputs $ getF hf'
                _            -> False
            ) lst
        makeRefactor = not multipleOutBool && isOutInpIntersect

    in
        makeRefactor = let
                newFS =
                    packF
                        ( Acc $ concatMap
                            (\case
                                Push Plus i@(I v) -> if elem v $ outputs $ getF hf'
                                    then mapMaybe (\case
                                                    pull@(Pull _) -> deleteFromPull v pull;
                                                    inp -> Just inp
                                            ) lst'
                                    else [Push Plus i]
                                Push Minus i@(I v) -> if elem v $ outputs $ getF hf'
                                    then mapMaybe
                                        (\case
                                            Push Plus x -> Just $ Push Minus x
                                            Push Minus x -> Just $ Push Plus x
                                            pull@(Pull _) -> deleteFromPull v pull
                                        ) lst'
                                    else [Push Minus i]
                                Pull vs  -> [Pull vs]
                            ) lst
                        ) `asTypeOf` (getF hf)
            in
                RefactoredFunc newFS [hf', hf]
    | otherwise = hf

data HistoryTree f = JustFunc f | RefactoredFunc f [HistoryTree f] deriving (Show, Eq, Ord)

toHistoryTree fs = map JustFunc fs

fromHistoryTree []                             = []
fromHistoryTree (JustFunc f : lstTail)         = f : fromHistoryTree lstTail
fromHistoryTree (RefactoredFunc f _ : lstTail) = f : fromHistoryTree lstTail

getF (JustFunc f)         = f
getF (RefactoredFunc f _) = f

getFS hfs = map getF hfs

refactorHfs hfs
    | startFS == newFS = newFS L.\\ funcsForDelete
    | otherwise        = refactorHfs newFS
    where
        startFS = hfs
        newFS = refactorContainers $ createContainers startFS

        containerMapRefactored = containerMapCreate newFS

        funcsForDelete = concatMap (findFuncForDelete containerMapRefactored) newFS


        findFuncForDelete outToFuncsMap hf = concatMap
            (\o ->
                case M.findWithDefault [] o outToFuncsMap of
                    []  -> []
                    lst -> hf : lst
            ) (S.toList $ outputs $ getF hf)


