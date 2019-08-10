{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Model.TargetSystem
Description : Model of target system for synthesis and so on.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.TargetSystem
    ( ModelState(..)
    , DataFlowGraph(..), fsToDataFlowGraph
    ) where

import           Control.Exception                (assert)
import qualified Data.List                        as L
import qualified Data.Set                         as S
import           GHC.Generics
import           NITTA.Intermediate.Functions     (reg)
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Problems.Binding
import           NITTA.Model.Problems.Dataflow
import           NITTA.Model.Problems.Refactor
import           NITTA.Model.ProcessorUnits.Types
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

instance ( UnitTag tag, VarValTime v x t, Semigroup v
        ) => RefactorProblem (ModelState (BusNetwork tag v x t) v x) v x where
    refactorOptions ModelState{ mUnit } = refactorOptions mUnit

    refactorDecision ModelState{ mUnit, mDataFlowGraph } d@(InsertOutRegister v v')
        = ModelState
            { mDataFlowGraph=patch (v, v') mDataFlowGraph
            , mUnit=refactorDecision mUnit d
            }
    refactorDecision ModelState{ mUnit, mDataFlowGraph=DFCluster leafs } d@(BreakLoop l i o) = let
            revokeLoop = leafs L.\\ [ DFLeaf $ F l ]
            addLoopParts = [ DFLeaf $ F i, DFLeaf $ F o ] ++ revokeLoop
        in ModelState
            { mDataFlowGraph=DFCluster $ addLoopParts
            , mUnit=refactorDecision mUnit d
            }
    refactorDecision _ _ = undefined


-- |Data flow graph - intermediate representation of application algorithm.
-- Right now can be replaced by @[F v x]@, but for future features like
-- conduction statement, we don't do that.
data DataFlowGraph v x
    = DFLeaf (F v x)
    | DFCluster [ DataFlowGraph v x ]
    deriving ( Show, Generic, Eq )

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


-- |Convert @[ F v x ]@ to 'DataFlowGraph'.
fsToDataFlowGraph alg = DFCluster $ map DFLeaf alg
