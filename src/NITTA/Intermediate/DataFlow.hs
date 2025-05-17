{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.Intermediate.DataFlow
Description : DataFlow graph
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Intermediate.DataFlow (
    DataFlowGraph (..),
    fsToDataFlowGraph,
    addFuncToDataFlowGraph,
) where

import Data.Default
import Data.List qualified as L
import Data.Set qualified as S
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Problems.Refactor
import NITTA.Utils.Base

{- | Data flow graph - intermediate representation of application algorithm.
Right now can be replaced by @[F v x]@, but for future features like
conduction statement, we don't do that.
-}
data DataFlowGraph v x
    = DFLeaf (F v x)
    | DFCluster [DataFlowGraph v x]
    deriving (Show, Generic)

instance Default (DataFlowGraph v x) where
    def = DFCluster []

instance Eq (DataFlowGraph v x) where
    -- `show` used for avoid `Ord (DataFlowGraph v x)`
    (DFCluster c1) == (DFCluster c2) = S.fromList (map show c1) == S.fromList (map show c2)
    (DFLeaf f1) == (DFLeaf f2) = f1 == f2
    _ == _ = False

instance Var v => Variables (DataFlowGraph v x) v where
    variables (DFLeaf fb) = variables fb
    variables (DFCluster g) = unionsMap variables g

instance WithFunctions (DataFlowGraph v x) (F v x) where
    functions (DFLeaf f) = [f]
    functions (DFCluster g) = concatMap functions g

instance (Var v, Val x) => BreakLoopProblem (DataFlowGraph v x) v x where
    breakLoopOptions _dfg = []

    breakLoopDecision dfg bl =
        let origin = recLoop bl
         in fsToDataFlowGraph $
                (recLoopIn bl){funHistory = [origin]}
                    : (recLoopOut bl){funHistory = [origin]}
                    : (functions dfg L.\\ [origin])

instance (Var v, Val x) => ConstantFoldingProblem (DataFlowGraph v x) v x where
    constantFoldingOptions _dfg = []

    constantFoldingDecision dfg ref@ConstantFolding{} =
        fsToDataFlowGraph $ constantFoldingDecision (functions dfg) ref

instance (Var v, Val x) => OptimizeAccumProblem (DataFlowGraph v x) v x where
    optimizeAccumOptions _dfg = []

    optimizeAccumDecision dfg ref@OptimizeAccum{} =
        fsToDataFlowGraph $ optimizeAccumDecision (functions dfg) ref

instance (Var v, Val x) => OptimizeLutProblem (DataFlowGraph v x) v x where
    optimizeLutOptions _dfg = []

    optimizeLutDecision dfg ref@OptimizeLut{} =
        fsToDataFlowGraph $ optimizeLutDecision (functions dfg) ref

instance Var v => ResolveDeadlockProblem (DataFlowGraph v x) v x where
    resolveDeadlockOptions _dfg = []

    resolveDeadlockDecision dfg ResolveDeadlock{newBuffer, changeset} =
        fsToDataFlowGraph (newBuffer : map (patch changeset) (functions dfg))

-- | Convert @[ F v x ]@ to 'DataFlowGraph'.
fsToDataFlowGraph fs = DFCluster $ map DFLeaf fs

addFuncToDataFlowGraph f (DFCluster items) = DFCluster (DFLeaf f : items)
addFuncToDataFlowGraph f leaf = DFCluster [DFLeaf f, leaf]
