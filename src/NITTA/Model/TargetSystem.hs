{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.Model.TargetSystem
Description : Model of target system for synthesis and so on.
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.TargetSystem (
    TargetSystem (..),
    processDuration,
    isSynthesisComplete,
) where

import Control.Exception (assert)
import Data.Default
import Data.Set qualified as S
import GHC.Generics
import NITTA.Intermediate.DataFlow
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Utils

{- | Model of target unit, which is a main subject of synthesis process and
synthesis graph.
-}
data TargetSystem u tag v x t = TargetSystem
    { mUnit :: u
    -- ^ model of target unit
    , mDataFlowGraph :: DataFlowGraph v x
    -- ^ whole application algorithm
    }
    deriving (Generic)

instance Default u => Default (TargetSystem u tag v x t) where
    def = TargetSystem def def

instance WithFunctions u (F v x) => WithFunctions (TargetSystem u tag v x t) (F v x) where
    functions TargetSystem{mUnit, mDataFlowGraph} =
        assert (S.fromList (functions mUnit) == S.fromList (functions mDataFlowGraph)) $ -- inconsistent TargetSystem
            functions mUnit

processDuration TargetSystem{mUnit} = nextTick mUnit - 1

isSynthesisComplete :: ProcessorUnit u v x t => TargetSystem u tag v x t -> Bool
isSynthesisComplete TargetSystem{mUnit, mDataFlowGraph} =
    transferred mUnit == variables mDataFlowGraph

instance ProcessorUnit u v x t => ProcessorUnit (TargetSystem u tag v x t) v x t where
    tryBind f ts@TargetSystem{mUnit} = (\u -> ts{mUnit = u}) <$> tryBind f mUnit
    process TargetSystem{mUnit} = process mUnit
    parallelismType TargetSystem{mUnit} = parallelismType mUnit
    puSize TargetSystem{mUnit} = puSize mUnit

instance BindProblem u tag v x => BindProblem (TargetSystem u tag v x t) tag v x where
    bindOptions TargetSystem{mUnit} = bindOptions mUnit

    bindDecision ts@TargetSystem{mUnit} d = ts{mUnit = bindDecision mUnit d}

instance DataflowProblem u tag v t => DataflowProblem (TargetSystem u tag v x t) tag v t where
    dataflowOptions TargetSystem{mUnit} = dataflowOptions mUnit

    dataflowDecision f@TargetSystem{mUnit} d = f{mUnit = dataflowDecision mUnit d}

instance
    (Var v, Val x, BreakLoopProblem u v x) =>
    BreakLoopProblem (TargetSystem u tag v x t) v x
    where
    breakLoopOptions TargetSystem{mUnit} = breakLoopOptions mUnit

    breakLoopDecision TargetSystem{mUnit, mDataFlowGraph} d =
        TargetSystem
            { mDataFlowGraph = breakLoopDecision mDataFlowGraph d
            , mUnit = breakLoopDecision mUnit d
            }

instance
    (Var v, Val x, OptimizeAccumProblem u v x) =>
    OptimizeAccumProblem (TargetSystem u tag v x t) v x
    where
    optimizeAccumOptions TargetSystem{mUnit} = optimizeAccumOptions mUnit

    optimizeAccumDecision TargetSystem{mUnit, mDataFlowGraph} d =
        TargetSystem
            { mDataFlowGraph = optimizeAccumDecision mDataFlowGraph d
            , mUnit = optimizeAccumDecision mUnit d
            }

instance
    (Var v, Val x, OptimizeLutProblem u v x) =>
    OptimizeLutProblem (TargetSystem u tag v x t) v x
    where
    optimizeLutOptions TargetSystem{mUnit} = optimizeLutOptions mUnit

    optimizeLutDecision TargetSystem{mUnit, mDataFlowGraph} d =
        TargetSystem
            { mDataFlowGraph = optimizeLutDecision mDataFlowGraph d
            , mUnit = optimizeLutDecision mUnit d
            }

instance (Var v, Val x, ConstantFoldingProblem u v x) => ConstantFoldingProblem (TargetSystem u tag v x t) v x where
    constantFoldingOptions TargetSystem{mUnit} = constantFoldingOptions mUnit

    constantFoldingDecision TargetSystem{mUnit, mDataFlowGraph} d =
        TargetSystem
            { mDataFlowGraph = constantFoldingDecision mDataFlowGraph d
            , mUnit = constantFoldingDecision mUnit d
            }

instance (Var v, ResolveDeadlockProblem u v x) => ResolveDeadlockProblem (TargetSystem u tag v x t) v x where
    resolveDeadlockOptions TargetSystem{mUnit} = resolveDeadlockOptions mUnit

    resolveDeadlockDecision TargetSystem{mUnit, mDataFlowGraph} d =
        TargetSystem
            { mDataFlowGraph = resolveDeadlockDecision mDataFlowGraph d
            , mUnit = resolveDeadlockDecision mUnit d
            }

instance AllocationProblem u tag => AllocationProblem (TargetSystem u tag v x t) tag where
    allocationOptions TargetSystem{mUnit} = allocationOptions mUnit

    allocationDecision f@TargetSystem{mUnit} d = f{mUnit = allocationDecision mUnit d}
