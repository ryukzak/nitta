{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
    isSynthesisFinish,
) where

import Control.Exception (assert)
import qualified Data.Set as S
import GHC.Generics
import NITTA.Intermediate.DataFlow
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Utils

{- |Model of target unit, which is a main subject of synthesis process and
synthesis graph.
-}
data TargetSystem u tag v x t = TargetSystem
    { -- |model of target unit
      mUnit :: u
    , -- |whole application algorithm
      mDataFlowGraph :: DataFlowGraph v x
    }
    deriving (Generic)

instance (WithFunctions u (F v x)) => WithFunctions (TargetSystem u tag v x t) (F v x) where
    functions TargetSystem{mUnit, mDataFlowGraph} =
        assert (S.fromList (functions mUnit) == S.fromList (functions mDataFlowGraph)) $ -- inconsistent TargetSystem
            functions mUnit

processDuration TargetSystem{mUnit} = nextTick $ process mUnit

{- |Synthesis process is finish when all variable from data flow are
transferred.
-}
isSynthesisFinish :: (ProcessorUnit u v x t) => TargetSystem u tag v x t -> Bool
isSynthesisFinish TargetSystem{mUnit, mDataFlowGraph} =
    transferred mUnit == variables mDataFlowGraph

instance (BindProblem u tag v x) => BindProblem (TargetSystem u tag v x t) tag v x where
    bindOptions TargetSystem{mUnit} = bindOptions mUnit

    bindDecision f@TargetSystem{mUnit} d = f{mUnit = bindDecision mUnit d}

instance (DataflowProblem u tag v t) => DataflowProblem (TargetSystem u tag v x t) tag v t where
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

instance (VarValTime v x t) => ConstantFoldingProblem (TargetSystem (BusNetwork tag v x t) v x) v x where
    compileTimeEvalOptions TargetSystem{mUnit} = compileTimeEvalOptions mUnit

    compileTimeEvalDecision TargetSystem{mUnit, mDataFlowGraph} d =
        TargetSystem
            { mDataFlowGraph = compileTimeEvalDecision mDataFlowGraph d
            , mUnit = compileTimeEvalDecision mUnit d
            }

instance (UnitTag tag, VarValTime v x t) => ResolveDeadlockProblem (TargetSystem (BusNetwork tag v x t) v x) v x where
    resolveDeadlockOptions TargetSystem{mUnit} = resolveDeadlockOptions mUnit

    resolveDeadlockDecision TargetSystem{mUnit, mDataFlowGraph} d =
        TargetSystem
            { mDataFlowGraph = resolveDeadlockDecision mDataFlowGraph d
            , mUnit = resolveDeadlockDecision mUnit d
            }
