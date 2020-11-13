{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : NITTA.Model.TargetSystem
Description : Model of target system for synthesis and so on.
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.TargetSystem
    ( TargetSystem(..)
    , processDuration, isSynthesisFinish
    ) where

import           Control.Exception ( assert )
import qualified Data.Set as S
import           GHC.Generics
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Problems
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.Types
import           NITTA.Utils


-- |Model of target unit, which is a main subject of synthesis process and
-- synthesis graph.
data TargetSystem u v x
    = TargetSystem
        { mUnit          :: u -- ^model of target unit
        , mDataFlowGraph :: DataFlowGraph v x -- ^whole application algorithm
        }
    deriving ( Generic )

instance WithFunctions (TargetSystem (BusNetwork tag v x t) v x) (F v x) where
    functions TargetSystem{ mUnit, mDataFlowGraph }
        = assert (S.fromList (functions mUnit) == S.fromList (functions mDataFlowGraph)) -- inconsistent TargetSystem
            $ functions mUnit


processDuration TargetSystem{ mUnit } = nextTick $ process mUnit


-- |Synthesis process is finish when all variable from data flow are
-- transferred.
isSynthesisFinish :: ( ProcessorUnit u v x t ) => TargetSystem u v x -> Bool
isSynthesisFinish TargetSystem{ mUnit, mDataFlowGraph } = let
        inWork = transferred mUnit
        inAlg = variables mDataFlowGraph
    in inWork == inAlg



instance ( UnitTag tag, VarValTime v x t
        ) => BindProblem (TargetSystem (BusNetwork tag v x t) v x) tag v x where
    bindOptions TargetSystem{ mUnit }      = bindOptions mUnit
    bindDecision f@TargetSystem{ mUnit } d = f{ mUnit=bindDecision mUnit d }


instance ( UnitTag tag, VarValTime v x t
        ) => DataflowProblem (TargetSystem (BusNetwork tag v x t) v x) tag v t
        where
    dataflowOptions TargetSystem{ mUnit }      = dataflowOptions mUnit
    dataflowDecision f@TargetSystem{ mUnit } d = f{ mUnit=dataflowDecision mUnit d }


instance ( UnitTag tag, VarValTime v x t
        ) => RefactorProblem (TargetSystem (BusNetwork tag v x t) v x) v x where
    refactorOptions TargetSystem{ mUnit } = refactorOptions mUnit

    refactorDecision TargetSystem{ mUnit, mDataFlowGraph } d
        = TargetSystem
            { mDataFlowGraph=refactorDecision mDataFlowGraph d
            , mUnit=refactorDecision mUnit d
            }


instance ( UnitTag tag, VarValTime v x t
         ) => SynthesisProblem (TargetSystem (BusNetwork tag v x t) v x) tag v x t where
    synthesisOptions m@TargetSystem{ mUnit } = concat
        [ map generalizeBinding $ bindOptions m
        , map generalizeDataflow $ dataflowOptions mUnit
        , map Refactor $ refactorOptions m
        ]

    synthesisDecision m (Binding f tag) = bindDecision m $ Bind f tag
    synthesisDecision m@TargetSystem{ mUnit } (Dataflow src trg) = m{ mUnit=dataflowDecision mUnit $ DataflowSt src trg }
    synthesisDecision m (Refactor d) = refactorDecision m d
