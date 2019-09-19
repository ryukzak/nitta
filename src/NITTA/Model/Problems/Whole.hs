{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Model.Problems.Whole
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Whole
    ( SynthesisStatement(..), SynthesisProblem(..)
    , specializeDataFlowOption, isDataFlow, isBinding
    , option2decision
    ) where

import           Control.Arrow                    (second)
import qualified Data.Map                         as M
import           Data.Maybe
import           GHC.Generics
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Problems.Binding
import           NITTA.Model.Problems.Dataflow
import           NITTA.Model.Problems.Refactor
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.TargetSystem         (ModelState (..))
import           NITTA.Model.Types
import           NITTA.Utils.Lens
import           Numeric.Interval                 (Interval, (...))


isBinding = \case Binding{} -> True; _ -> False
isDataFlow = \case Dataflow{} -> True; _ -> False

specializeDataFlowOption (Dataflow s t) = DataFlowO s t
specializeDataFlowOption _ = error "Can't specialize non Model option!"

generalizeDataFlowOption (DataFlowO s t) = Dataflow s t
generalizeBindingOption (Bind s t) = Binding s t



data SynthesisStatement tag v x tp
    = Binding (F v x) tag
    | Dataflow
        { dfSource  :: (tag, tp)
        , dfTargets :: M.Map v (Maybe (tag, tp))
        }
    | Refactor (Refactor v x)
    deriving ( Generic, Show )


class SynthesisProblem u tag v x t | u -> tag v x t where
    synthesisOptions :: u -> [ SynthesisStatement tag v x (TimeConstrain t) ]
    synthesisDecision :: u -> SynthesisStatement tag v x (Interval t) -> u


instance ( UnitTag tag, VarValTime v x t
         ) => SynthesisProblem (ModelState (BusNetwork tag v x t) v x) tag v x t where
    synthesisOptions m@ModelState{ mUnit } = concat
        [ map generalizeBindingOption $ bindOptions m
        , map generalizeDataFlowOption $ dataflowOptions mUnit
        , map Refactor $ refactorOptions m
        ]

    synthesisDecision m (Binding f tag) = bindDecision m $ Bind f tag
    synthesisDecision m@ModelState{ mUnit } (Dataflow src trg) = m{ mUnit=dataflowDecision mUnit $ DataFlowD src trg }
    synthesisDecision m (Refactor d) = refactorDecision m d


-- |The simplest way to convert 'Option SynthesisDT' to 'Decision SynthesisDT'.
option2decision (Binding f tag) = Binding f tag
option2decision (Dataflow src trg)
    = let
        pushTimeConstrains = map snd $ catMaybes $ M.elems trg
        pullStart    = maximum $ (snd src^.avail.infimum) : map (\o -> o^.avail.infimum) pushTimeConstrains
        pullDuration = maximum $ map (\o -> o^.dur.infimum) $ snd src : pushTimeConstrains
        pullEnd = pullStart + pullDuration - 1
        pushStart = pullStart
        mkEvent (from_, tc) = Just (from_, pushStart ... (pushStart + tc^.dur.infimum - 1))
        pushs = map (second $ maybe Nothing mkEvent) $ M.assocs trg
    in Dataflow ( fst src, pullStart ... pullEnd ) $ M.fromList pushs
option2decision (Refactor o) = Refactor o
