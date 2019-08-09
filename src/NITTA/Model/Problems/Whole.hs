{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
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
    ( SynthesisOption(..), SynthesisDecision(..), SynthesisProblem(..)
    , specializeDataFlowOption, isDataFlow, isBinding
    , option2decision
    ) where

import           Control.Arrow                    (second)
import qualified Data.Map                         as M
import           Data.Maybe
import           Data.Proxy
import           GHC.Generics
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Problems.Binding
import           NITTA.Model.Problems.Refactor
import           NITTA.Model.Problems.Transport
import           NITTA.Model.Problems.Types
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.TargetSystem         (ModelState (..))
import           NITTA.Model.Types
import           NITTA.Utils.Lens
import           Numeric.Interval                 (Interval, (...))


isBinding = \case BindingOption{} -> True; _ -> False
isDataFlow = \case DataFlowOption{} -> True; _ -> False

specializeDataFlowOption (DataFlowOption s t) = DataFlowO s t
specializeDataFlowOption _ = error "Can't specialize non Model option!"

generalizeDataFlowOption (DataFlowO s t) = DataFlowOption s t
generalizeBindingOption (BindingO s t) = BindingOption s t



data SynthesisOption tag v x t
    = BindingOption (F v x) tag
    | DataFlowOption (Source tag (TimeConstrain t)) (Target tag v (TimeConstrain t))
    | RefactorOption (Refactor v x)
    deriving ( Generic, Show )

data SynthesisDecision tag v x t
    = BindingDecision (F v x) tag
    | DataFlowDecision (Source tag (Interval t)) (Target tag v (Interval t))
    | RefactorDecision (Refactor v x)
    deriving ( Generic, Show )


class SynthesisProblem u tag v x t | u -> tag v x t where
    synthesisOptions :: u -> [ SynthesisOption tag v x t ]
    synthesisDecision :: u -> SynthesisDecision tag v x t -> u


instance ( UnitTag tag, VarValTime v x t, Semigroup v
         ) => SynthesisProblem (ModelState (BusNetwork tag v x t) v x) tag v x t where
    synthesisOptions m@ModelState{ mUnit } = let
            binds = map generalizeBindingOption $ options binding m
            transfers = map generalizeDataFlowOption $ options dataFlowDT mUnit
            refactors = map RefactorOption $ refactorOptions m
        in concat [ binds, transfers, refactors ]

    synthesisDecision fr (BindingDecision f tag) = decision binding fr $ BindingD f tag
    synthesisDecision fr@ModelState{ mUnit } (DataFlowDecision src trg) = fr{ mUnit=decision dataFlowDT mUnit $ DataFlowD src trg }
    synthesisDecision m (RefactorDecision d) = refactorDecision m d


-- |The simplest way to convert 'Option SynthesisDT' to 'Decision SynthesisDT'.
option2decision (BindingOption f tag) = BindingDecision f tag
option2decision (DataFlowOption src trg)
    = let
        pushTimeConstrains = map snd $ catMaybes $ M.elems trg
        pullStart    = maximum $ (snd src^.avail.infimum) : map (\o -> o^.avail.infimum) pushTimeConstrains
        pullDuration = maximum $ map (\o -> o^.dur.infimum) $ snd src : pushTimeConstrains
        pullEnd = pullStart + pullDuration - 1
        pushStart = pullStart
        mkEvent (from_, tc) = Just (from_, pushStart ... (pushStart + tc^.dur.infimum - 1))
        pushs = map (second $ maybe Nothing mkEvent) $ M.assocs trg
    in DataFlowDecision ( fst src, pullStart ... pullEnd ) $ M.fromList pushs
option2decision (RefactorOption o) = RefactorDecision o
