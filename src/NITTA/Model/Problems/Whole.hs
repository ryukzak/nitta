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
    ( SynthesisDT, Option(..), Decision(..), synthesisOptions, synthesisDecision
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


-- |Decision type of whole system synthesis process.
data SynthesisDT u
synthesisOptions m = options (Proxy :: Proxy SynthesisDT) m
synthesisDecision m d = decision (Proxy :: Proxy SynthesisDT) m d

isBinding = \case BindingOption{} -> True; _ -> False
isDataFlow = \case DataFlowOption{} -> True; _ -> False

specializeDataFlowOption (DataFlowOption s t) = DataFlowO s t
specializeDataFlowOption _ = error "Can't specialize non Model option!"

generalizeDataFlowOption (DataFlowO s t) = DataFlowOption s t
generalizeBindingOption (BindingO s t) = BindingOption s t


instance DecisionType (SynthesisDT (BusNetwork tag v x t)) where
    data Option (SynthesisDT (BusNetwork tag v x t))
        = BindingOption (F v x) tag
        | DataFlowOption (Source tag (TimeConstrain t)) (Target tag v (TimeConstrain t))
        | RefactorOption (Option (RefactorDT v x))
        deriving ( Generic, Show )

    data Decision (SynthesisDT (BusNetwork tag v x t))
        = BindingDecision (F v x) tag
        | DataFlowDecision (Source tag (Interval t)) (Target tag v (Interval t))
        | RefactorDecision (Decision (RefactorDT v x))
        deriving ( Generic, Show )


instance ( UnitTag tag, VarValTime v x t
         ) => DecisionProblem (SynthesisDT (BusNetwork tag v x t))
                  SynthesisDT (ModelState (BusNetwork tag v x t) v x)
        where
    options _ m@ModelState{ mUnit }
        = let
            binds = map generalizeBindingOption $ options binding m
            transfers = map generalizeDataFlowOption $ options dataFlowDT mUnit
            refactors = map RefactorOption $ refactorOptions m
        in concat [ binds, transfers, refactors ]

    decision _ fr (BindingDecision f tag) = decision binding fr $ BindingD f tag
    decision _ fr@ModelState{ mUnit } (DataFlowDecision src trg) = fr{ mUnit=decision dataFlowDT mUnit $ DataFlowD src trg }
    decision _ m (RefactorDecision d) = refactorDecision m d


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
option2decision (RefactorOption o) = RefactorDecision $ refactorOption2decision o
