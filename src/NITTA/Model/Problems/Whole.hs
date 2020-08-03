{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : NITTA.Model.Problems.Whole
Description : A whole system synthesis options and decisions
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Whole
    ( SynthesisStatement(..), SynthesisProblem(..)
    , isDataFlow, isBinding
    , specializeDataflow, generalizeDataflow, generalizeBinding
    , option2decision
    ) where

import qualified Data.Map                      as M
import           GHC.Generics
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Binding
import           NITTA.Model.Problems.Dataflow
import           NITTA.Model.Problems.Refactor
import           NITTA.Model.Types
import           Numeric.Interval


isBinding = \case Binding{} -> True; _ -> False
isDataFlow = \case Dataflow{} -> True; _ -> False

specializeDataflow (Dataflow s t) = DataflowSt s t
specializeDataflow _              = error "Can't specialize non Model option!"

generalizeDataflow (DataflowSt s t) = Dataflow s t
generalizeBinding (Bind s t) = Binding s t


-- |It is a wrapper over another low-level synthesis problem, which allows
-- controlling over the whole synthesis process from one place (synthesis method
-- or user interface).
--
-- - option if @tp ~ TimeConstrain t@;
-- - decision if @tp ~ Interval t@.
data SynthesisStatement tag v x tp
    = Binding (F v x) tag -- ^see: "NITTA.Model.Problems.Binding"
    | Dataflow -- ^see: "NITTA.Model.Problems.Dataflow"
        { dfSource  :: (tag, tp)
        , dfTargets :: M.Map v (Maybe (tag, tp))
        }
    | Refactor (Refactor v x) -- ^see: "NITTA.Model.Problems.Refactor"
    deriving ( Generic, Show, Eq )


class SynthesisProblem u tag v x t | u -> tag v x t where
    synthesisOptions :: u -> [ SynthesisStatement tag v x (TimeConstrain t) ]
    synthesisDecision :: u -> SynthesisStatement tag v x (Interval t) -> u


-- |The simplest way to convert 'synthesisOptions' to 'synthesisDecision'.
option2decision
    :: ( Var v, Time t )
    => SynthesisStatement tag v x (TimeConstrain t)
    -> SynthesisStatement tag v x (Interval t)
option2decision (Binding f tag) = Binding f tag
option2decision df@Dataflow{} = generalizeDataflow $ dataflowOption2decision $ specializeDataflow df
option2decision (Refactor o) = Refactor o
