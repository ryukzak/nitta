{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Model.Problems.Refactor
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Refactor
    ( RefactorOption(..), RefactorDecision(..), RefactorProblem(..)
    , refactorOption2decision
    ) where

import           Data.Proxy
import           GHC.Generics
import           NITTA.Intermediate.Functions
import           NITTA.Model.Problems.Types

data RefactorOption v x
    -- |Example:
    --
    -- >>> f1 :: (...) -> (a)
    -- f2 :: (a, ...) -> (...)
    -- f1 and f2 process on same mUnit
    -- In this case, we have deadlock, which can be fixed by insetion of register between functions:
    -- f1 :: (...) -> (a)
    -- reg :: a -> buf_a
    -- f2 :: (buf_a, ...) -> (...)
    = InsertOutRegisterO v
    -- |Example: l = Loop (X x) (O o) (I i) -> LoopIn l (I i), LoopOut (I o)
    | BreakLoopO (Loop v x) (LoopOut v x) (LoopIn v x)
    deriving ( Generic, Show, Eq )

data RefactorDecision v x
        = InsertOutRegisterD v v
        | BreakLoopD (Loop v x) (LoopOut v x) (LoopIn v x)
        deriving ( Generic, Show )

class RefactorProblem u v x | u -> v x where
  refactorOptions :: u -> [ RefactorOption v x ]
  refactorOptions _ = []
  refactorDecision :: u -> RefactorDecision v x -> u
  refactorDecision _ _ = error "not implemented"

refactorOption2decision (InsertOutRegisterO v) = InsertOutRegisterD v (v <> v)
refactorOption2decision (BreakLoopO origin src trg) = BreakLoopD origin src trg
