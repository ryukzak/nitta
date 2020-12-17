{- FOURMOLU_DISABLE -}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : NITTA.Model.Problems.Refactor.Types
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Refactor.Types
    ( Refactor(..), RefactorProblem(..)
    ) where

import qualified Data.Set as S
import           GHC.Generics
import           NITTA.Intermediate.Types


data Refactor v x
    = ResolveDeadlock (S.Set v)
      -- ^ResolveDeadlock example:
      --
      -- > ResolveDeadlock [a, b]
      --
      -- before:
      --
      -- > f1 :: (...) -> ([a, b])
      -- > f2 :: (a, ...) -> (...)
      -- > f3 :: (b, ...) -> (...)
      --
      -- f1, f2 and f3 process on same process unit. In this case, we have
      -- deadlock, which can be fixed by insertion of buffer register between
      -- functions.
      --
      -- after:
      --
      -- > f1 :: (...) -> ([a@buf])
      -- > reg :: a@buf -> ([a, b])
      -- > f2 :: (a, ...) -> (...)
      -- > f3 :: (b, ...) -> (...)
    | BreakLoop
      -- ^BreakLoop example:
      --
      -- > BreakLoop x o i
      --
      -- before:
      --
      -- > l@( Loop (X x) (O o) (I i) )
      --
      -- after:
      --
      -- > LoopIn l (I i)
      -- > LoopOut l (O o)
        { loopX :: x       -- ^initial looped value
        , loopO :: S.Set v -- ^output variables
        , loopI :: v       -- ^input variable
        }
    | OptimizeAccum
        { refOld :: [ F v x ]
        , refNew :: [ F v x ]
        }
      -- ^OptimizeAccum example:
      --
      -- > OptimizeAccum [+a +tmp_1 => d; +b +c => tmp_1] [+a +b +c => d]
      --
      -- before:
      --
      -- > [+a +tmp_1 => d; +b +c => tmp_1]
      --
      -- after:
      --
      -- > [+a +b +c => d]
    deriving ( Generic, Show, Eq )


class RefactorProblem u v x | u -> v x where
    refactorOptions :: u -> [ Refactor v x ]
    refactorOptions _ = []

    refactorDecision :: u -> Refactor v x -> u
    refactorDecision _ _ = error "not implemented"
