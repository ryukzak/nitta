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
Module      : NITTA.Model.Problems.Refactor
Description : Automatic manipulation over an intermediate representation
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

Some times, CAD can not synthesis a target system because of a features of an
algorithm and microarchitecture (too less process units, too many functions, too
complicated algorithm).

In this case user can manually add some tweaks to the algorithm, but for that he
should be an expert with deep understanding of NITTA project. Of course, it is
not acceptable. This module defines type of that tweaks.
-}
module NITTA.Model.Problems.Refactor
    ( Refactor(..), RefactorProblem(..)
    , recLoop, recLoopOut, recLoopIn
    , prepareBuffer
    , maxBufferStack
    ) where

import           Data.Default
import qualified Data.Map as M
import qualified Data.Set as S
import           GHC.Generics
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           NITTA.Utils.Base


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
        { refOld :: [F v x]
        , refNew :: [F v x]
        }
      -- ^AlgSub example:
      --
      -- > AlgSub [+a +b +c => d]
      --
      -- before:
      --
      -- > [+a +tmp_1 => d; +b +c => tmp_1]
      --
      -- after:
      --
      -- > [+a +b +c => d]

    deriving ( Generic, Show, Eq )

recLoop BreakLoop{ loopX, loopO, loopI }
    = packF $ Loop (X loopX) (O loopO) (I loopI)
recLoop _ = error "applicable only for BreakLoop"

recLoopIn BreakLoop{ loopX, loopO, loopI }
    = packF $ LoopIn (Loop (X loopX) (O loopO) (I loopI)) (I loopI)
recLoopIn _ = error "applicable only for BreakLoop"

recLoopOut BreakLoop{ loopX, loopO, loopI }
    = packF $ LoopOut (Loop (X loopX) (O loopO) (I loopI)) (O loopO)
recLoopOut _ = error "applicable only for BreakLoop"


class RefactorProblem u v x | u -> v x where
    refactorOptions :: u -> [ Refactor v x ]
    refactorOptions _ = []

    refactorDecision :: u -> Refactor v x -> u
    refactorDecision _ _ = error "not implemented"


prepareBuffer :: ( Var v, Val x ) => Refactor v x -> ( F v x, Changeset v )
prepareBuffer (ResolveDeadlock vs) = let
        bufferI = bufferSuffix $ oneOf vs
        bufferO = S.elems vs
        diff = def{ changeO=M.fromList $ map (\o -> (o, S.singleton bufferI)) bufferO }
    in ( reg bufferI bufferO, diff )

prepareBuffer _ = undefined


-- |The constant, which restrict maximum length of a buffer sequence.
maxBufferStack = 2 :: Int
