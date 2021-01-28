{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Model.Problems.Refactor.ResolveDeadlock
Description : Refactoring for resolving deadlocks
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

Possible deadlocks (recognized in 'NITTA.Model.Networks.Bus'):

- selfsending;

    @
                      source1
      DoubleLoopOut--b1--+--b2--> DoubleLoopIn
                 |       \------> Send
                 |
                 \------b3------> DoubleLoopIn
                      source2

    Possible buffers: b1, b2, b3
    @
- classical deadlock betweeb two function on same PU.

    @
      a + b = c ---+----> c + d = e ---> e * c = f
                   |                         ^
                   \-------------b1----------/
    @

ResolveDeadlock example:

> ResolveDeadlock [a, b]

before:

> f1 :: (...) -> ([a, b])
> f2 :: (a, ...) -> (...)
> f3 :: (b, ...) -> (...)

f1, f2 and f3 process on same process unit. In this case, we have deadlock,
which can be fixed by insertion of buffer register between functions.

after:

> f1 :: (...) -> ([a@buf])
> reg :: a@buf -> ([a, b])
> f2 :: (a, ...) -> (...)
> f3 :: (b, ...) -> (...)
-}
module NITTA.Model.Problems.Refactor.ResolveDeadlock (
    ResolveDeadlock (..),
    ResolveDeadlockProblem (..),
    resolveDeadlock,
    maxBufferStack,
) where

import Data.Default
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Generics
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.Utils.Base

data ResolveDeadlock v x = ResolveDeadlock
    { buffer :: F v x
    , changeset :: Changeset v
    }
    deriving (Generic, Show, Eq)

class ResolveDeadlockProblem u v x | u -> v x where
    resolveDeadlockOptions :: u -> [ResolveDeadlock v x]
    resolveDeadlockOptions _ = []

    resolveDeadlockDecision :: u -> ResolveDeadlock v x -> u
    resolveDeadlockDecision _ _ = error "not supported"

resolveDeadlock :: (Var v, Val x) => S.Set v -> ResolveDeadlock v x
resolveDeadlock buffered =
    let bufferI = bufferSuffix $ oneOf buffered
        bufferO = S.elems buffered
        diff = def{changeO = M.fromList $ map (\o -> (o, S.singleton bufferI)) bufferO}
     in ResolveDeadlock
            { buffer = reg bufferI bufferO
            , changeset = diff
            }

-- |The constant, which restrict maximum length of a buffer sequence.
maxBufferStack = 2 :: Int
