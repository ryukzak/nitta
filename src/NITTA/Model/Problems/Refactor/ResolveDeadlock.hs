{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Model.Problems.Refactor.ResolveDeadlock
Description : Refactoring for resolving deadlocks
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

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
    prepareBuffer,
    maxBufferStack,
) where

import Data.Default
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Generics
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.Utils.Base

data ResolveDeadlock v = ResolveDeadlock
    { bufferOut :: S.Set v
    }
    deriving (Generic, Show, Eq)

class ResolveDeadlockProblem u v x | u -> v x where
    resolveDeadlockOptions :: u -> [ResolveDeadlock v]
    resolveDeadlockOptions _ = []

    resolveDeadlockDecision :: u -> ResolveDeadlock v -> u
    resolveDeadlockDecision _ _ = error "not supported"

prepareBuffer :: (Var v, Val x) => ResolveDeadlock v -> (F v x, Changeset v)
prepareBuffer ResolveDeadlock{bufferOut} =
    let bufferI = bufferSuffix $ oneOf bufferOut
        bufferO = S.elems bufferOut
        diff = def{changeO = M.fromList $ map (\o -> (o, S.singleton bufferI)) bufferO}
     in (reg bufferI bufferO, diff)

-- |The constant, which restrict maximum length of a buffer sequence.
maxBufferStack = 2 :: Int
