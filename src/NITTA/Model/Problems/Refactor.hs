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
    ( Refactor(..), RefactorProblem(..)
    , recLoop, recLoopOut, recLoopIn
    , prepareBuffer
    , maxBufferStack
    ) where

import           Data.Default
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import           GHC.Generics
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           NITTA.Utils


data Refactor v x
    = ResolveDeadlock (S.Set v)
    -- |Example: l = Loop (X x) (O o) (I i) -> LoopIn l (I i), LoopOut (I o)
    | BreakLoop{ loopX :: x, loopO :: S.Set v, loopI :: v } -- (Loop v x) (LoopOut v x) (LoopIn v x)
    deriving ( Generic, Show, Eq )


recLoop BreakLoop{ loopX, loopO, loopI } = Loop (X loopX) (O loopO) (I loopI)
recLoop _ = error "applicable only for BreakLoop"
recLoopIn bl@BreakLoop{ loopI } = LoopIn (recLoop bl) (I loopI)
recLoopIn _                     = error "applicable only for BreakLoop"
recLoopOut bl@BreakLoop{ loopO } = LoopOut (recLoop bl) (O loopO)
recLoopOut _                     = error "applicable only for BreakLoop"


class RefactorProblem u v x | u -> v x where
  refactorOptions :: u -> [ Refactor v x ]
  refactorOptions _ = []
  refactorDecision :: u -> Refactor v x -> u
  refactorDecision _ _ = error "not implemented"


prepareBuffer (ResolveDeadlock vs) = let
        bufferI = bufferSuffix $ oneOf vs
        bufferO = S.elems vs
        diff = def{ diffO=M.fromList $ map (\o -> (o, S.singleton bufferI)) bufferO }
    in ( reg bufferI bufferO, diff )

prepareBuffer _ = undefined

maxBufferStack = 2 :: Int
