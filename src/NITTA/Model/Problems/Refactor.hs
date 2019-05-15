{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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
    ( RefactorDT, Option(..), Decision(..)
    , refactorOptions, refactorDecision
    ) where

import           Data.Proxy
import           GHC.Generics
import           NITTA.Model.Problems.Types


data RefactorDT v
refactorOptions m = options (Proxy :: Proxy RefactorDT) m
refactorDecision m d = decision (Proxy :: Proxy RefactorDT) m d

instance DecisionType (RefactorDT v) where
    data Option (RefactorDT v)
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
        deriving ( Generic, Show, Eq, Ord )
    data Decision (RefactorDT v)
        = InsertOutRegisterD v v
        deriving ( Generic, Show )
