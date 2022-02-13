{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Model.Problems.Bind
Description : Function distribution between processor units
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Bind (
    Bind (..),
    BindProblem (..),
) where

import Data.String.ToString
import GHC.Generics
import NITTA.Intermediate.Types

data Bind tag v x
    = Bind (F v x) tag
    deriving (Generic, Eq)

instance (ToString tag) => Show (Bind tag v x) where
    show (Bind f tag) = "Bind " <> show f <> " " <> toString tag

class BindProblem u tag v x | u -> tag v x where
    bindOptions :: u -> [Bind tag v x]
    bindDecision :: u -> Bind tag v x -> u

instance (Var v) => Variables (Bind tab v x) v where
    variables (Bind f _tag) = variables f
