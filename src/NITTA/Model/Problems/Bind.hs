{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

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
    GroupBindingT (..),
) where

import qualified Data.Set as S
import Data.String.ToString
import GHC.Generics
import NITTA.Intermediate.Types

data GroupBindingT = NonAlternativeBinds | AllBinds | FirstWaveBinds
    deriving (Show, Generic, Eq)

data Bind tag v x
    = Bind (F v x) tag
    | GroupBinding GroupBindingT [Bind tag v x]
    deriving (Generic, Eq)

instance (ToString tag) => Show (Bind tag v x) where
    show (Bind f tag) = "Bind " <> show f <> " " <> toString tag
    show (GroupBinding t bindings) = "GroupBiding (" <> show t <> ") " <> concatMap (\(Bind f tag) -> show f <> " " <> toString tag) bindings

class BindProblem u tag v x | u -> tag v x where
    bindOptions :: u -> [Bind tag v x]
    bindDecision :: u -> Bind tag v x -> u

instance (Var v) => Variables (Bind tab v x) v where
    variables (Bind f _tag) = variables f
    variables (GroupBinding _ binds) = S.unions $ map variables binds
