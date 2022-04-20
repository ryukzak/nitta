{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Model.Problems.Allocation
Description : PU allocation on the bus network
Copyright   : (c) Aleksandr Penskoi, 2022
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Allocation (
    Allocation (..),
    AllocationProblem (..),
) where

import Data.String.ToString (ToString (..))
import GHC.Generics (Generic)

data Allocation tag = Allocation
    { bnTag :: tag
    -- ^Tag of the BusNetwork where PU will be allocated
    , puTag :: tag
    -- ^Tag of the prototype that will be used for allocation
    }
    deriving (Generic, Eq)

instance (ToString tag) => Show (Allocation tag) where
    show Allocation{bnTag, puTag} = "Allocation of " <> toString puTag <> " on " <> toString bnTag

class AllocationProblem u tag | u -> tag where
    allocationOptions :: u -> [Allocation tag]
    allocationDecision :: u -> Allocation tag -> u
