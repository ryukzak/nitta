{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module NITTA.Types
  ( module NITTA.Types.Base
  , module NITTA.Types.Network
  , module NITTA.Types.Poly
  , IntX(..)
  ) where

import           NITTA.Types.Base
import           NITTA.Types.Network
import           NITTA.Types.Poly

import           GHC.TypeLits

newtype IntX (w :: Nat) = IntX Int
