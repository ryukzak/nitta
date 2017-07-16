{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module NITTA.Types where

import           Data.Default  (Default)
import           Data.Typeable (Typeable)

class ( Typeable v, Eq v, Ord v, Show v ) => Var v
instance ( Typeable v, Eq v, Ord v, Show v ) => Var v

class ( Default t, Num t, Bounded t, Ord t, Show t, Typeable t, Enum t ) => Time t
instance ( Default t, Num t, Bounded t, Ord t, Show t, Typeable t, Enum t ) => Time t

class Vars a var | a -> var where
  variables :: a -> [var]
