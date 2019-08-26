{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Utils.Lens
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Utils.Lens
  ( module NITTA.Utils.Lens
  , module Control.Lens
  ) where

import           Control.Lens      (Lens', lens, to, (&), (.~), (^.))
import           NITTA.Model.Types
import           Numeric.Interval


class HasAvail a b | a -> b where
  avail :: Lens' a b

instance HasAvail (TimeConstrain t) (Interval t) where
  avail = lens tcAvailable $ \a b -> a{ tcAvailable=b }


class HasDur a b | a -> b where
  dur :: Lens' a b

instance HasDur (TimeConstrain t) (Interval t) where
  dur = lens tcDuration $ \e s -> e{ tcDuration=s }
instance ( Time t ) => HasDur (Interval t) t where
  dur = lens width $ \e s -> inf e ... (inf e + s)


infimum :: ( Ord i ) => Lens' (Interval i) i
infimum = lens inf $ \a b -> b ... sup a

supremum :: ( Ord i ) => Lens' (Interval i) i
supremum = lens sup $ \a b -> inf a ... b
