{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}


module NITTA.Lens ( module NITTA.Lens
                  , module Control.Lens
                  ) where

import           Control.Lens     (Lens', lens, to, (&), (.~), (^.))
import           NITTA.Types
import           Numeric.Interval hiding (elem)



class HasAvail a b | a -> b where
  avail :: Lens' a b

instance HasAvail (TimeConstrain t) (Interval t) where
  avail = lens tcAvailable $ \a b -> a{ tcAvailable=b }



class HasAt a b | a -> b where
  at :: Lens' a b

instance HasAt (Option (Network title) v t) (TimeConstrain t) where
  at = lens toPullAt $ \variant v -> variant{ toPullAt=v }
instance HasAt (Action (Network title) v t) (Interval t) where
  at = lens taPullAt $ \variant v -> variant{ taPullAt=v }
instance HasAt (Option Passive v t) (TimeConstrain t) where
  at = lens eoAt $ \variant v -> variant{ eoAt=v }
instance HasAt (Action Passive v t) (Interval t) where
  at = lens eaAt $ \variant v -> variant{ eaAt=v }



class HasDur a b | a -> b where
  dur :: Lens' a b

instance HasDur (TimeConstrain t) (Interval t) where
  dur = lens tcDuration $ \e s -> e{ tcDuration=s }
instance ( Time t ) => HasDur (Interval t) t where
  dur = lens width $ \e s -> inf e ... (inf e + s)
instance ( Time t ) => HasDur (Action Passive v t) t where
  dur = at . dur



infimum :: ( Ord i ) => Lens' (Interval i) i
infimum = lens inf $ \a b -> b ... sup a

supremum :: ( Ord i ) => Lens' (Interval i) i
supremum = lens sup $ \a b -> inf a ... b
