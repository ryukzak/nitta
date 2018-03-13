{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}


module NITTA.Lens
  ( module NITTA.Lens
  , module Control.Lens
  ) where

import           Control.Lens     (Lens', lens, to, (&), (.~), (^.))
import           NITTA.Types
import           Numeric.Interval



class HasAvail a b | a -> b where
  avail :: Lens' a b

instance HasAvail (TimeConstrain t) (Interval t) where
  avail = lens tcAvailable $ \a b -> a{ tcAvailable=b }



class HasEndType a b | a -> b where
  endType :: Lens' a b
instance HasEndType (Option (EndpointDT v t)) (EndpointType v) where
  endType = lens epoType $ \a@EndpointO{..} b -> a{ epoType=b }
instance HasEndType (Decision (EndpointDT v t)) (EndpointType v) where
  endType = lens epdType $ \a@EndpointD{..} b -> a{ epdType=b }



class HasAt a b | a -> b where
  at :: Lens' a b

instance HasAt (Option (DataFlowDT title v t)) (TimeConstrain t) where
  at = lens (snd . dfoSource) $ \a@DataFlowO{ dfoSource=(title, _time) } b -> a{ dfoSource=(title, b) }
instance HasAt (Decision (DataFlowDT title v t)) (Interval t) where
  at = lens (snd . dfdSource) $ \a@DataFlowD{ dfdSource=(title, _time) } b -> a{ dfdSource=(title, b) }
instance HasAt (Option (EndpointDT v t)) (TimeConstrain t) where
  at = lens epoAt $ \a@EndpointO{..} b -> a{ epoAt=b }
instance HasAt (Decision (EndpointDT v t)) (Interval t) where
  at = lens epdAt $ \a@EndpointD{..} b -> a{ epdAt=b }



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
