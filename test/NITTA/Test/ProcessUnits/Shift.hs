{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

module NITTA.Test.ProcessUnits.Shift where

import           Data.Proxy
import           NITTA.ProcessUnits.Shift


proxy = Proxy :: Proxy (Shift String Int Int)

-----------------------------------------------------------

-- shiftBiDirection = unitTestbench "shiftBiDirection" proxy
--   def
--   [ FB.loop 16 ["f1"] "g1" :: FB (Parcel String Int)
--   , FB.shiftL "f1" ["g1"]
--   , FB.loop 16 ["f2"] "g2"
--   , FB.shiftR "f2" ["g2"]
--   ]
