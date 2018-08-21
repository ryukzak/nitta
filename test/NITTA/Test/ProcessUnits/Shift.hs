{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

module NITTA.Test.ProcessUnits.Shift where

import           Data.Default
import           Data.Proxy
import qualified NITTA.Functions          as F
import           NITTA.ProcessUnits.Shift
import           NITTA.Test.ProcessUnits
import           NITTA.Types


proxy = Proxy :: Proxy (Shift String Int Int)

-----------------------------------------------------------

-- shiftBiDirection = unitTestBench "shiftBiDirection" proxy
--   (Just def)
--   [ F.loop 16 "g1" ["f1"] :: F (Parcel String Int)
--   , F.shiftL "f1" ["g1"]
--   , F.loop 16 "g2" ["f2"]
--   , F.shiftR "f2" ["g2"]
--   ]
