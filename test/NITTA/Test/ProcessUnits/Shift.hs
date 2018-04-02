{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

module NITTA.Test.ProcessUnits.Shift where

import           Control.Applicative       ((<$>))
import           Data.Default
import qualified Data.Map                  as M
import           Data.Proxy
import           NITTA.Compiler
import qualified NITTA.FunctionBlocks      as FB
import           NITTA.ProcessUnits.Shift
import           NITTA.Test.FunctionBlocks ()
import           NITTA.Test.ProcessUnits
import           NITTA.TestBench
import           NITTA.Types
import           System.FilePath.Posix     (joinPath)
import           Test.QuickCheck
import           Test.Tasty.HUnit


proxy = Proxy :: Proxy (Shift String Int Int)

-----------------------------------------------------------

-- shiftBiDirection = unitTestbench "shiftBiDirection" proxy
--   def
--   [ FB.loop 16 ["f1"] "g1" :: FB (Parcel String Int)
--   , FB.shiftL "f1" ["g1"]
--   , FB.loop 16 ["f2"] "g2"
--   , FB.shiftR "f2" ["g2"]
--   ]
