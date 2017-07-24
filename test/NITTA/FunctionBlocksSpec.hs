{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NITTA.FunctionBlocksSpec where

import           Data.List               (nub)
import           NITTA.FunctionBlocks
import           NITTA.ProcessUnits.Fram
import           NITTA.Types
import           Test.QuickCheck


addrGen = choose (0, framSize - 1)
forPull = resize 3 $ listOf1 $ vectorOf 3 (elements ['a'..'z'])
forPush = vectorOf 3 (elements ['a'..'z'])

uniqVars fb = let vs = variables fb
              in length vs == length (nub vs)


instance Arbitrary (FramInput String) where
  arbitrary = suchThat (FramInput <$> addrGen <*> forPull) uniqVars

instance Arbitrary (FramOutput String) where
  arbitrary = suchThat (FramOutput <$> addrGen <*> forPush) uniqVars

instance Arbitrary (Loop String) where
  arbitrary = suchThat (Loop <$> forPush <*> forPull) uniqVars

instance Arbitrary (Reg String) where
  arbitrary = suchThat (Reg <$> forPush <*> forPull) uniqVars
