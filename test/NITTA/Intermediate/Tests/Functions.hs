{- FOURMOLU_DISABLE -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -fno-warn-orphans #-}

-- |
-- Module      : NITTA.Intermediate.Tests.Functions
-- Description :
-- Copyright   : (c) Aleksandr Penskoi, 2020
-- License     : BSD3
-- Maintainer  : aleksandr.penskoi@gmail.com
-- Stability   : experimental
module NITTA.Intermediate.Tests.Functions () where

import Data.Set (fromList, intersection)
import qualified Data.Set as S
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import Test.QuickCheck

maxLenght = 8

varNameSize = 7

outputVarsGen = O . fromList <$> resize maxLenght (listOf1 $ vectorOf varNameSize $ elements ['a' .. 'z'])

inputVarGen = I <$> vectorOf varNameSize (elements ['a' .. 'z'])

-- FIXME: Sometimes the algorithm can generate the following function: "qqq =
-- joi * joi", and we cannot recognize this case, because we use outputs returns
-- singleton.
uniqueVars fb = S.null (inputs fb `intersection` outputs fb)

instance ( Arbitrary x ) => Arbitrary (Loop String x) where
  arbitrary = suchThat (Loop <$> (X <$> arbitrary) <*> outputVarsGen <*> inputVarGen) uniqueVars

instance Arbitrary (Reg String x) where
  arbitrary = suchThat (Reg <$> inputVarGen <*> outputVarsGen) uniqueVars

instance Arbitrary (BrokenReg String x) where
  arbitrary = suchThat (BrokenReg <$> inputVarGen <*> outputVarsGen) uniqueVars

instance ( Val x, Arbitrary x ) => Arbitrary (Constant String x) where
  arbitrary = suchThat (Constant <$> (X <$> arbitrary) <*> outputVarsGen) uniqueVars

instance Arbitrary (Multiply String x) where
  arbitrary = suchThat (Multiply <$> inputVarGen <*> inputVarGen <*> outputVarsGen) uniqueVars

instance Arbitrary (Division String x) where
  arbitrary = suchThat (Division <$> inputVarGen <*> inputVarGen <*> outputVarsGen <*> outputVarsGen) uniqueVars

instance Arbitrary (Acc String x) where
  arbitrary = suchThat (Acc . concat <$> resize maxLenght (listOf1 $ (++) <$> genPush <*> genPull)) uniqueVars
    where
      genPush = resize maxLenght $ listOf1 $ oneof [Push Plus <$> inputVarGen, Push Minus <$> inputVarGen]
      genPull = resize 1 $ listOf1 $ Pull <$> outputVarsGen

instance Arbitrary (IntX m) where
    arbitrary = IntX <$> choose (0, 256)

instance ( Arbitrary x ) => Arbitrary (Attr x) where
    arbitrary = Attr <$> arbitrary <*> arbitrary
