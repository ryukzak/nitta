{-# OPTIONS -fno-warn-orphans #-}

{- |
 Module      : NITTA.Intermediate.Tests.Functions
 Description :
 Copyright   : (c) Aleksandr Penskoi, 2020
 License     : BSD3
 Maintainer  : aleksandr.penskoi@gmail.com
 Stability   : experimental
-}
module NITTA.Intermediate.Tests.Functions () where

import Data.Set (fromList, intersection)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Map qualified as M
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import Test.QuickCheck

maxLenght = 8

varNameSize = 7

outputVarsGen = O . fromList . map T.pack <$> resize maxLenght (listOf1 $ vectorOf varNameSize $ elements ['a' .. 'z'])

inputVarGen = I . T.pack <$> vectorOf varNameSize (elements ['a' .. 'z'])

-- FIXME: Sometimes the algorithm can generate the following function: "qqq =
-- joi * joi", and we cannot recognize this case, because we use outputs returns
-- singleton.
uniqueVars fb = S.null (inputs fb `intersection` outputs fb)

instance Arbitrary x => Arbitrary (Loop T.Text x) where
    arbitrary = suchThat (Loop <$> (X <$> arbitrary) <*> outputVarsGen <*> inputVarGen) uniqueVars

instance Arbitrary (Buffer T.Text x) where
    arbitrary = suchThat (Buffer <$> inputVarGen <*> outputVarsGen) uniqueVars

instance Arbitrary (BrokenBuffer T.Text x) where
    arbitrary = suchThat (BrokenBuffer <$> inputVarGen <*> outputVarsGen) uniqueVars

instance (Val x, Arbitrary x) => Arbitrary (Constant T.Text x) where
    arbitrary = suchThat (Constant <$> (X <$> arbitrary) <*> outputVarsGen) uniqueVars

instance Arbitrary (Multiply T.Text x) where
    arbitrary = suchThat (Multiply <$> inputVarGen <*> inputVarGen <*> outputVarsGen) uniqueVars

instance Arbitrary (Division T.Text x) where
    arbitrary = suchThat (Division <$> inputVarGen <*> inputVarGen <*> outputVarsGen <*> outputVarsGen) uniqueVars

instance Arbitrary (Acc T.Text x) where
    arbitrary = suchThat (Acc . concat <$> resize maxLenght (listOf1 $ (++) <$> genPush <*> genPull)) uniqueVars
        where
            genPush = resize maxLenght $ listOf1 $ oneof [Push Plus <$> inputVarGen, Push Minus <$> inputVarGen]
            genPull = resize 1 $ listOf1 $ Pull <$> outputVarsGen

instance Arbitrary (IntX m) where
    arbitrary = IntX <$> choose (0, 256)

instance Arbitrary x => Arbitrary (Attr x) where
    arbitrary = Attr <$> arbitrary <*> arbitrary

instance Arbitrary x => Arbitrary (LUT T.Text x) where
    arbitrary = suchThat generateUniqueVars uniqueVars
      where
        generateUniqueVars = LUT <$> arbitraryTable <*> inputVarsGen <*> outputVarGen

        arbitraryTable = do
            keys <- resize maxLenght $ listOf1 (vectorOf maxLenght arbitrary)
            values <- vectorOf (length keys) arbitrary
            return $ M.fromList $ zip keys values

        inputVarsGen = resize maxLenght $ listOf1 inputVarGen

        outputVarGen = outputVarsGen

instance Arbitrary x => Arbitrary (LogicFunction T.Text x) where
    arbitrary = oneof [ genLogicAnd, genLogicOr, genLogicNot ]
      where
        genLogicAnd = LogicAnd <$> inputVarGen <*> inputVarGen <*> outputVarsGen
        genLogicOr = LogicOr <$> inputVarGen <*> inputVarGen <*> outputVarsGen
        genLogicNot = LogicNot <$> inputVarGen <*> outputVarsGen


