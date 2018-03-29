{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
Данный модуль реализует функции для генерации функциональных блоков.
-}
module NITTA.Test.FunctionBlocks where

import           Data.Default
import           Data.Set                (fromList, intersection)
import qualified Data.Set                as S
import           NITTA.FunctionBlocks
import           NITTA.ProcessUnits.Fram
import           NITTA.Types
import           Test.QuickCheck


framDefSize = frSize (def :: Fram () Int ())
framAddrGen = choose (0, framDefSize - 1)


outputVarsGen = O . fromList <$> resize 3 (listOf1 $ vectorOf 3 $ elements ['a'..'z'])
inputVarGen = I <$> vectorOf 3 (elements ['a'..'z'])


uniqueVars fb = S.null (inputs fb `intersection` outputs fb)


instance Arbitrary (FramInput (Parcel String Int)) where
  arbitrary = suchThat (FramInput <$> framAddrGen <*> outputVarsGen) uniqueVars

instance Arbitrary (FramOutput (Parcel String Int)) where
  arbitrary = suchThat (FramOutput <$> framAddrGen <*> inputVarGen) uniqueVars

instance Arbitrary (Loop (Parcel String Int)) where
  arbitrary = suchThat (Loop <$> outputVarsGen <*> inputVarGen) uniqueVars

instance Arbitrary (Reg (Parcel String Int)) where
  arbitrary = suchThat (Reg <$> inputVarGen <*> outputVarsGen) uniqueVars

instance Arbitrary (Constant Int (Parcel String Int)) where
  arbitrary = suchThat (Constant <$> choose (10, 16) <*> outputVarsGen) uniqueVars
