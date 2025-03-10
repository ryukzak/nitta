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

import Control.Monad (forM)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.Map qualified as M
import Data.Set (fromList, intersection)
import Data.Set qualified as S
import Data.Text qualified as T
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import Test.QuickCheck hiding (Function)

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

instance Arbitrary (FloatDivision T.Text x) where
    arbitrary = suchThat (FloatDivision <$> inputVarGen <*> inputVarGen <*> outputVarsGen) uniqueVars


instance Arbitrary (Acc T.Text x) where
    arbitrary = suchThat (Acc . concat <$> resize maxLenght (listOf1 $ (++) <$> genPush <*> genPull)) uniqueVars
        where
            genPush = resize maxLenght $ listOf1 $ oneof [Push Plus <$> inputVarGen, Push Minus <$> inputVarGen]
            genPull = resize 1 $ listOf1 $ Pull <$> outputVarsGen

instance Arbitrary (IntX m) where
    arbitrary = IntX <$> choose (0, 256)

instance Arbitrary x => Arbitrary (Attr x) where
    arbitrary = Attr <$> arbitrary <*> arbitrary

instance Arbitrary CmpOp where
    arbitrary = elements [CmpEq, CmpLt, CmpLte, CmpGt, CmpGte]

instance Arbitrary x => Arbitrary (Compare T.Text x) where
    arbitrary = suchThat generateUniqueVars uniqueVars
        where
            generateUniqueVars = do
                op <- arbitrary
                a <- inputVarGen
                b <- inputVarGen
                Compare op a b <$> outputVarsGen

instance Arbitrary x => Arbitrary (TruthTable T.Text x) where
    arbitrary = suchThat generateUniqueVars uniqueVars
        where
            generateUniqueVars = TruthTable <$> arbitraryTable <*> inputVarsGen <*> outputVarGen

            arbitraryTable = do
                keys <- resize maxLenght $ listOf1 (vectorOf maxLenght arbitrary)
                values <- vectorOf (length keys) arbitrary
                return $ M.fromList $ zip keys values

            inputVarsGen = resize maxLenght $ listOf1 inputVarGen

            outputVarGen = outputVarsGen

instance Arbitrary (LogicFunction T.Text x) where
    arbitrary =
        oneof
            [ LogicAnd <$> inputVarGen <*> inputVarGen <*> outputVarsGen
            , LogicOr <$> inputVarGen <*> inputVarGen <*> outputVarsGen
            , LogicNot <$> inputVarGen <*> outputVarsGen
            ]
            `suchThat` uniqueVars

instance {-# OVERLAPS #-} (Arbitrary f', Function f' T.Text) => Arbitrary ([f'], Cntx T.Text Int) where
    arbitrary = do
        f <- arbitrary

        let inVars = S.toList $ inputs f

        inputValues <- forM inVars $ \_ -> do
            Positive x <- arbitrary
            return x

        let cntx =
                Cntx
                    { cntxProcess = [CycleCntx $ HM.fromList $ zip inVars inputValues]
                    , cntxReceived = M.empty
                    , cntxCycleNumber = 0
                    }

        return ([f], cntx)

instance Arbitrary (Mux T.Text Int) where
    arbitrary =
        Mux
            <$> inputVarGen
            <*> vectorOf 11 inputVarGen
            <*> outputVarsGen

instance {-# OVERLAPS #-} Arbitrary ([Mux T.Text Int], Cntx T.Text Int) where
    arbitrary = do
        m@(Mux sel ins _) <- suchThat arbitrary uniqueVars

        let inputVars = [v | I v <- ins]
            selVar = case sel of I v -> v
            allVars = L.nub $ inputVars ++ [selVar]

        initialValues <- forM allVars $ \v -> do
            Positive x <- arbitrary
            return (v, x)

        let dataCount = length inputVars
        selValue <-
            if dataCount > 0
                then choose (0, dataCount - 1)
                else pure 0

        let cntx =
                Cntx
                    { cntxProcess = [CycleCntx $ HM.fromList $ (selVar, selValue) : initialValues]
                    , cntxReceived = M.empty
                    , cntxCycleNumber = 0
                    }

        return ([m], cntx)
