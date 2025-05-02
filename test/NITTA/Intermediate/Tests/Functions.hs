{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

-- {-# LANGUAGE OverlappingInstances #-}

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
import Data.List (nub)
import Data.Map qualified as M
import Data.Set (fromList, intersection)
import Data.Set qualified as S
import Data.Text qualified as T
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

instance Arbitrary x => Arbitrary (Lut T.Text x) where
    arbitrary = suchThat generateUniqueVars uniqueVars
        where
            generateUniqueVars = Lut <$> arbitraryTable <*> inputVarsGen <*> outputVarGen

            arbitraryTable = do
                keys <- resize maxLenght $ listOf1 (vectorOf maxLenght arbitrary)
                values <- vectorOf (length keys) arbitrary
                return $ M.fromList $ zip keys values

            inputVarsGen = resize maxLenght $ listOf1 inputVarGen

            outputVarGen = outputVarsGen

-- instance Arbitrary x => Arbitrary (LogicFunction T.Text x) where
--     arbitrary = oneof [genLogicAnd, genLogicOr, genLogicNot]
--         where
--             genLogicAnd = LogicAnd <$> inputVarGen <*> inputVarGen <*> outputVarsGen
--             genLogicOr = LogicOr <$> inputVarGen <*> inputVarGen <*> outputVarsGen
--             genLogicNot = LogicNot <$> inputVarGen <*> outputVarsGen

instance Arbitrary x => Arbitrary (LogicCompare T.Text x) where
    arbitrary = suchThat generateUniqueVars uniqueVars
        where
            generateUniqueVars = do
                op <- elements [CMP_EQ, CMP_LT, CMP_LTE, CMP_GT, CMP_GTE]
                a <- inputVarGen
                b <- inputVarGen
                LogicCompare op a b <$> outputVarsGen

instance Arbitrary x => Arbitrary (Cntx T.Text x) where
    arbitrary = do
        inputVars <- vectorOf 16 inputVarGen
        let keys = map (\(I v) -> v) inputVars
        values <- mapM (const arbitrary) keys
        let cycleCntxMap = HM.fromList (zip keys values)
        let process = [CycleCntx cycleCntxMap]

        let received = M.empty
        return $
            Cntx
                { cntxProcess = process
                , cntxReceived = received
                , cntxCycleNumber = 0
                }

instance Arbitrary (Mux T.Text Int) where
    arbitrary =
        Mux
            <$> vectorOf 11 inputVarGen
            <*> inputVarGen
            <*> outputVarsGen

instance {-# OVERLAPS #-} Arbitrary ([Mux T.Text Int], Cntx T.Text Int) where
    arbitrary = do
        m@(Mux ins sel _) <- suchThat arbitrary uniqueVars

        let inputVars = [v | I v <- ins]
            selVar = case sel of I v -> v
            allVars = nub $ inputVars ++ [selVar]

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

instance {-# OVERLAPS #-} Arbitrary ([LogicFunction T.Text Int], Cntx T.Text Int) where
    arbitrary = do
        f <- oneof [genLogicAnd, genLogicOr, genLogicNot]

        let (inVars, _) = case f of
                LogicAnd a b (O o) -> ([a, b], head $ S.toList o)
                LogicOr a b (O o) -> ([a, b], head $ S.toList o)
                LogicNot a (O o) -> ([a], head $ S.toList o)

        inputValues <- forM inVars $ \_ -> do
            Positive x <- arbitrary
            return x

        let cntx =
                Cntx
                    { cntxProcess = [CycleCntx $ HM.fromList $ zip (map getVar inVars) inputValues]
                    , cntxReceived = M.empty
                    , cntxCycleNumber = 0
                    }

        return ([f], cntx)
        where
            genLogicAnd = suchThat (LogicAnd <$> inputVarGen <*> inputVarGen <*> outputVarsGen) uniqueVars
            genLogicOr = suchThat (LogicOr <$> inputVarGen <*> inputVarGen <*> outputVarsGen) uniqueVars
            genLogicNot = suchThat (LogicNot <$> inputVarGen <*> outputVarsGen) uniqueVars

            getVar (I v) = v
