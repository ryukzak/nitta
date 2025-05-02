{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

module NITTA.Model.ProcessorUnits.LUT.Tests (
    tests,
) where

import Control.Monad (replicateM)
import Data.Default
import qualified Data.Map as M
import Data.Set (fromList)
import qualified Data.Text as T
import qualified NITTA.Intermediate.Functions as F
import NITTA.Intermediate.Types
import NITTA.Model.ProcessorUnits.LUT
import NITTA.Model.ProcessorUnits.Tests.Providers
import Test.QuickCheck
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)

tests =
    testGroup
        "LogicFunction"
        [ puCoSimTestCase "and_1_1" u [("a", 1), ("b", 1)] [F.logicAnd (T.pack "a") (T.pack "b") [T.pack "res"]]
        , puCoSimTestCase "and_1_0" u [("a", 1), ("b", 0)] [F.logicAnd (T.pack "a") (T.pack "b") [T.pack "res"]]
        , puCoSimTestCase "or_1_1" u [("a", 1), ("b", 1)] [F.logicOr (T.pack "a") (T.pack "b") [T.pack "res"]]
        , puCoSimTestCase "or_1_0" u [("a", 1), ("b", 0)] [F.logicOr (T.pack "a") (T.pack "b") [T.pack "res"]]
        , puCoSimTestCase "not_1" u [("a", 1)] [F.logicNot (T.pack "a") [T.pack "res"]]
        , puCoSimTestCase "not_0" u [("a", 0)] [F.logicNot (T.pack "a") [T.pack "res"]]
        , puCoSimPropWithContext
            "co simulation properties"
            u
            lfWithContextGen
        ]
    where
        u = def :: LUT T.Text Int Int
        u2 = def :: LUT T.Text (Attr (IntX 32)) Int

        lfWithContextGen = do
            (lf, cntx) <- arbitrary :: Gen ([F.LogicFunction T.Text Int], Cntx T.Text Int)
            return (fmap packF lf, cntx)
