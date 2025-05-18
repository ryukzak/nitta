{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module NITTA.Model.ProcessorUnits.LogicalUnit.Tests (
    tests,
) where

import Data.Default
import Data.Text qualified as T
import NITTA.Intermediate.Functions qualified as F
import NITTA.Intermediate.Types
import NITTA.Model.ProcessorUnits.LogicalUnit
import NITTA.Model.ProcessorUnits.Tests.Providers
import Test.QuickCheck
import Test.Tasty (testGroup)

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
        u = def :: LogicalUnit T.Text Int Int

        lfWithContextGen = do
            (lf, cntx) <- arbitrary :: Gen ([F.LogicFunction T.Text Int], Cntx T.Text Int)
            return (fmap packF lf, cntx)
