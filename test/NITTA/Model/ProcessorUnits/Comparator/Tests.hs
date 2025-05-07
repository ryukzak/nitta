{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-partial-type-signatures #-}

module NITTA.Model.ProcessorUnits.Comparator.Tests (
    tests,
) where

import Data.Default
import Data.Text qualified as T
import NITTA.Intermediate.Functions qualified as F
import NITTA.Model.ProcessorUnits.Comparator
import NITTA.Model.ProcessorUnits.Tests.Providers
import Test.QuickCheck
import Test.Tasty (testGroup)

tests =
    testGroup
        "Comparator PU"
        [ -- Basic comparison operations
          puCoSimTestCase "cmp_eq_eq" u [("a", 5), ("b", 5)] [F.cmp F.CmpEq (T.pack "a") (T.pack "b") [T.pack "res"]]
        , puCoSimTestCase "cmp_lt_less" u [("a", 3), ("b", 5)] [F.cmp F.CmpLt (T.pack "a") (T.pack "b") [T.pack "res"]]
        , puCoSimTestCase "cmp_gt_greater" u [("a", 7), ("b", 5)] [F.cmp F.CmpGt (T.pack "a") (T.pack "b") [T.pack "res"]]
        , -- Edge cases
          puCoSimTestCase "cmp_eq_eq_zeros" u [("a", 0), ("b", 0)] [F.cmp F.CmpEq (T.pack "a") (T.pack "b") [T.pack "res"]]
        , puCoSimTestCase "cmp_lte_eq" u [("a", 5), ("b", 5)] [F.cmp F.CmpLte (T.pack "a") (T.pack "b") [T.pack "res"]]
        , puCoSimTestCase "cmp_gte_eq" u [("a", 5), ("b", 5)] [F.cmp F.CmpGte (T.pack "a") (T.pack "b") [T.pack "res"]]
        , puCoSimTestCase "cmp_gt" u [("a", 7), ("b", -9)] [F.cmp F.CmpGt (T.pack "a") (T.pack "b") [T.pack "res"]]
        , -- Attribute handling
          puCoSimTestCase
            "Comparator with Attr"
            u2
            [("a", Attr 10 True), ("b", Attr 10 True)]
            [F.cmp F.CmpEq (T.pack "a") (T.pack "b") [T.pack "res"]]
        , puCoSimTestCase
            "cmp_mix_attr"
            u2
            [("a", Attr 8 True), ("b", Attr 5 False)]
            [F.cmp F.CmpGt (T.pack "a") (T.pack "b") [T.pack "res"]]
        , -- Properties
          finitePUSynthesisProp "finite synthesis properties" u compareGen
        , puCoSimProp "co simulation properties with attr" u2 compareGen
        ]
    where
        u = def :: Comparator T.Text Int Int
        u2 = def :: Comparator T.Text (Attr (IntX 32)) Int
        compareGen =
            algGen
                [ fmap packF (arbitrary :: Gen (F.Compare T.Text _))
                ]
