{- |
Module      : NITTA.Test.Utils
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Utils.Tests (
    tests, testCaseM
) where

import Data.Set (fromList)
import NITTA.Intermediate.Functions ()
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Utils
import Test.Tasty
import Test.Tasty.HUnit

tests =
    testGroup
        "NITTA.Utils.Tests"
        [ testCaseM "values2dump" $ do
            "0" @=? values2dump [Undef, Undef, Undef, Undef]
            "0" @=? values2dump [Bool False, Bool False, Bool False, Bool False]
            "f" @=? values2dump [Bool True, Bool True, Bool True, Bool True]
            "17" @=? values2dump [Bool True, Bool False, Bool True, Bool True, Bool True]
            "000000" @=? values2dump (replicate 24 $ Bool False)
        , testCaseM "endpoint role equality" $ do
            let source = Source . fromList
            Target "a" == Target "a" @? "Target eq"
            Target "a" /= Target "b" @? "Target not eq"
            source ["a"] /= Target "b" @? "Target not eq Source"
            source ["a"] == source ["a"] @? "Source eq"
            source ["a", "b"] == source ["a", "b"] @? "Source eq"
            source ["b", "a"] == source ["a", "b"] @? "Source eq"
            source ["b", "a"] /= source ["a", "c"] @? "Source not eq"
        ]


testCaseM name = testCase $ toModuleName name

