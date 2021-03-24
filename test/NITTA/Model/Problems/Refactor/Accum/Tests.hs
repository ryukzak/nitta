{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : NITTA.Model.Problems.Refactor.Accum.Tests
Description :
Copyright   : (c) Daniil Prohorov, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Refactor.Accum.Tests (
    tests,
) where

import qualified Data.Set as S
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.Model.Problems.Refactor
import Test.Tasty (testGroup)
import Test.Tasty.HUnit

refactorTo :: HasCallStack => [F String Int] -> [F String Int] -> Assertion
refactorTo startFs resultFs = S.fromList (simpleRefactor startFs) @?= S.fromList resultFs

simpleRefactor dfg =
    case optimizeAccumOptions dfg of
        [] -> dfg
        (r : _) -> simpleRefactor $ optimizeAccumDecision dfg r

tests =
    testGroup
        "Refactor problem (Optimize Accum)"
        [ testCase "Add refactor test" $
            let -- Start algorithm:
                -- tmp1 = a - b
                -- res = c - tmp1
                --
                -- Result algorithm:
                -- res = c - a + b
                func1 = sub "a" "b" ["tmp1"]
                func2 = sub "c" "tmp1" ["res"]
                funcRes = acc [Push Plus (I "c"), Push Minus (I "a"), Push Plus (I "b"), Pull (O $ S.fromList ["res"])]
             in [func1, func2] `refactorTo` [funcRes]
        , testCase "Acc and Add refactor test" $
            let -- Start algorithm:
                -- tmp1 = Acc [a, b] => tmp1
                -- res = c + tmp1
                --
                -- Result algorithm:
                -- res = c + a + b
                func1 = acc [Push Plus (I "a"), Push Plus (I "b"), Pull (O $ S.fromList ["tmp1"])]
                func2 = add "c" "tmp1" ["res"]
                funcRes = acc [Push Plus (I "c"), Push Plus (I "a"), Push Plus (I "b"), Pull (O $ S.fromList ["res"])]
             in [func1, func2] `refactorTo` [funcRes]
        , testCase "simple 1 tmp variable sum refactor" $
            let -- Start algorithm:
                -- tmp1 = a + b
                -- res = tmp1 + c
                --
                -- Result algorithm:
                -- res = a + b + c
                func1 = acc [Push Plus (I "a"), Push Plus (I "b"), Pull (O $ S.fromList ["tmp1"])]
                func2 = acc [Push Plus (I "tmp1"), Push Plus (I "c"), Pull (O $ S.fromList ["res"])]
                funcRes = acc [Push Plus (I "a"), Push Plus (I "b"), Push Plus (I "c"), Pull (O $ S.fromList ["res"])]
             in [func1, func2] `refactorTo` [funcRes]
        , testCase "not refactor this" $
            let -- Start algorithm:
                -- tmp1, tmp2 = a + b
                -- res = tmp1 + c
                --
                -- Result algorithm:
                -- tmp1, tmp2 = a + b
                -- res = tmp1 + c
                func1 = acc [Push Plus (I "a"), Push Plus (I "b"), Pull (O $ S.fromList ["tmp1", "tmp2"])]
                func2 = acc [Push Plus (I "tmp1"), Push Plus (I "c"), Pull (O $ S.fromList ["res"])]
             in [func1, func2] `refactorTo` [func1, func2]
        , testCase "simple 4 items sum refactor" $
            let -- Start algorithm:
                -- tmp1 = a + b
                -- tmp2 = tmp1 + c
                -- res = tmp2 - d
                --
                -- Result algorithm:
                -- res = a + b + c - d
                func1 = acc [Push Plus (I "a"), Push Plus (I "b"), Pull (O $ S.fromList ["tmp1"])]
                func2 = acc [Push Plus (I "tmp1"), Push Plus (I "c"), Pull (O $ S.fromList ["tmp2"])]
                func3 = acc [Push Plus (I "tmp2"), Push Minus (I "d"), Pull (O $ S.fromList ["res"])]
                funcRes = acc [Push Plus (I "a"), Push Plus (I "b"), Push Plus (I "c"), Push Minus (I "d"), Pull (O $ S.fromList ["res"])]
             in [func1, func2, func3] `refactorTo` [funcRes]
        , testCase "4 items sum refactor, two tmp vals in one expression" $
            let -- Start algorithm:
                -- tmp1 = a + b
                -- tmp2 = c + d
                -- res = tmp1 - tmp2
                --
                -- Result algorithm:
                -- res = a + b - c - d
                func1 = acc [Push Plus (I "a"), Push Plus (I "b"), Pull (O $ S.fromList ["tmp1"])]
                func2 = acc [Push Plus (I "c"), Push Plus (I "d"), Pull (O $ S.fromList ["tmp2"])]
                func3 = acc [Push Plus (I "tmp1"), Push Minus (I "tmp2"), Pull (O $ S.fromList ["res"])]
                funcRes = acc [Push Plus (I "a"), Push Plus (I "b"), Push Minus (I "c"), Push Minus (I "d"), Pull (O $ S.fromList ["res"])]
             in [func1, func2, func3] `refactorTo` [funcRes]
            -- issue: https://nitta.io/nitta-corp/nitta/-/issues/75
            -- , testCase "Complex items sum refactor" $ let
            --         -- Start algorithm:
            --         -- tmp1, tmp2 = a + b
            --         -- tmp3, tmp4 = c + d
            --         -- res1 = one + tmp1 + tmp3
            --         -- res2 = two + tmp2 + tmp4
            --         -- res = res1 + res2

            --         -- Result algorithm:
            --         -- res = one + a + b + c + d + two + a + b + c + d
            --         func1 = acc [Push Plus (I "a"), Push Plus (I "b"), Pull (O $ S.fromList ["tmp1", "tmp2"])]
            --         func2 = acc [Push Plus (I "c"), Push Plus (I "d"), Pull (O $ S.fromList ["tmp3", "tmp4"])]
            --         func3 = acc [Push Plus (I "one"), Push Plus (I "tmp1"), Push Plus (I "tmp3"), Pull (O $ S.fromList ["res1"])]
            --         func4 = acc [Push Plus (I "two"), Push Plus (I "tmp2"), Push Plus (I "tmp4"), Pull (O $ S.fromList ["res2"])]
            --         func5 = acc [Push Plus (I "res1"), Push Plus (I "res2"), Pull (O $ S.fromList ["res"])]

            --         funcRes = acc
            --             [ Push Plus (I "one")
            --             , Push Plus (I "a")
            --             , Push Plus (I "b")
            --             , Push Plus (I "c")
            --             , Push Plus (I "d")
            --             , Push Plus (I "two")
            --             , Push Plus (I "a")
            --             , Push Plus (I "b")
            --             , Push Plus (I "c")
            --             , Push Plus (I "d")
            --             , Pull (O $ S.fromList ["res"])] :: F String Int
            --         df = fsToDataFlowGraph ([func1, func2, func3, func4, func5] :: [F String Int])
            --         dfRes = fsToDataFlowGraph ([funcRes] :: [F String Int])
            --         option = head $ refactorOptions df
            --         dfRefactored = refactorDecision df option
            --     in dfRefactored @?= dfRes
        ]
