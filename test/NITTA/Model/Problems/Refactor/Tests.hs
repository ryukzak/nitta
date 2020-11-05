{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : NITTA.Model.Problems.Refactor.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Refactor.Tests
    ( tests
    ) where

import qualified Data.Map as M
import qualified Data.Set as S
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems
import           NITTA.Model.TargetSystem
import           NITTA.Model.Types
import           NITTA.TargetSynthesis
import           Test.Tasty ( testGroup )
import           Test.Tasty.HUnit

refactorTo startFs resultFs = dfRefactored @?= dfRes
    where
        df = fsToDataFlowGraph (startFs :: [F String Int])
        dfRes = fsToDataFlowGraph (resultFs :: [F String Int])
        dfRefactored = simpleRefactor df

tests = testGroup "Refactor problem"
    [ testCase "self sending 1" $ let
            df = fsToDataFlowGraph ([ reg "a" ["b"], reg "b" ["c"] ] :: [F String Int])
            df' = refactorDecision df (ResolveDeadlock $ S.fromList ["b"])
        in df' @?= DFCluster
            [ DFLeaf $ reg "a" ["b@buf"]
            , DFLeaf $ reg "b@buf" ["b"]
            , DFLeaf $ reg "b" ["c"]
            ]

    , testCase "self sending 2" $ let
            df = fsToDataFlowGraph ([ reg "a" ["b1", "b2"], reg "b1" ["c1"], reg "b2" ["c2"] ] :: [F String Int])
            df' = refactorDecision df (ResolveDeadlock $ S.fromList ["b1"])
        in df' @?= DFCluster
            [ DFLeaf $ reg "a" ["b1@buf", "b2"]
            , DFLeaf $ reg "b1@buf" ["b1"]
            , DFLeaf $ reg "b1" ["c1"]
            , DFLeaf $ reg "b2" ["c2"]
            ]

    , testCase "self sending 3" $ let
            df = fsToDataFlowGraph ([ reg "a" ["b1", "b2"], reg "b1" ["c1"], reg "b2" ["c2"] ] :: [F String Int])
            df' = refactorDecision df (ResolveDeadlock $ S.fromList ["b1", "b2"])
        in df' @?= DFCluster
            [ DFLeaf $ reg "a" ["b1@buf"]
            , DFLeaf $ reg "b1@buf" ["b1", "b2"]
            , DFLeaf $ reg "b1" ["c1"]
            , DFLeaf $ reg "b2" ["c2"]
            ]
  , testCase "simple 1 tmp variable sum refactor" $ let
            -- Start algorithm:
            -- tmp1 = a + b
            -- res = tmp1 + c
            --
            -- Result algorithm:
            -- res = a + b + c
            func1 = acc [Push Plus (I "a"), Push Plus (I "b"), Pull (O $ S.fromList ["tmp1"])]
            func2 = acc [Push Plus (I "tmp1"), Push Plus (I "c"), Pull (O $ S.fromList ["res"])]
            funcRes = acc [Push Plus (I "a"), Push Plus (I "b"), Push Plus (I "c"), Pull (O $ S.fromList ["res"])]
        in [func1, func2] `refactorTo` [funcRes]
  , testCase "not refactor this" $ let
            -- Start algorithm:
            -- tmp1, tmp2 = a + b
            -- res = tmp1 + c
            --
            -- Result algorithm:
            -- tmp1, tmp2 = a + b
            -- res = tmp1 + c
            func1 = acc [Push Plus (I "a"), Push Plus (I "b"), Pull (O $ S.fromList ["tmp1", "tmp2"])]
            func2 = acc [Push Plus (I "tmp1"), Push Plus (I "c"), Pull (O $ S.fromList ["res"])]
        in [func1, func2] `refactorTo` [func1, func2]

    , testCase "simple 4 items sum refactor" $ let
            -- Start algorithm:
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

    , testCase "4 items sum refactor, two tmp vals in one expression" $ let
            -- Start algorithm:
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

    , testCase "patch source" $ do
        patch Changeset{ changeO=M.fromList [("a1@buf", S.fromList ["a1", "a2"])], changeI=M.empty } (Source $ S.fromList ["a1@buf"])
            @?= Source (S.fromList ["a1", "a2"])
        patch Changeset{ changeO=M.fromList [("a1", S.fromList ["a1@buf"]), ("a2", S.fromList ["a1@buf"])], changeI=M.empty } (Source $ S.fromList ["a1", "a2"])
            @?= Source (S.fromList ["a1@buf"])

    , testCase "reverse diff" $ do
        reverseDiff Changeset{ changeI=M.fromList [("a", "b")], changeO=M.fromList [("c", S.fromList ["e", "f"])] }
            @?= Changeset{ changeI=M.fromList [("b", "a")], changeO=M.fromList [("e", S.fromList ["c"]), ("f", S.fromList ["c"])] }
        reverseDiff Changeset{ changeI=M.fromList [("a", "b")], changeO=M.fromList [("c", S.fromList ["e"]),("d", S.fromList ["e"])] }
            @?= Changeset{ changeI=M.fromList [("b", "a")], changeO=M.fromList [("e", S.fromList ["c", "d"])] }

    ]
