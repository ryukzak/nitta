{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : NITTA.Model.Problems.Refactor.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Refactor.Tests (
    tests,
) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import NITTA.Intermediate.DataFlow
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import Test.Tasty (testGroup)
import Test.Tasty.HUnit

tests =
    testGroup
        "Refactor problem"
        [ testCase "self sending 1" $
            let df = fsToDataFlowGraph ([buffer "a" ["b"], buffer "b" ["c"]] :: [F String Int])
                df' = resolveDeadlockDecision df (resolveDeadlock $ S.fromList ["b"])
             in df'
                    @?= DFCluster
                        [ DFLeaf $ buffer "a" ["b@buf"]
                        , DFLeaf $ buffer "b@buf" ["b"]
                        , DFLeaf $ buffer "b" ["c"]
                        ]
        , testCase "self sending 2" $
            let df = fsToDataFlowGraph ([buffer "a" ["b1", "b2"], buffer "b1" ["c1"], buffer "b2" ["c2"]] :: [F String Int])
                df' = resolveDeadlockDecision df (resolveDeadlock $ S.fromList ["b1"])
             in df'
                    @?= DFCluster
                        [ DFLeaf $ buffer "a" ["b1@buf", "b2"]
                        , DFLeaf $ buffer "b1@buf" ["b1"]
                        , DFLeaf $ buffer "b1" ["c1"]
                        , DFLeaf $ buffer "b2" ["c2"]
                        ]
        , testCase "self sending 3" $
            let df = fsToDataFlowGraph ([buffer "a" ["b1", "b2"], buffer "b1" ["c1"], buffer "b2" ["c2"]] :: [F String Int])
                df' = resolveDeadlockDecision df (resolveDeadlock $ S.fromList ["b1", "b2"])
             in df'
                    @?= DFCluster
                        [ DFLeaf $ buffer "a" ["b1@buf"]
                        , DFLeaf $ buffer "b1@buf" ["b1", "b2"]
                        , DFLeaf $ buffer "b1" ["c1"]
                        , DFLeaf $ buffer "b2" ["c2"]
                        ]
        , testCase "patch source" $ do
            patch Changeset{changeO = M.fromList [("a1@buf", S.fromList ["a1", "a2"])], changeI = M.empty} (Source $ S.fromList ["a1@buf"])
                @?= Source (S.fromList ["a1", "a2"])
            patch Changeset{changeO = M.fromList [("a1", S.fromList ["a1@buf"]), ("a2", S.fromList ["a1@buf"])], changeI = M.empty} (Source $ S.fromList ["a1", "a2"])
                @?= Source (S.fromList ["a1@buf"])
        , testCase "reverse diff" $ do
            reverseDiff Changeset{changeI = M.fromList [("a", "b")], changeO = M.fromList [("c", S.fromList ["e", "f"])]}
                @?= Changeset{changeI = M.fromList [("b", "a")], changeO = M.fromList [("e", S.fromList ["c"]), ("f", S.fromList ["c"])]}
            reverseDiff Changeset{changeI = M.fromList [("a", "b")], changeO = M.fromList [("c", S.fromList ["e"]), ("d", S.fromList ["e"])]}
                @?= Changeset{changeI = M.fromList [("b", "a")], changeO = M.fromList [("e", S.fromList ["c", "d"])]}
        ]
