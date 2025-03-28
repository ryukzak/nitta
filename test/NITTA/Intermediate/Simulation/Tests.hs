{-# LANGUAGE DataKinds #-}

{- |
Module      : NITTA.Intermediate.Simulation.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Intermediate.Simulation.Tests (
    tests,
) where

import Data.CallStack
import Data.Default
import Data.HashMap.Strict qualified as HM
import Data.List (permutations)
import Data.Map qualified as M
import Data.Maybe
import NITTA.Intermediate.Analysis (estimateVarWaves, reorderAlgorithm)
import NITTA.Intermediate.DataFlow
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Simulation
import NITTA.Intermediate.Types
import NITTA.Model.TargetSystem ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

simulationTests =
    testGroup
        "functional simulation"
        [ testCase "reorder algorithm" $ do
            let action = reorderAlgorithm :: [F String Int] -> [F String Int]
                l1 = loop 0 "b2" ["a1"]
                l2 = loop 1 "c" ["b1", "b2"]
                a = add "a1" "b1" ["c"]
            mapM_ ([l1, l2, a] @=?) $ map action [[l1, l2, a], [l1, a, l2], [a, l1, l2]]
        , testGroup
            "estimate waves"
            [ testCase "function sequence" $ do
                let action = estimateVarWaves ["a", "b"] :: [F String Int] -> M.Map String Int
                    a1 = add "a" "b" ["c"]
                    a2 = add "c" "b" ["d"]
                    a3 = add "d" "a" ["e"]
                mapM_ (M.fromList [("c", 0), ("d", 1), ("e", 2)] @=?) $ map action $ permutations [a1, a2, a3]
            , testCase "loop output vars have zero wave" $ do
                let action = estimateVarWaves [] :: [F String Int] -> M.Map String Int
                    l1 = loop 1 "b2" ["a1"]
                    l2 = loop 1 "c" ["b1", "b2"]
                    a = add "a1" "b1" ["c"]
                M.fromList [("a1", 0), ("b1", 0), ("b2", 0), ("c", 1)] @=? action [l1, l2, a]
            ]
        , simulationTestCase @Int
            "fibonacci sequence"
            7
            def
            [ loop 0 "b2" ["a1"]
            , loop 1 "c" ["b1", "b2"]
            , add "a1" "b1" ["c"]
            ]
            ("a1", [0, 1, 1, 2, 3, 5, 8])
        , simulationTestCase @Int
            "send and receive"
            5
            [ ("a", [1, 2, 3, 4, 5])
            ]
            [ receive ["a"]
            , constant 10 ["b"]
            , add "a" "b" ["c"]
            , send "c"
            ]
            ("c", [11, 12, 13, 14, 15])
        , simulationTestCase @(FX 32 32)
            "fibonacci sequence for FX"
            7
            def
            [ loop FX{rawFX = 0} "b2" ["a1"]
            , loop FX{rawFX = 1} "c" ["b1", "b2"]
            , add "a1" "b1" ["c"]
            ]
            ("a1", [FX{rawFX = 0}, FX{rawFX = 1}, FX{rawFX = 1}, FX{rawFX = 2}, FX{rawFX = 3}, FX{rawFX = 5}, FX{rawFX = 8}])
        , simulationTestCase @Float
            "division for Float"
            7
            def
            [ loop 2 "c" ["a"]
            , constant 2 ["b"]
            , floatDivision "a" "b" ["c"]
            ]
            ("a", [2, 1, 0.5, 0.25, 0.125, 0.0625, 0.03125])
        , testGroup
            "logic operations"
            [ simulationTestCase @Int
                "lessThan comparison"
                3
                [ ("a", [2, 5, 3])
                , ("b", [5, 2, 3])
                ]
                [ receive ["a"]
                , receive ["b"]
                , cmp CmpLt "a" "b" ["c"]
                , send "c"
                ]
                ("c", [1, 0, 0])
            , simulationTestCase @Int
                "lessThanOrEqual comparison"
                3
                [ ("a", [2, 5, 3])
                , ("b", [5, 2, 3])
                ]
                [ receive ["a"]
                , receive ["b"]
                , cmp CmpLte "a" "b" ["c"]
                , send "c"
                ]
                ("c", [1, 0, 1])
            , simulationTestCase @Int
                "equal comparison"
                3
                [ ("a", [2, 5, 3])
                , ("b", [5, 2, 3])
                ]
                [ receive ["a"]
                , receive ["b"]
                , cmp CmpEq "a" "b" ["c"]
                , send "c"
                ]
                ("c", [0, 0, 1])
            , simulationTestCase @Int
                "greaterThan comparison"
                3
                [ ("a", [2, 5, 3])
                , ("b", [5, 2, 3])
                ]
                [ receive ["a"]
                , receive ["b"]
                , cmp CmpGt "a" "b" ["c"]
                , send "c"
                ]
                ("c", [0, 1, 0])
            , simulationTestCase @Int
                "greaterThanOrEqual comparison"
                3
                [ ("a", [2, 5, 3])
                , ("b", [5, 2, 3])
                ]
                [ receive ["a"]
                , receive ["b"]
                , cmp CmpGte "a" "b" ["c"]
                , send "c"
                ]
                ("c", [0, 1, 1])
            ]
        , testGroup
            "logic operations"
            [ simulationTestCase @Int
                "AND operation"
                4
                [ ("a", [1, 1, 0, 0])
                , ("b", [1, 0, 1, 0])
                ]
                [ receive ["a"]
                , receive ["b"]
                , logicAnd "a" "b" ["c"]
                , send "c"
                ]
                ("c", [1, 0, 0, 0])
            , simulationTestCase @Int
                "OR operation"
                4
                [ ("a", [1, 1, 0, 0])
                , ("b", [1, 0, 1, 0])
                ]
                [ receive ["a"]
                , receive ["b"]
                , logicOr "a" "b" ["c"]
                , send "c"
                ]
                ("c", [1, 1, 1, 0])
            , simulationTestCase @Int
                "NOT operation"
                4
                [ ("a", [1, 0, 1, 0])
                ]
                [ receive ["a"]
                , logicNot "a" ["c"]
                , send "c"
                ]
                ("c", [0, 1, 0, 1])
            ]
        , simulationTestCase @Int
            "combination of operations"
            4
            [ ("a", [1, 1, 0, 0])
            , ("b", [5, 0, 1, 0])
            , ("c1", [2, 0, 0, 1])
            , ("c2", [1, 1, 1, 0])
            , ("c3", [1, 0, 1, 0])
            ]
            [ receive ["a"]
            , receive ["b"]
            , receive ["c1"]
            , receive ["c2"]
            , receive ["c3"]
            , logicAnd "a" "b" ["r1"]
            , logicAnd "c1" "c2" ["tmp"]
            , logicOr "tmp" "c3" ["r2"]
            , send "r1"
            , send "r2"
            ]
            ("r2", [1, 0, 1, 0])
        ]

tests =
    testGroup
        "intermediate simulation"
        [ simulationTests
        ]

simulationTestCase ::
    forall x.
    Val x =>
    HasCallStack =>
    String ->
    Int ->
    [(String, [x])] ->
    [F String x] ->
    (String, [x]) ->
    TestTree
simulationTestCase name n received alg (v, expect) =
    testCase name $
        let dfg = fsToDataFlowGraph alg
            Cntx{cntxProcess} = simulateDataFlowGraph n def received dfg
            actual = map (\(CycleCntx c) -> fromMaybe (error $ show c) (HM.lookup v c)) cntxProcess
         in expect @=? actual
