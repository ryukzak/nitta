{-# LANGUAGE TypeApplications #-}

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
            , simulationTestCase @Int
                "lessThan comparison"
                3
                [ ("a", [2, 5, 3])
                , ("b", [5, 2, 3])
                ]
                [ receive ["a"]
                , receive ["b"]
                , logicCompare CMP_LT "a" "b" ["c"]
                , send "c"
                ]
                ("c", [1, 0, 0])
            , simulationTestCase @Int
                "mux selector"
                4
                [ ("cond", [1, 0, 1, 0])
                , ("a", [10, 20, 30, 40])
                , ("b", [50, 60, 70, 80])
                ]
                [ receive ["a"]
                , receive ["b"]
                , receive ["cond"]
                , mux "a" "b" "cond" ["c"]
                , send "c"
                ]
                ("c", [50, 20, 70, 40])
            ]
        , testGroup
            "combined logic scenarios"
            [ simulationTestCase @Int
                "complex expression: (a AND b) OR (NOT c)"
                4
                [ ("a", [1, 1, 0, 0])
                , ("b", [1, 0, 1, 0])
                , ("c", [0, 1, 0, 1])
                ]
                [ receive ["a"]
                , receive ["b"]
                , receive ["c"]
                , logicAnd "a" "b" ["tmp"]
                , logicNot "c" ["not_c"]
                , logicOr "tmp" "not_c" ["result"]
                , send "result"
                ]
                ("result", [1, 0, 1, 0])
            ]
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
