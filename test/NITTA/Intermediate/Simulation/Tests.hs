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
import NITTA.Utils.Tests (testCaseM)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

simulationTests =
    testGroup
        "functional simulation"
        [ testCaseM "reorder algorithm" $ do
            let action = reorderAlgorithm :: [F String Int] -> [F String Int]
                l1 = loop 0 "b2" ["a1"]
                l2 = loop 1 "c" ["b1", "b2"]
                a = add "a1" "b1" ["c"]
            mapM_ ([l1, l2, a] @=?) $ map action [[l1, l2, a], [l1, a, l2], [a, l1, l2]]
        , testGroup
            "estimate waves"
            [ testCaseM "function sequence" $ do
                let action = estimateVarWaves ["a", "b"] :: [F String Int] -> M.Map String Int
                    a1 = add "a" "b" ["c"]
                    a2 = add "c" "b" ["d"]
                    a3 = add "d" "a" ["e"]
                mapM_ (M.fromList [("c", 0), ("d", 1), ("e", 2)] @=?) $ map action $ permutations [a1, a2, a3]
            , testCaseM "loop output vars have zero wave" $ do
                let action = estimateVarWaves [] :: [F String Int] -> M.Map String Int
                    l1 = loop 1 "b2" ["a1"]
                    l2 = loop 1 "c" ["b1", "b2"]
                    a = add "a1" "b1" ["c"]
                M.fromList [("a1", 0), ("b1", 0), ("b2", 0), ("c", 1)] @=? action [l1, l2, a]
            ]
        , simulationTestCase
            "fibonacci sequence"
            7
            def
            [ loop 0 "b2" ["a1"]
            , loop 1 "c" ["b1", "b2"]
            , add "a1" "b1" ["c"]
            ]
            ("a1", [0, 1, 1, 2, 3, 5, 8])
        , simulationTestCase
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
        ]

tests =
    testGroup
        "intermediate simulation"
        [ simulationTests
        ]

simulationTestCase ::
    HasCallStack =>
    String ->
    Int ->
    [(String, [Int])] ->
    [F String Int] ->
    (String, [Int]) ->
    TestTree
simulationTestCase name n received alg (v, expect) =
    testCaseM name $
        let dfg = fsToDataFlowGraph alg
            Cntx{cntxProcess} = simulateDataFlowGraph n def received dfg
            actual = map (\(CycleCntx c) -> fromMaybe (error $ show c) (HM.lookup v c)) cntxProcess
         in expect @=? actual
