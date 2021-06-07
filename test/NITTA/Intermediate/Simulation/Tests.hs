{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import qualified Data.HashMap.Strict as HM
import Data.List (permutations)
import Data.Maybe
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
            let f = reorderAlgorithm :: [F String Int] -> [F String Int]
                l1 = loop 0 "b2" ["a1"]
                l2 = loop 1 "c" ["b1", "b2"]
                a = add "a1" "b1" ["c"]
            mapM_ (([l1, l2, a] @=?) . f) $ permutations [l1, l2, a]
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
    testCase name $
        let dfg = fsToDataFlowGraph alg
            Cntx{cntxProcess} = simulateDataFlowGraph n def received dfg
            actual = map (\(CycleCntx c) -> fromMaybe (error $ show c) (HM.lookup v c)) cntxProcess
         in expect @=? actual
