{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS -fno-warn-partial-type-signatures #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Tests.DSL.Tests
Description :
Copyright   : (c) Artyom Kostyuchik, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Tests.DSL.Tests (
    tests,
) where

import NITTA.Model.ProcessorUnits.Tests.Providers
import Test.Tasty (testGroup)
import Test.Tasty.ExpectedFailure

tests =
    testGroup
        "DSL Tests"
        [ expectFail $
            unitTestCase "should error, when proccess is not done" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                decideAt 1 2 $ consume "a"
                assertSynthesisDone
        , expectFail $
            unitTestCase "should fail coSim, when variables not set" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                decide $ consume "a"
                decide $ consume "b"
                decide $ provide ["c", "d"]
                assertCoSimulation
        , unitTestCase "check assertEndpoint and assertAllEndpointRoles with Target success" u $ do
            assign $ multiply "a" "b" ["c", "d"]
            assertAllEndpointRoles [consume "a", consume "b"]
            assertEndpoint 1 maxBound $ consume "a"
            decide $ consume "a"
        , expectFail $
            unitTestCase "check assertAllEndpointRoles and fail (too many)" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                assertAllEndpointRoles [consume "a", consume "b", consume "c"]
        , expectFail $
            unitTestCase "check assertAllEndpointRoles and fail (too less)" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                assertAllEndpointRoles [consume "a"]
        , unitTestCase "check assertEndpoint with Source success" u $ do
            assign $ multiply "a" "b" ["c", "d"]
            decideAt 1 1 $ consume "a"
            decideAt 2 2 $ consume "b"
            assertEndpoint 5 maxBound $ provide ["c", "d"]
        , unitTestCase "check assertEndpoint with Source success" u $ do
            assign $ multiply "a" "b" ["c", "d"]
            decideAt 1 1 $ consume "a"
            decideAt 2 2 $ consume "b"
            decideAt 5 5 $ provide ["c"]
            assertEndpoint 6 maxBound $ provide ["d"]
        , expectFail $
            unitTestCase "check assertEndpoint fail when wrong interval" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                assertEndpoint 2 2 $ consume "a"
                decide $ consume "a"
        , expectFail $
            unitTestCase "check assertEndpoint success" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                assertEndpoint 2 2 $ consume "a"
                decide $ consume "a"
        , expectFail $
            unitTestCase "should fail coSim, when variables incorrect" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                setValue "a" 1
                setValue "b" 1
                decide $ consume "a"
                decide $ consume "b"
                decide $ provide ["c", "d"]
                assertCoSimulation
        , expectFail $
            unitTestCase "should not bind, when PU incompatible with F" u $
                assign $ sub "a" "b" ["c"]
        , expectFail $
            unitTestCase "decide should error, when Target in Decision is not present" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                decideAt 1 1 $ consume "aa"
        , expectFail $
            unitTestCase "Multiplier should error, when Source in Decision is Targets" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                decideAt 1 1 $ provide ["a"]
        , expectFail $
            unitTestCase "decide should error, when Target in Decision is Source" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                decide $ consume "a"
                decide $ consume "b"
                decideAt 4 4 $ consume "c"
        , expectFail $
            unitTestCase "decide should error, when Interval is not correct" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                decideAt 2 2 $ consume "a"
                decideAt 1 1 $ consume "b"
        , expectFail $
            unitTestCase "should error: breakLoop is not supportd" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                breakLoop 10 "a" ["c"]
        , expectFail $
            unitTestCase "should error: setValue variable is unavailable" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                setValue "e" 10
        , expectFail $
            unitTestCase "should error: setValue variable is alreay set" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                setValue "a" 10
                setValue "b" 11
                setValue "a" 15
        , -- TODO: rewrite that tests to BrokenPU
          testGroup
            "decideAtUnsafe"
            [ unitTestCase "check constrain existance" u $ do
                assign $ multiply "a" "b" ["c"]
                assertEndpoint 1 maxBound $ consume "a"
                decideAtUnsafe 1 1 $ consume "a"
            , expectFail $
                unitTestCase "check that safe fail" u $ do
                    assign $ multiply "a" "b" ["c"]
                    assertEndpoint 1 maxBound $ consume "a"
                    decideAt 0 0 $ consume "a"
            , -- TODO: remove uneccessary restriction for Multiplier model
              unitTestCase "check that unsafe pass" u $ do
                assign $ multiply "a" "b" ["c"]
                setValue "a" 2
                setValue "b" 12
                assertEndpoint 1 maxBound $ consume "a"
                decideAtUnsafe 0 0 $ consume "a"
                decideAtUnsafe 1 1 $ consume "b"
                decideAtUnsafe 10 10 $ provide ["c"]
                assertCoSimulation
            ]
        , testGroup
            "assertLocks"
            [ unitTestCase "assertLocks - success" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                decide $ consume "a"
                assertLocks [Lock{locked = "c", lockBy = "b"}, Lock{locked = "d", lockBy = "b"}]
            , expectFail $
                unitTestCase "assertLocks - success" u $ do
                    assign $ multiply "a" "b" ["c", "d"]
                    decide $ consume "a"
                    assertLocks [Lock{locked = "c", lockBy = "b"}]
            ]
        ]
    where
        u = multiplier True :: Multiplier String Int Int
