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

import Data.Default
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
        , testGroup
            "decideAtUnsafe"
            [ unitTestCase "check constrain existance" broken $ do
                assign $ brokenBuffer "a" ["b"]
                assertEndpoint 1 maxBound $ consume "a"
                decideAt 1 1 $ consume "a"
                assertEndpoint 4 maxBound $ provide ["b"]
            , expectFail $
                unitTestCase "check that safe decide fail on wrong decision" broken $ do
                    assign $ brokenBuffer "a" ["b"]
                    setValue "a" 1
                    assertEndpoint 1 maxBound $ consume "a"
                    decideAt 1 1 $ consume "a"
                    assertEndpoint 4 maxBound $ provide ["b"]
                    decideAt 3 3 $ provide ["b"]
            , unitTestCase "check that unsafe decide success and test success pass" broken $ do
                assign $ brokenBuffer "a" ["b"]
                setValue "a" 1
                assertEndpoint 1 maxBound $ consume "a"
                decideAt 1 1 $ consume "a"
                assertEndpoint 4 maxBound $ provide ["b"]
                decideAtUnsafe 3 3 $ provide ["b"] -- incorrect decision
                assertCoSimulation
            ]
        , testGroup
            "assertLocks"
            [ unitTestCase "assertLocks - success" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                decide $ consume "a"
                assertLocks [Lock{locked = "c", lockBy = "b"}, Lock{locked = "d", lockBy = "b"}]
            , expectFail $
                unitTestCase "assertLocks - fail" u $ do
                    assign $ multiply "a" "b" ["c", "d"]
                    decide $ consume "a"
                    assertLocks [Lock{locked = "c", lockBy = "b"}]
            ]
        ]
    where
        u = multiplier True :: Multiplier String Int Int
        broken = def :: Broken String Int Int
