{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS -fno-warn-partial-type-signatures #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Multiplier.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Multiplier.Tests (
    tests,
) where

import Data.String.Interpolate
import NITTA.Intermediate.Functions
import qualified NITTA.Intermediate.Functions as F
import NITTA.Intermediate.Tests.Functions ()
import NITTA.Intermediate.Types
import NITTA.LuaFrontend.Tests.Utils
import NITTA.Model.Networks.Types
import NITTA.Model.ProcessorUnits
import NITTA.Model.ProcessorUnits.Tests.PuUnitTestDsl
import NITTA.Model.ProcessorUnits.Tests.Utils
import NITTA.Model.Tests.Microarchitecture
import Test.QuickCheck
import Test.Tasty (testGroup)
import Test.Tasty.ExpectedFailure

tests =
    testGroup
        "Multiplier PU"
        [ nittaCoSimTestCase
            "smoke test"
            march
            [ constant 2 ["a"]
            , loop 1 "c" ["b"]
            , multiply "a" "b" ["c"]
            , constant 3 ["x"]
            , loop 1 "z" ["y"]
            , multiply "y" "x" ["z"]
            ]
        , puCoSimTestCase
            "multiplier with attr"
            u2
            [ ("a", Attr (IntX 1) True)
            , ("b", Attr (IntX 2) False)
            , ("d", Attr (IntX 258) False)
            , ("e", Attr (IntX 258) False)
            ]
            [multiply "a" "b" ["c"], multiply "d" "e" ["f"]]
        , luaTestCase
            "geometric progression"
            [__i|
                function f(x)
                    local tmp = buffer(2 * x)
                    f(tmp)
                end
                f(1)
            |]
        , typedLuaTestCase
            (microarch ASync SlaveSPI)
            pFX22_32
            "fixpoint 22 32"
            [__i|
                function f()
                    send(0.5 * -0.5)
                    send(-20.5 * -2)
                end
                f()
            |]
        , typedLuaTestCase
            (microarch ASync SlaveSPI)
            pFX42_64
            "fixpoint 42 64"
            [__i|
                function f()
                    send(0.5 * -0.5)
                    send(-20.5 * -2)
                end
                f()
            |]
        , finitePUSynthesisProp "isFinish" u fsGen
        , puCoSimProp "multiplier_coSimulation" u fsGen
        , puUnitTestCase "multiplier smoke test" u $ do
            bindFunc fDef
            assertBindFullness
            doDecision $ beTargetAt 1 2 "a"
            doDecisionWithTarget "b"
            doDecision $ beSourceAt 5 5 ["c"]
            doDecisionWithSource ["d"]
            assertSynthesisDone
        , puUnitTestCase "multiplier coSim smoke test" u $ do
            bindFunc fDef
            doDecisionWithTarget "a"
            doDecisionWithTarget "b"
            doDecisionWithSource ["c", "d"]
            assertCoSimulation [("a", 2), ("b", 7)]
        , expectFail $
            puUnitTestCase "coSim test should fail because synthesis not complete" u $ do
                bindFunc fDef
                doDecisionWithTarget "b"
                assertCoSimulation [("a", 2), ("b", 7)]
        , expectFail $
            puUnitTestCase "should error, when proccess is not done" u $ do
                bindFunc fDef
                doDecision $ beTargetAt 1 2 "a"
                assertSynthesisDone
        , expectFail $
            puUnitTestCase "shouldn't bind, when PU incompatible with F" u $ do
                bindFunc fSub
        , expectFail $
            puUnitTestCase "doDecision should error, when Target in Decision is not present" u $ do
                bindFunc fDef
                doDecision $ beTargetAt 1 1 "aa"
        , expectFail $
            puUnitTestCase "Multiplier should error, when Source in Decision is Targets" u $ do
                bindFunc fDef
                doDecision $ beSourceAt 1 1 ["a"]
                assertSynthesisDone -- to force evaluation
        , expectFail $
            puUnitTestCase "doDecision should error, when Target in Decision is Source" u $ do
                bindFunc fDef
                doDecisionWithTarget "a"
                doDecisionWithTarget "b"
                doDecision $ beTargetAt 4 4 "c"
        , expectFail $
            puUnitTestCase "doDecision should error, when Interval is not correct" u $ do
                bindFunc fDef
                doDecision $ beTargetAt 2 2 "a"
                doDecision $ beTargetAt 1 1 "b"
        , expectFail $
            puUnitTestCase "doFstDecision should error, when decisions are spent" u $ do
                bindFunc fDef
                doDecisionWithTarget "a"
                doDecisionWithTarget "b"
                doDecisionWithSource ["c", "d"]
                doFstDecision
        , expectFail $
            puUnitTestCase "doFstDecision should error, when PU is not bound" u $ do
                doFstDecision
        ]
    where
        u = multiplier True :: Multiplier String Int Int
        u2 = multiplier True :: Multiplier String (Attr (IntX 16)) Int
        fDef = F.multiply "a" "b" ["c", "d"] :: F String Int
        fSub = F.sub "a" "b" ["c"] :: F String Int
        fsGen =
            algGen
                [ fmap packF (arbitrary :: Gen (Multiply _ _))
                ]
