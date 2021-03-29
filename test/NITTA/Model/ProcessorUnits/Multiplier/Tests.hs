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

import Data.Default
import Data.String.Interpolate
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Tests.Functions ()
import NITTA.Intermediate.Types
import NITTA.LuaFrontend.Tests.Utils
import NITTA.Model.MultiplierDsl
import NITTA.Model.Networks.Types
import NITTA.Model.ProcessorUnits
import NITTA.Model.ProcessorUnits.Tests.Utils
import NITTA.Model.Tests.Microarchitecture
import Test.QuickCheck
import Test.Tasty (testGroup)

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
        , multiplierTest "multiplier smoke test" u $ do
            bindFunc fDef
            _ <- assertBindFullness
            doDecisionSafe $ beTargetAt 1 2 "a"
            doNDecision 0
            doDecisionSafe $ beSourceAt 5 5 ["c"]
            doFstDecision
            _ <- assertProcessDone
            assertExecute
        , multiplierTest "accum smoke test" u3 $ do
            bindFunc fSub
            doDecisionSafe $ beTargetAt 4 4 "a"
            doFstDecision
            doDecisionSafe $ beSourceAt 5 5 ["c"]
            _ <- assertProcessDone
            assertExecute
        , multiplierNegTest "should error, when proccess is not done" u $ do
            bindFunc fDef
            doDecisionSafe $ beTargetAt 1 2 "a"
            doNDecision 0
            _ <- assertProcessDone
            assertExecute
        , multiplierNegTest "shouldn't bind, when PU incompatible with F" u $ do
            bindFunc fSub
            assertExecute
        , multiplierNegTest "shouldn't bind, when different signatures" u3 $ do
            bindFunc fSub
            _ <- assertBindFullness -- TODO: Why Accum return "Acc" as a label instead "-"?
            assertExecute
        , multiplierNegTest "shouldn't " u4 $ do
            bindFunc fSub
            assertExecute
        ]
    where
        u = multiplier True :: Multiplier String Int Int
        u2 = multiplier True :: Multiplier String (Attr (IntX 16)) Int
        u3 = def :: Accum String Int Int
        u4 = def :: Fram String Int Int
        fsGen =
            algGen
                [ fmap packF (arbitrary :: Gen (Multiply _ _))
                ]
