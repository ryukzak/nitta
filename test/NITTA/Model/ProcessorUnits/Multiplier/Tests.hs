{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Text as T
import NITTA.FrontEnds.LuaFrontend.Tests.Providers
import NITTA.Model.ProcessorUnits.Tests.Providers
import NITTA.Model.Tests.Providers
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
        , unitTestCase "multiplier detail test" u $ do
            assign $ multiply "a" "b" ["c"]
            setValue "a" 2
            setValue "b" 12

            assertEndpoint 0 maxBound $ consume "a"
            assertLocks [Lock{locked = "c", lockBy = "a"}, Lock{locked = "c", lockBy = "b"}]
            decideAt 0 0 $ consume "a"

            assertEndpoint 1 maxBound $ consume "b"
            assertLocks [Lock{locked = "c", lockBy = "b"}]
            decideAt 1 1 $ consume "b"

            assertEndpoint 4 maxBound $ provide ["c"]
            assertLocks []
            decideAt 4 4 $ provide ["c"]

            assertLocks []
            assertPUCoSimulation
        , unitTestCase "multiplier coSim smoke test" u $ do
            assign $ multiply "a" "b" ["c", "d"]
            setValue "a" 2
            setValue "b" 7
            decide $ consume "a"
            decide $ consume "b"
            decide $ provide ["c", "d"]
            assertPUCoSimulation
        ]
    where
        u = multiplier True :: Multiplier T.Text Int Int
        u2 = multiplier True :: Multiplier T.Text (Attr (IntX 16)) Int
        fsGen =
            algGen
                [ fmap packF (arbitrary :: Gen (Multiply _ _))
                ]
