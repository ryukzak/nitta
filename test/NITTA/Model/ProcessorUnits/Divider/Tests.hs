{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Divider.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Divider.Tests (
    tests,
) where

import Data.Default
import Data.String.Interpolate
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.LuaFrontend.Tests.Utils
import NITTA.Model.Networks.Types
import NITTA.Model.ProcessorUnits
import NITTA.Model.ProcessorUnits.Tests.DSL
import NITTA.Model.ProcessorUnits.Tests.TestCaseTemplates
import NITTA.Model.Tests.Microarchitecture
import Test.Tasty (testGroup)
import Test.Tasty.ExpectedFailure

tests =
    testGroup
        "Divider PU"
        [ nittaCoSimTestCase
            "smoke test"
            march
            [ constant 100 ["a"]
            , loop 2 "e" ["b"]
            , division "a" "b" ["c"] ["d"]
            , add "c" "d" ["e"]
            , constant 200 ["a1"]
            , loop 2 "e1" ["b1"]
            , division "a1" "b1" ["c1"] ["d1"]
            , add "c1" "d1" ["e1"]
            ]
        , puUnitTestCase "division simple" u2 $ do
            assign $ division "a" "b" ["c"] ["d"]
            setValue "a" 64
            setValue "b" 12
            decideAt 1 1 $ consume "a" -- but I would like to write: decide $ consume "a"
            decideAt 2 2 $ consume "b"
            decideAt 10 10 $ provide ["c"]
            decideAt 11 11 $ provide ["d"]
            assertCoSimulation
        , puUnitTestCase "division only mod" u2 $ do
            assign $ division "a" "b" ["c"] []
            setValue "a" 64
            setValue "b" 12
            decideAt 1 1 $ consume "a"
            decideAt 2 2 $ consume "b"
            decideAt 10 10 $ provide ["c"]
            assertCoSimulation
        , puUnitTestCase "division only rem" u2 $ do
            assign $ division "a" "b" [] ["d"]
            setValue "a" 64
            setValue "b" 12
            decideAt 1 1 $ consume "a"
            decideAt 2 2 $ consume "b"
            decideAt 11 11 $ provide ["d"]
            assertCoSimulation
        , puUnitTestCase "division success pipeline" u2 $ do
            assign $ division "a" "b" ["c"] []
            assign $ division "e" "f" ["g"] []
            setValues [("a", 64), ("b", 12), ("e", 64), ("f", 12)]
            decideAt 1 1 $ consume "a"
            decideAt 2 2 $ consume "b"
            decideAt 3 3 $ consume "e"
            decideAt 4 4 $ consume "f"
            decideAt 7 7 $ provide ["c"]
            decideAt 9 9 $ provide ["g"]
            assertCoSimulation
        , expectFail $
            puUnitTestCase "division failed pipeline" u2 $ do
                assign $ division "a" "b" ["c"] []
                assign $ division "e" "f" ["g"] []
                setValues [("a", 64), ("b", 12), ("e", 4), ("f", 2)]
                decideAt 1 1 $ consume "a"
                decideAt 2 2 $ consume "b"
                decideAt 3 3 $ consume "e"
                decideAt 4 4 $ consume "f"
                traceEndpoints
                decideAt 12 12 $ provide ["c"] -- should fail here, specific time matter (only here, in another case should use `decide`)
                decideAt 13 13 $ provide ["g"]
                assertCoSimulation
        , -- FIXME: the test fail with following description:
          --
          -- > division failed pipeline:  - ?Source "c"@(7..∞ /P 1..1)
          -- > FAIL (expected) (0.03s)
          -- >       test/NITTA/Model/ProcessorUnits/Tests/PuUnitTestDsl.hs:160:
          -- >       Simulation failed (expected failure)
          --
          -- wrong error place
          puCoSimTestCase
            "division by zero"
            u2
            [("a", 64), ("b", 0)]
            [ division "a" "b" ["c"] ["d"]
            ]
        , puCoSimTestCase
            "division with overflow"
            u2
            [("a", 64), ("b", 100)]
            [ division "a" "b" ["c"] ["d"]
            ]
        , luaTestCase
            "one division"
            [__i|
                function f(a)
                    a, _b = a / 2
                    f(a)
                end
                f(1024)
            |]
        , luaTestCase
            "two division"
            [__i|
                function f(a, b)
                    a, _ = a / 2
                    b, _ = b / 3
                    f(a, b)
                end
                f(1024, 1024)
            |]
        , typedLuaTestCase
            (microarch ASync SlaveSPI)
            pFX22_32
            "fixed point 22 32"
            [__i|
                function f(a, b)
                    a, b = -1.25 / 0.5
                    send(a)
                    send(b)
                    c, d = 75 / -2
                    send(c)
                    send(d)
                end
                f(1024, 1024)
            |]
        , typedLuaTestCase
            (microarch ASync SlaveSPI)
            pFX42_64
            "fixed point 42 64"
            [__i|
                function f(a, b)
                    a, b = -1.25 / 0.5
                    send(a)
                    send(b)
                    c, d = 75 / -2
                    send(c)
                    send(d)
                end
                f(1024, 1024)
            |]
            -- FIXME: Auto text can't work correctly, because processGen don't take into account the
            -- facts that some variables may go out.
            -- , testProperty "isUnitSynthesisFinish" $ isUnitSynthesisFinish <$> dividerGen
            -- , testProperty "coSimulation" $ fmap (coSimulation "prop_simulation_divider") $ initialCycleCntxGen =<< dividerGen
        ]
    where
        u2 = def :: Divider String (Attr (IntX 16)) Int

-- where
-- _gen = processAlgOnEndpointGen (divider 4 True :: Divider String Int Int)
--     [ fmap F (arbitrary :: Gen (Division _ _))
--     ]
