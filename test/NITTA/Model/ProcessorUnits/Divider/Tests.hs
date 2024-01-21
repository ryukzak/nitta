{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
import NITTA.Frontends.Lua.Tests.Providers
import NITTA.Model.ProcessorUnits.Tests.Providers
import NITTA.Model.Tests.Providers
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
        , unitTestCase "division simple" u2 $ do
            assign $ division "a" "b" ["c"] ["d"]
            setValue "a" 64
            setValue "b" 12
            decideAt 0 0 $ consume "a"
            decideAt 1 1 $ consume "b"
            -- traceProcess >> tracePU >> traceEndpoints
            assertEndpoint 7 maxBound $ provide ["c"]
            decideAt 7 7 $ provide ["c"]
            decideAt 8 8 $ provide ["d"]
            assertPUCoSimulation
        , unitTestCase "division only mod" u2 $ do
            assign $ division "a" "b" ["c"] []
            setValue "a" 64
            setValue "b" 12
            decideAt 1 1 $ consume "a"
            decideAt 2 2 $ consume "b"
            decideAt 8 8 $ provide ["c"]
            assertPUCoSimulation
        , unitTestCase "division only rem" u2 $ do
            assign $ division "a" "b" [] ["d"]
            setValue "a" 64
            setValue "b" 12
            decideAt 1 1 $ consume "a"
            decideAt 2 2 $ consume "b"
            decideAt 11 11 $ provide ["d"]
            assertPUCoSimulation
        , unitTestCase "division success pipeline" u2 $ do
            assign $ division "a" "b" ["c"] []
            assign $ division "e" "f" ["g"] []
            setValues [("a", 10), ("b", 2), ("e", 9), ("f", 3)]
            decideAt 0 0 $ consume "a"
            decideAt 1 1 $ consume "b"
            decideAt 3 3 $ consume "e"
            decideAt 4 4 $ consume "f"
            decideAt 7 7 $ provide ["c"]
            decideAt 10 10 $ provide ["g"]
            assertPUCoSimulation
        , unitTestCase "division pipeline on last tick" u2 $ do
            assign $ division "a" "b" ["c"] []
            assign $ division "e" "f" ["g"] []
            setValues [("a", 64), ("b", 12), ("e", 4), ("f", 2)]
            assertLocks
                [ Lock{locked = "c", lockBy = "a"}
                , Lock{locked = "c", lockBy = "b"}
                , Lock{locked = "g", lockBy = "e"}
                , Lock{locked = "g", lockBy = "f"}
                ]
            decideAt 1 1 $ consume "a"
            assertLocks
                [ Lock{locked = "c", lockBy = "b"}
                , Lock{locked = "e", lockBy = "b"}
                , Lock{locked = "f", lockBy = "b"}
                , Lock{locked = "g", lockBy = "b"}
                ]
            decideAt 2 2 $ consume "b"
            assertLocks
                [ Lock{locked = "g", lockBy = "e"}
                , Lock{locked = "g", lockBy = "f"}
                ]
            decideAt 4 4 $ consume "e"
            assertLocks
                [ Lock{locked = "g", lockBy = "f"}
                ]
            decideAt 5 5 $ consume "f"
            assertLocks
                [ Lock{locked = "g", lockBy = "c"}
                ]
            assertEndpoint 8 10 $ provide ["c"]
            decideAt 10 10 $ provide ["c"] -- last tick
            assertLocks []
            assertEndpoint 11 maxBound $ provide ["g"]
            decideAt 13 13 $ provide ["g"]
            assertPUCoSimulation
        , expectFail $
            unitTestCase "division pipeline after result corrupted" u2 $ do
                assign $ division "a" "b" ["c"] []
                assign $ division "e" "f" ["g"] []
                setValues [("a", 64), ("b", 12), ("e", 4), ("f", 2)]
                decideAt 1 1 $ consume "a"
                decideAt 2 2 $ consume "b"
                decideAt 4 4 $ consume "e"
                decideAt 5 5 $ consume "f"
                assertEndpoint 8 10 $ provide ["c"]
                decideAt 11 11 $ provide ["c"] -- fail here because out of available time
        , puCoSimTestCase
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
            "one division explicit"
            [__i|
                function f(a)
                    a, _b = a / 2
                    f(a)
                end
                f(1024)
            |]
        , luaTestCase
            "one division implicit"
            [__i|
                function f(a)
                    a = a / 2
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
        ]
    where
        -- FIXME: Auto text can't work correctly, because processGen don't take into account the
        -- facts that some variables may go out.
        -- , testProperty "isUnitSynthesisFinish" $ isUnitSynthesisFinish <$> dividerGen
        -- , testProperty "coSimulation" $ fmap (coSimulation "prop_simulation_divider") $ initialCycleCntxGen =<< dividerGen

        u2 = def :: Divider String (Attr (IntX 16)) Int

-- where
-- _gen = processAlgOnEndpointGen (divider 4 True :: Divider String Int Int)
--     [ fmap F (arbitrary :: Gen (Division _ _))
--     ]
