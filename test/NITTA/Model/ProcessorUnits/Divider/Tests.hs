{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

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
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.LuaFrontend.Tests.Utils
import NITTA.Model.Networks.Types
import NITTA.Model.ProcessorUnits
import NITTA.Model.ProcessorUnits.Tests.Utils
import NITTA.Model.Tests.Microarchitecture
import Test.Tasty (testGroup)
import Text.InterpolatedString.Perl6 (qc)

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
            "one division"
            [qc|
        function f(a)
            a, _b = a / 2
            f(a)
        end
        f(1024)
        |]
        , luaTestCase
            "two division"
            [qc|
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
            [qc|
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
            [qc|
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
