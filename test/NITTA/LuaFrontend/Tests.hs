{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : NITTA.LuaFrontend.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.LuaFrontend.Tests
    ( tests
    ) where

import           Data.FileEmbed ( embedStringFile )
import           NITTA.LuaFrontend.Tests.Utils
import           NITTA.Model.Networks.Types
import           NITTA.Model.Tests.Microarchitecture
import           Test.Tasty ( TestTree, testGroup )
import           Test.Tasty.TH
import           Text.InterpolatedString.Perl6 ( qc )


test_simple_recursion =
    [ luaTestCase "unary operator" [qc|
        function counter(i)
            counter(-i)
        end
        counter(2)
        |]
    , luaTestCase "binary operator" [qc|
        function counter(i)
            counter(i + 1)
        end
        counter(0)
        |]
    , luaTestCase "binary operator with bracket" [qc|
        function counter(i)
            counter((i + 1))
        end
        counter(0)
        |]
    , luaTestCase "function call" [qc|
        function counter(x)
            counter(reg(x))
        end
        counter(0)
        |]
    , luaTestCase "function call statement" [qc|
        function counter(x)
            send(x)
            counter(x)
        end
        counter(0)
        |]
    ]


test_assignment_and_reassignment =
   [ luaTestCase "assignment statement with new global variable" [qc|
        function f(x)
            y = x + 1
            f(y)
        end
        f(0)
        |]
    , luaTestCase "assignment statement with new global variable and bracket" [qc|
        function f(x)
            y = (x + 1)
            f(y)
        end
        f(0)
        |]
    , luaTestCase "assigment function result" [qc|
       function counter(x)
            y = reg(x + 1)
            counter(y)
        end
        counter(0)
        |]
    , luaTestCase "multiple assigment function result" [qc|
       function counter(a, b)
            a, b = b, a + b
            counter(a, b)
        end
        counter(1, 1)
        |]
    , luaTestCase "assigment function multiple results to global variables" [qc|
        function f(a, b)
            n, d = a / b
            f(n, d + 1)
        end
        f(4, 2)
        |]
    , luaTestCase "assigment function multiple results to local variables" [qc|
        function f(a, b)
            local n, d = a / b
            f(n, d + 1)
        end
        f(4, 2)
        |]

    , luaTestCase "argument variable reassignment" [qc|
        function counter(x)
            x = x + 1
            counter(x)
        end
        counter(0)
        |]
    , luaTestCase "global variable reassignment" [qc|
        function counter(x)
            y = x + 1
            y = y + 1
            counter(y)
        end
        counter(0)
        |]
    , luaTestCase "local variable reassignment" [qc|
        function counter(x)
            local y = x + 1
            y = y + 1
            counter(y)
        end
        counter(0)
        |]
    ]


test_complex_examples =
    [ luaTestCase "fibonacci" [qc|
        function fib(a, b)
            b, a = a + b, b
            fib(a, b)
        end
        fib(0, 1)
        |]
    , luaTestCase "fibonacci with registers" [qc|
        function fib(a, b)
            a, b = b, reg(reg(a) + reg(b))
            fib(a, b)
        end
        fib(0, 1)
        |]
    , luaTestCase "fibonacci with registers and zeros" [qc|
        function fib(a, b)
            a, b = b, reg(a + reg(b + 0)) + 0
            fib(a, b)
        end
        fib(0, 1)
        |]
    ]


test_trace_features =
    [ traceLuaSimulationTestCase pInt "simple trace" [qc|
        function counter(i)
            debug.trace(i)
            counter(i + 1)
        end
        counter(0)
        |] $ unlines
            [ "i    "
            , "0.000"
            , "1.000"
            , "2.000"
            , "3.000"
            , "4.000"
            ]

    , traceLuaSimulationTestCase pInt "specific fmt" [qc|
        function counter(i)
            debug.trace("%.0f", i)
            counter(i + 1)
        end
        counter(0)
        |] $ unlines
            [ "i"
            , "0"
            , "1"
            , "2"
            , "3"
            , "4"
            ]

    , traceLuaSimulationTestCase pInt "specific fmt and multiple variable" [qc|
        function counter(i)
            local tmp = i + 1
            debug.trace("%.0f", i, tmp)
            counter(tmp)
        end
        counter(0)
        |] $ unlines
            [ "i tmp"
            , "0 1  "
            , "1 2  "
            , "2 3  "
            , "3 4  "
            , "4 5  "
            ]

    , traceLuaSimulationTestCase pInt "default trace" [qc|
        function counter(i, two)
            local one = 1
            local tmp = i + one + two
            counter(tmp, 2)
        end
        counter(0, 2)
        |] $ unlines
            [ "i      two  "
            , "0.000  2.000"
            , "3.000  2.000"
            , "6.000  2.000"
            , "9.000  2.000"
            , "12.000 2.000"
            ]

    -- FIXME:
    -- , traceLuaSimulationTestCase pInt "variable before changing" [qc|
    --     function counter(i)
    --         debug.trace("%.0f", i)
    --         i = i + 1
    --         counter(i)
    --     end
    --     counter(0)
    --     |] $ unlines
    --         [ "i"
    --         , "0"
    --         , "1"
    --         , "2"
    --         , "3"
    --         , "4"
    --         ]

    , traceLuaSimulationTestCase pInt "variable after changing" [qc|
        function counter(i)
            i = i + 1
            debug.trace("%.0f", i)
            counter(i)
        end
        counter(0)
        |] $ unlines
            [ "i"
            , "1"
            , "2"
            , "3"
            , "4"
            , "5"
            ]

    -- TODO: traceLuaSimulationTestCase pInt "variable before and after changing"
    ]

-- TODO: actualize
test_examples =
    [ typedLuaTestCase (microarch Sync SlaveSPI) pFX22_32 "teacup io wait"
        $(embedStringFile "examples/teacup.lua")
    , typedLuaTestCase (microarch ASync SlaveSPI) pFX22_32 "teacup io drop"
        $(embedStringFile "examples/teacup.lua")

    , typedLuaTestCase (microarch Sync SlaveSPI) pFX22_32 "fibonacci io wait"
        $(embedStringFile "examples/fibonacci.lua")

    , typedLuaTestCase (microarch ASync SlaveSPI) pFX22_32 "self sending 1 io drop"
        $(embedStringFile "test/lua/self-send1.lua")
    , typedLuaTestCase (microarch Sync SlaveSPI) pFX22_32 "self sending 2 io wait"
        $(embedStringFile "test/lua/self-send2.lua")
    , typedLuaTestCase (microarch Sync SlaveSPI) pFX32_32 "pu deadlock"
        $(embedStringFile "test/lua/pu-deadlock.lua")

    -- FIXME: uncomment when IO synchronization propogation and SPI will be fixed.
    -- , testCase "examples/fibonacci.lua drop" $ either assertFailure return
    --     =<< lua "fibonacci_drop" (pFX22_32, microarch ASync SlaveSPI) $(embedStringFile "examples/fibonacci.lua")
    , typedLuaTestCase (microarch Sync SlaveSPI) pFX32_32 "pid io wait"
        $(embedStringFile "examples/pid.lua")
    , typedLuaTestCase (microarch Sync SlaveSPI) pFX32_32 "fail io wait"
        $(embedStringFile "test/lua/fail.lua")
    , typedLuaTestCase (microarch Sync SlaveSPI) pFX32_32 "spi many outputs"
        $(embedStringFile "test/lua/spi-many-outputs.lua")

    -- , testCase "examples/pid.lua drop" $ either assertFailure return
    --     =<< lua "pid_drop" (pFX22_32, microarch ASync SlaveSPI) $(embedStringFile "examples/pid.lua")
    , typedLuaTestCase (microarch Sync SlaveSPI) pFX22_32 "example spi1 lua"
        $(embedStringFile "examples/spi1.lua")
    , typedLuaTestCase (microarch Sync SlaveSPI) pFX22_32 "example spi2 lua"
        $(embedStringFile "examples/spi2.lua")
    , typedLuaTestCase (microarch Sync SlaveSPI) pFX22_32 "example spi3 lua"
        $(embedStringFile "examples/spi3.lua")
    ]


tests :: TestTree
tests = $(testGroupGenerator)
