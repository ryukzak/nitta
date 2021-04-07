{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
Module      : NITTA.LuaFrontend.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.LuaFrontend.Tests (
    tests,
) where

import Control.Exception (ErrorCall, catch)
import Data.FileEmbed (embedStringFile)
import Data.String.Interpolate
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.LuaFrontend
import NITTA.LuaFrontend.Tests.Utils
import NITTA.Model.Networks.Types
import NITTA.Model.Tests.Microarchitecture
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Test.Tasty.TH

case_lua_constant_declatation =
    let src =
            [__i|
                function sum(a)
                    local t = 2
                    local r = -1
                    sum(a + t + r)
                end
                sum(0)
            |]
        dfg =
            [ add "a#0" "t#0" ["tmp_1#0"]
            , add "tmp_1#0" "r#0" ["tmp_0#0"]
            , loop 0 "tmp_0#0" ["a#0"]
            , constant (-1) ["r#0"]
            , constant 2 ["t#0"] :: F String Int
            ]
     in dfg @?= functions (frDataFlow $ lua2functions src)

test_lua_two_name_for_same_constant =
    let src =
            [__i|
                function sum(a)
                    local t = 1
                    local r = 1
                    sum(a + t + r)
                end
                sum(0)
            |]
     in -- TODO: it is a correct code and should be translated to dataflow graph
        -- correctly, but in the current LuaFrontend stage, it cannot be done
        -- without a huge redesign of LuaFrontend
        [ expectFail $
            testCase "contant with same name in lua source code" $ do
                catch
                    ( do
                        let !_ = functions $ frDataFlow $ lua2functions src :: [F String Int]
                        return ()
                    )
                    (\(_ :: ErrorCall) -> assertFailure "fail")
                assertBool "check temporal restriction" True
        ]

test_simple_recursion =
    [ luaTestCase
        "unary operator"
        [__i|
            function counter(i)
                counter(-i)
            end
            counter(2)
        |]
    , luaTestCase
        "binary operator"
        [__i|
            function counter(i)
                counter(i + 1)
            end
            counter(0)
        |]
    , luaTestCase
        "binary operator with bracket"
        [__i|
            function counter(i)
                counter((i + 1))
            end
            counter(0)
        |]
    , luaTestCase
        "function call"
        [__i|
            function counter(x)
                counter(buffer(x))
            end
            counter(0)
        |]
    , luaTestCase
        "function call statement"
        [__i|
            function counter(x)
                send(x)
                counter(x)
            end
            counter(0)
        |]
    ]

test_assignment_and_reassignment =
    [ luaTestCase
        "assignment statement with new global variable"
        [__i|
            function f(x)
                y = x + 1
                f(y)
            end
            f(0)
        |]
    , luaTestCase
        "assignment statement with new global variable and bracket"
        [__i|
            function f(x)
                y = (x + 1)
                f(y)
            end
            f(0)
        |]
    , luaTestCase
        "assigment function result"
        [__i|
           function counter(x)
                y = buffer(x + 1)
                counter(y)
            end
            counter(0)
        |]
    , luaTestCase
        "multiple assigment function result"
        [__i|
            function counter(a, b)
                a, b = b, a + b
                counter(a, b)
            end
            counter(1, 1)
        |]
    , luaTestCase
        "assigment function multiple results to global variables"
        [__i|
            function f(a, b)
                n, d = a / b
                f(n, d + 1)
            end
            f(4, 2)
        |]
    , luaTestCase
        "assigment function multiple results to local variables"
        [__i|
            function f(a, b)
                local n, d = a / b
                f(n, d + 1)
            end
            f(4, 2)
        |]
    , luaTestCase
        "argument variable reassignment"
        [__i|
            function counter(x)
                x = x + 1
                counter(x)
            end
            counter(0)
        |]
    , luaTestCase
        "global variable reassignment"
        [__i|
            function counter(x)
                y = x + 1
                y = y + 1
                counter(y)
            end
            counter(0)
        |]
    , luaTestCase
        "local variable reassignment"
        [__i|
            function counter(x)
                local y = x + 1
                y = y + 1
                counter(y)
            end
            counter(0)
        |]
    ]

test_complex_examples =
    [ luaTestCase
        "fibonacci"
        [__i|
            function fib(a, b)
                b, a = a + b, b
                fib(a, b)
            end
            fib(0, 1)
        |]
    , luaTestCase
        "fibonacci with registers"
        [__i|
            function fib(a, b)
                a, b = b, buffer(buffer(a) + buffer(b))
                fib(a, b)
            end
            fib(0, 1)
        |]
    , luaTestCase
        "fibonacci with registers and zeros"
        [__i|
            function fib(a, b)
                a, b = b, buffer(a + buffer(b + 0)) + 0
                fib(a, b)
            end
            fib(0, 1)
        |]
    ]

test_trace_features =
    [ traceLuaSimulationTestCase
        pInt
        "simple trace"
        [__i|
            function counter(i)
                debug.trace(i)
                counter(i + 1)
            end
            counter(0)
        |]
        $ unlines
            [ "i    "
            , "0.000"
            , "1.000"
            , "2.000"
            , "3.000"
            , "4.000"
            ]
    , traceLuaSimulationTestCase
        pInt
        "specific fmt"
        [__i|
            function counter(i)
                debug.trace("%.0f", i)
                counter(i + 1)
            end
            counter(0)
         |]
        $ unlines
            [ "i"
            , "0"
            , "1"
            , "2"
            , "3"
            , "4"
            ]
    , traceLuaSimulationTestCase
        pInt
        "specific fmt and multiple variable"
        [__i|
            function counter(i)
                local tmp = i + 1
                debug.trace("%.0f", i, tmp)
                counter(tmp)
            end
            counter(0)
        |]
        $ unlines
            [ "i tmp"
            , "0 1  "
            , "1 2  "
            , "2 3  "
            , "3 4  "
            , "4 5  "
            ]
    , traceLuaSimulationTestCase
        pInt
        "default trace"
        [__i|
            function counter(i, two)
                local one = 1
                local tmp = i + one + two
                counter(tmp, 2)
            end
            counter(0, 2)
        |]
        $ unlines
            [ "i      two  "
            , "0.000  2.000"
            , "3.000  2.000"
            , "6.000  2.000"
            , "9.000  2.000"
            , "12.000 2.000"
            ]
    , -- FIXME:
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

      traceLuaSimulationTestCase
        pInt
        "variable after changing"
        [__i|
            function counter(i)
                i = i + 1
                debug.trace("%.0f", i)
                counter(i)
            end
            counter(0)
        |]
        $ unlines
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
    [ typedLuaTestCase
        (microarch Sync SlaveSPI)
        pFX22_32
        "teacup io wait"
        $(embedStringFile "examples/teacup.lua")
    , typedLuaTestCase
        (microarch ASync SlaveSPI)
        pFX22_32
        "teacup io drop"
        $(embedStringFile "examples/teacup.lua")
    , typedLuaTestCase
        (microarch ASync SlaveSPI)
        pAttrFX22_32
        "teacup io drop with attr FX"
        $(embedStringFile "examples/teacup.lua")
    , typedLuaTestCase
        (microarch Sync SlaveSPI)
        pFX22_32
        "fibonacci io wait"
        $(embedStringFile "examples/fibonacci.lua")
    , typedLuaTestCase
        (microarch ASync SlaveSPI)
        pFX22_32
        "self sending 1 io drop"
        $(embedStringFile "test/lua/self-send1.lua")
    , typedLuaTestCase
        (microarch Sync SlaveSPI)
        pFX22_32
        "self sending 2 io wait"
        $(embedStringFile "test/lua/self-send2.lua")
    , typedLuaTestCase
        (microarch Sync SlaveSPI)
        pFX32_32
        "pu deadlock"
        $(embedStringFile "test/lua/pu-deadlock.lua")
    , -- FIXME: uncomment when IO synchronization propogation and SPI will be fixed.
      -- , testCase "examples/fibonacci.lua drop" $ either assertFailure return
      --     =<< lua "fibonacci_drop" (pFX22_32, microarch ASync SlaveSPI) $(embedStringFile "examples/fibonacci.lua")
      typedLuaTestCase
        (microarch Sync SlaveSPI)
        pFX32_32
        "pid io wait"
        $(embedStringFile "examples/pid.lua")
    , typedLuaTestCase
        (microarch Sync SlaveSPI)
        pFX32_32
        "fail io wait"
        $(embedStringFile "test/lua/fail.lua")
    , typedLuaTestCase
        (microarch Sync SlaveSPI)
        pFX32_32
        "spi many outputs"
        $(embedStringFile "test/lua/spi-many-outputs.lua")
    , -- , testCase "examples/pid.lua drop" $ either assertFailure return
      --     =<< lua "pid_drop" (pFX22_32, microarch ASync SlaveSPI) $(embedStringFile "examples/pid.lua")
      typedLuaTestCase
        (microarch Sync SlaveSPI)
        pFX22_32
        "example spi1 lua"
        $(embedStringFile "examples/spi1.lua")
    , typedLuaTestCase
        (microarch Sync SlaveSPI)
        pFX22_32
        "example spi2 lua"
        $(embedStringFile "examples/spi2.lua")
    , typedLuaTestCase
        (microarch Sync SlaveSPI)
        pFX22_32
        "example spi3 lua"
        $(embedStringFile "examples/spi3.lua")
    ]

tests :: TestTree
tests = $(testGroupGenerator)
