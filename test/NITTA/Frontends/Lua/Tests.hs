{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- |
Module      : NITTA.Frontends.Lua.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Frontends.Lua.Tests (
    tests,
) where

import Control.Monad.State
import Data.Default
import Data.FileEmbed (embedStringFile)
import Data.HashMap.Strict qualified as HM
import Data.String.Interpolate
import Data.Text qualified as T
import Language.Lua qualified as Lua
import NITTA.Frontends.Lua
import NITTA.Frontends.Lua.Tests.Providers
import NITTA.Intermediate.Functions qualified as F
import NITTA.Intermediate.Types
import NITTA.Model.ProcessorUnits.Tests.Providers
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.TH

case_find_startup_function_several_args =
    let src =
            [__i|
                function sum(a, b)
                    local t = 2
                    local r = -1
                    sum(b + t + r, a)
                end
                sum(1, 2)
            |]
        (actualName, Lua.FunCall (Lua.NormalFunCall _ (Lua.Args actualArgValue)), Lua.FunAssign _ (Lua.FunBody actualArg _ _)) = findStartupFunction (getLuaBlockFromSources src)
        expectedValues = ("sum", [Lua.Name "a", Lua.Name "b"], [Lua.Number Lua.IntNum "1", Lua.Number Lua.IntNum "2"])
     in (actualName, actualArg, actualArgValue) @?= expectedValues

case_find_startup_function_no_args =
    let src =
            [__i|
                function sum()
                    t = receive()
                    t = t + 1
                    send(t)
                    sum()
                end
                sum()
            |]
        (actualName, Lua.FunCall (Lua.NormalFunCall _ (Lua.Args actualArgValue)), Lua.FunAssign _ (Lua.FunBody actualArg _ _)) = findStartupFunction (getLuaBlockFromSources src)
        expectedValues = ("sum", [], [])
     in (actualName, actualArg, actualArgValue) @?= expectedValues

-- | local a = 2
case_process_local_assignment_statement =
    let assignment = Lua.LocalAssign [Lua.Name "a"] (Just [Lua.Number Lua.IntNum "2"])
        expected = HM.fromList [("a", LuaValueInstance "a" 0 False)]
        (_, LuaAlgBuilder{algLatestLuaValueInstance}) = runState (processStatement "_" assignment) defaultAlgBuilder
     in algLatestLuaValueInstance @?= expected

-- | a = 2
case_process_assignment_statement =
    let assignment = Lua.Assign [Lua.VarName (Lua.Name "a")] [Lua.Number Lua.IntNum "2"]
        expected = HM.fromList [("a", LuaValueInstance "a" 0 False)]
        (_, LuaAlgBuilder{algLatestLuaValueInstance}) = runState (processStatement "_" assignment) defaultAlgBuilder
     in algLatestLuaValueInstance @?= expected

-- | a = 1 + 2
case_process_add_statement =
    let assignment = Lua.Assign [Lua.VarName (Lua.Name "a")] [Lua.Binop Lua.Add (Lua.Number Lua.IntNum "1") (Lua.Number Lua.IntNum "2")]
        expected =
            [ LuaStatement{fIn = ["!1#0", "!2#0"], fOut = [LuaValueInstance "a" 0 False], fValues = [], fName = "add", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueInstance "2" 0 True], fValues = [2], fName = "constant", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueInstance "1" 0 True], fValues = [1], fName = "constant", fInt = []}
            ]
        (_, LuaAlgBuilder{algGraph}) = runState (processStatement "_" assignment) defaultAlgBuilder
     in algGraph @?= expected

-- | a = 1 - 2
case_process_sub_statement =
    let assignment = Lua.Assign [Lua.VarName (Lua.Name "a")] [Lua.Binop Lua.Sub (Lua.Number Lua.IntNum "1") (Lua.Number Lua.IntNum "2")]
        expected =
            [ LuaStatement{fIn = ["!1#0", "!2#0"], fOut = [LuaValueInstance "a" 0 False], fValues = [], fName = "sub", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueInstance "2" 0 True], fValues = [2], fName = "constant", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueInstance "1" 0 True], fValues = [1], fName = "constant", fInt = []}
            ]
        (_, LuaAlgBuilder{algGraph}) = runState (processStatement "_" assignment) defaultAlgBuilder
     in algGraph @?= expected

-- | a = 1 / 2
case_process_divide_statement =
    let assignment = Lua.Assign [Lua.VarName (Lua.Name "a")] [Lua.Binop Lua.Div (Lua.Number Lua.IntNum "1") (Lua.Number Lua.IntNum "2")]
        expected =
            [ LuaStatement{fIn = ["!1#0", "!2#0"], fOut = [LuaValueInstance "a" 0 False], fValues = [], fName = "divide", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueInstance "2" 0 True], fValues = [2], fName = "constant", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueInstance "1" 0 True], fValues = [1], fName = "constant", fInt = []}
            ]
        (_, LuaAlgBuilder{algGraph}) = runState (processStatement "_" assignment) defaultAlgBuilder
     in algGraph @?= expected

-- | a, b = 1 / 2
case_process_divide_mod_rem_statement =
    let assignment = Lua.Assign [Lua.VarName (Lua.Name "a"), Lua.VarName (Lua.Name "b")] [Lua.Binop Lua.Div (Lua.Number Lua.IntNum "1") (Lua.Number Lua.IntNum "2")]
        expected =
            [ LuaStatement
                { fIn = ["!1#0", "!2#0"]
                , fOut =
                    [ LuaValueInstance "a" 0 False
                    , LuaValueInstance "b" 0 False
                    ]
                , fValues = []
                , fName = "divide"
                , fInt = []
                }
            , LuaStatement{fIn = [], fOut = [LuaValueInstance "2" 0 True], fValues = [2], fName = "constant", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueInstance "1" 0 True], fValues = [1], fName = "constant", fInt = []}
            ]
        (_, LuaAlgBuilder{algGraph}) = runState (processStatement "_" assignment) defaultAlgBuilder
     in algGraph @?= expected

-- | _, b = 1 / 2
case_process_divide_rem_statement =
    let assignment = Lua.Assign [Lua.VarName (Lua.Name "_"), Lua.VarName (Lua.Name "b")] [Lua.Binop Lua.Div (Lua.Number Lua.IntNum "1") (Lua.Number Lua.IntNum "2")]
        expected =
            [ LuaStatement
                { fIn = ["!1#0", "!2#0"]
                , fOut =
                    [ LuaValueInstance "_" 0 False
                    , LuaValueInstance "b" 0 False
                    ]
                , fValues = []
                , fName = "divide"
                , fInt = []
                }
            , LuaStatement{fIn = [], fOut = [LuaValueInstance "2" 0 True], fValues = [2], fName = "constant", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueInstance "1" 0 True], fValues = [1], fName = "constant", fInt = []}
            ]
        (_, LuaAlgBuilder{algGraph}) = runState (processStatement "_" assignment) defaultAlgBuilder
     in algGraph @?= expected

-- | a, _ = 1 / 2
case_process_divide_mod_statement =
    let assignment = Lua.Assign [Lua.VarName (Lua.Name "a"), Lua.VarName (Lua.Name "_")] [Lua.Binop Lua.Div (Lua.Number Lua.IntNum "1") (Lua.Number Lua.IntNum "2")]
        expected =
            [ LuaStatement
                { fIn = ["!1#0", "!2#0"]
                , fOut =
                    [ LuaValueInstance "a" 0 False
                    , LuaValueInstance "_" 0 False
                    ]
                , fValues = []
                , fName = "divide"
                , fInt = []
                }
            , LuaStatement{fIn = [], fOut = [LuaValueInstance "2" 0 True], fValues = [2], fName = "constant", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueInstance "1" 0 True], fValues = [1], fName = "constant", fInt = []}
            ]
        (_, LuaAlgBuilder{algGraph}) = runState (processStatement "_" assignment) defaultAlgBuilder
     in algGraph @?= expected

-- | a = 1 * 2
case_process_multiply_statement =
    let assignment = Lua.Assign [Lua.VarName (Lua.Name "a")] [Lua.Binop Lua.Mul (Lua.Number Lua.IntNum "1") (Lua.Number Lua.IntNum "2")]
        expected =
            [ LuaStatement{fIn = ["!1#0", "!2#0"], fOut = [LuaValueInstance "a" 0 False], fValues = [], fName = "multiply", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueInstance "2" 0 True], fValues = [2], fName = "constant", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueInstance "1" 0 True], fValues = [1], fName = "constant", fInt = []}
            ]
        (_, LuaAlgBuilder{algGraph}) = runState (processStatement "_" assignment) defaultAlgBuilder
     in algGraph @?= expected

-- | a = 1 + 2 + 3
case_temporary_variable =
    let assignment = Lua.Assign [Lua.VarName (Lua.Name "a")] [Lua.Binop Lua.Add (Lua.Binop Lua.Add (Lua.Number Lua.IntNum "1") (Lua.Number Lua.IntNum "2")) (Lua.Number Lua.IntNum "3")]
        expected =
            [ LuaStatement{fIn = ["_0#a", "!3#0"], fOut = [LuaValueInstance "a" 0 False], fValues = [], fName = "add", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueInstance "3" 0 True], fValues = [3], fName = "constant", fInt = []}
            , LuaStatement{fIn = ["!1#0", "!2#0"], fOut = [LuaValueInstance "_0#a" 0 False], fValues = [], fName = "add", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueInstance "2" 0 True], fValues = [2], fName = "constant", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueInstance "1" 0 True], fValues = [1], fName = "constant", fInt = []}
            ]
        (_, LuaAlgBuilder{algGraph}) = runState (processStatement "_" assignment) defaultAlgBuilder
     in algGraph @?= expected

case_lua_two_name_for_same_constant =
    let src =
            [__i|
                function sum(a)
                    local t = 1
                    local r = 1
                    sum(a + t + r + 1)
                end
                sum(0)
            |]
        dfg =
            [ F.constant 1 ["!1#2", "t^0#0", "t^0#1"] :: F T.Text Int
            , F.add "a^0#0" "t^0#0" ["_2#loop"]
            , F.add "_2#loop" "t^0#1" ["_1#loop"]
            , F.add "_1#loop" "!1#2" ["_0#loop"]
            , F.loop 0 "_0#loop" ["a^0#0"]
            ]
     in functions (frDataFlow $ translateLua src) @?= dfg

case_lua_negative_operator =
    let src =
            [__i|
                function sum(a)
                    b = -a
                    sum(b)
                end
                sum(0)
            |]
        dfg =
            [ F.neg "a^0#0" ["b^0#0"] :: F T.Text Int
            , F.loop 0 "b^0#0" ["a^0#0"]
            ]
     in functions (frDataFlow $ translateLua src) @?= dfg

defaultAlgBuilder =
    LuaAlgBuilder
        { algGraph = []
        , algLatestLuaValueInstance = HM.empty
        , algVarCounters = HM.empty
        , algVars = HM.empty
        , algStartupArgs = HM.empty
        , algConstants = HM.empty
        , algTraceFuncs = []
        } ::
        LuaAlgBuilder Int

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
            [ F.constant 2 ["t^0#0"] :: F T.Text Int
            , F.constant (-1) ["r^0#0"]
            , F.add "a^0#0" "t^0#0" ["_1#loop"]
            , F.add "_1#loop" "r^0#0" ["_0#loop"]
            , F.loop 0 "_0#loop" ["a^0#0"]
            ]
     in functions (frDataFlow $ translateLua src) @?= dfg

case_lua_complex_assignment =
    let src =
            [__i|
                function sum(a, b)
                    a, b = b + 2, a + 1
                    sum(a, b)
                end
                sum(0, 0)
            |]
        dfg =
            [ F.constant 2 ["!2#0"] :: F T.Text Int
            , F.add "b^0#0" "!2#0" ["a&^0#0"]
            , F.constant 1 ["!1#0"]
            , F.add "a^0#0" "!1#0" ["b&^0#0"]
            , F.loop 0 "a&^0#0" ["a^0#0"]
            , F.loop 0 "b&^0#0" ["b^0#0"]
            ]
     in functions (frDataFlow $ translateLua src) @?= dfg

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
    , luaTestCase
        "variable invertation"
        [__i|
                function sum(a)
                    local b = -a
                    sum(b)
                end
                sum(0)
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
        "loop argument tracing"
        [__i|
            function counter(i)
                counter(i + 1)
            end
            counter(0)
        |]
        [__i|
            | Cycle  | i      |
            |:-------|:-------|
            | 1      | 0.000  |
            | 2      | 1.000  |
            | 3      | 2.000  |
            | 4      | 3.000  |
            | 5      | 4.000  |\n
            |]
    , traceLuaSimulationTestCase
        pInt
        "simple trace"
        [__i|
            function counter(i)
                debug.trace(i)
                counter(i + 1)
            end
            counter(0)
        |]
        [__i|
            | Cycle  | i      |
            |:-------|:-------|
            | 1      | 0.000  |
            | 2      | 1.000  |
            | 3      | 2.000  |
            | 4      | 3.000  |
            | 5      | 4.000  |\n
            |]
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
        [__i|
            | Cycle  | i  |
            |:-------|:---|
            | 1      | 0  |
            | 2      | 1  |
            | 3      | 2  |
            | 4      | 3  |
            | 5      | 4  |\n
            |]
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
        [__i|
            | Cycle  | i  | tmp  |
            |:-------|:---|:-----|
            | 1      | 0  | 1    |
            | 2      | 1  | 2    |
            | 3      | 2  | 3    |
            | 4      | 3  | 4    |
            | 5      | 4  | 5    |\n
            |]
    , traceLuaSimulationTestCase
        pInt
        "default trace"
        [__i|
            function counter(i, two)
                local one = 1
                local tmp = i + one + two
                debug.trace("%.3f", i)
                debug.trace("%.1f", two)
                counter(tmp, 2)
            end
            counter(0, 2)
        |]
        [__i|
            | Cycle  | i       | two  |
            |:-------|:--------|:-----|
            | 1      | 0.000   | 2.0  |
            | 2      | 3.000   | 2.0  |
            | 3      | 6.000   | 2.0  |
            | 4      | 9.000   | 2.0  |
            | 5      | 12.000  | 2.0  |\n
            |]
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
        [__i|
            | Cycle  | i  |
            |:-------|:---|
            | 1      | 1  |
            | 2      | 2  |
            | 3      | 3  |
            | 4      | 4  |
            | 5      | 5  |\n
            |]
    ]

-- TODO: traceLuaSimulationTestCase pInt "variable before and after changing"

test_examples =
    [ unitTestCase "teacup io wait" def $ do
        setNetwork $ microarch Sync SlaveSPI
        setBusType pFX22_32
        assignLua $(embedStringFile "examples/teacup.lua")
        synthesizeAndCoSim
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
        [__i|
            function fib(a, b)
              b = a + b
              fib(a, b)
            end
            fib(0, 1)
            |]
    , typedLuaTestCase
        (microarch Sync SlaveSPI)
        pFX22_32
        "self sending 2 io wait"
        [__i|
            function pid(prev_err)
                local temperature_desired = 50
                local getValueSPI = receive()

                err = temperature_desired - getValueSPI

                local PID = err - prev_err
                send(PID)

                pid(err)
            end
            pid(0)
            |]
    , typedLuaTestCase
        (microarch Sync SlaveSPI)
        pFX32_32
        "pu deadlock"
        [__i|
            function pid(I, prev_err)
              err = 50 - 2

              I = I + 1 * err
              D = 0 * (err - prev_err)

              pid(I, I + D)
            end

            pid(0, 0)
            |]
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
        [__i|
            function f(a, b)
                c = a + b
                d = c + b
                f(c, d)
            end

            f(0, 0)
            |]
    , typedLuaTestCase
        (microarch Sync SlaveSPI)
        pFX32_32
        "spi many outputs"
        [__i|
            function pid(I, prev_err)
              local getValueSPI = receive()
              err = 50 - getValueSPI

              P = 2 * err
              I = (I + 0 * err)
              D = 0 * buffer(err - prev_err)

              local PID = buffer(P + I) + D
              send(PID)

              pid(I, err)
            end

            pid(0, 0)
            |]
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
