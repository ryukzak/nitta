{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Control.Monad.State
import Data.FileEmbed (embedStringFile)
import qualified Data.HashMap.Strict as HM
import Data.String.Interpolate
import Data.Text as T
import Language.Lua
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.LuaFrontend
import NITTA.LuaFrontend.Tests.Providers
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.TH

case_find_startup_function =
    let src =
            [__i|
                function sum(a)
                    local t = 2
                    local r = -1
                    sum(a + t + r)
                end
                sum(1)
            |]
        (actualName, FunCall (NormalFunCall _ (Args actualArgValue)), FunAssign _ (FunBody actualArg _ _)) = findStartupFunction (getLuaBlockFromSources src)
        expectedValues = ("sum", [Name "a"], [Number IntNum "1"])
     in (actualName, actualArg, actualArgValue) @?= expectedValues

case_process_local_assignment_statement =
    let assignment = LocalAssign [Name "a"] (Just [Number IntNum "2"])
        expected = HM.fromList [("a", LuaValueVersion{luaValueVersionName = "a", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False})]
        (_, LuaAlgBuilder{algLatestLuaValueVersion}) = runState (processStatement "_" assignment) defaultAlgBuilder
     in algLatestLuaValueVersion @?= expected

case_process_assignment_statement =
    let assignment = Assign [VarName (Name "a")] [Number IntNum "2"]
        expected = HM.fromList [("a", LuaValueVersion{luaValueVersionName = "a", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False})]
        (_, LuaAlgBuilder{algLatestLuaValueVersion}) = runState (processStatement "_" assignment) defaultAlgBuilder
     in algLatestLuaValueVersion @?= expected

--case_process_multiple_assignments_statement =
--    let assignment = Assign [VarName (Name $  "a"), VarName (Name $  "b")] [Number IntNum $  "2", Number FloatNum $  "2.5"]
--        expected =
--            Map.fromList
--                [ ( "a", LuaValueVersion{luaValueVersionName =  "a", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False})
--                , ( "b", LuaValueVersion{luaValueVersionName =  "b", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False})
--                ]
--        (_str :: String, AlgBuilder{algLatestLuaValueVersion}) = runState (processStatement ( "_") assignment) defaultAlgBuilder
--     in algLatestLuaValueVersion @?= expected

case_process_add_statement =
    let assignment = Assign [VarName (Name "a")] [Binop Language.Lua.Add (Number IntNum "1") (Number IntNum "2")]
        expected =
            [ LuaStatement{fIn = ["!1#0", "!2#0"], fOut = [LuaValueVersion{luaValueVersionName = "a", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}], fValues = [], fName = "add", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = "2", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [2], fName = "constant", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = "1", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [1], fName = "constant", fInt = []}
            ]
        (_, LuaAlgBuilder{algGraph}) = runState (processStatement "_" assignment) defaultAlgBuilder
     in algGraph @?= expected

case_process_sub_statement =
    let assignment = Assign [VarName (Name "a")] [Binop Language.Lua.Sub (Number IntNum "1") (Number IntNum "2")]
        expected =
            [ LuaStatement{fIn = ["!1#0", "!2#0"], fOut = [LuaValueVersion{luaValueVersionName = "a", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}], fValues = [], fName = "sub", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = "2", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [2], fName = "constant", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = "1", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [1], fName = "constant", fInt = []}
            ]
        (_, LuaAlgBuilder{algGraph}) = runState (processStatement "_" assignment) defaultAlgBuilder
     in algGraph @?= expected

case_process_divide_statement =
    let assignment = Assign [VarName (Name "a")] [Binop Div (Number IntNum "1") (Number IntNum "2")]
        expected =
            [ LuaStatement{fIn = ["!1#0", "!2#0"], fOut = [LuaValueVersion{luaValueVersionName = "a", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}], fValues = [], fName = "divide", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = "2", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [2], fName = "constant", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = "1", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [1], fName = "constant", fInt = []}
            ]
        (_, LuaAlgBuilder{algGraph}) = runState (processStatement "_" assignment) defaultAlgBuilder
     in algGraph @?= expected

case_process_multiply_statement =
    let assignment = Assign [VarName (Name "a")] [Binop Mul (Number IntNum "1") (Number IntNum "2")]
        expected =
            [ LuaStatement{fIn = ["!1#0", "!2#0"], fOut = [LuaValueVersion{luaValueVersionName = "a", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}], fValues = [], fName = "multiply", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = "2", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [2], fName = "constant", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = "1", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [1], fName = "constant", fInt = []}
            ]
        (_, LuaAlgBuilder{algGraph}) = runState (processStatement "_" assignment) defaultAlgBuilder
     in algGraph @?= expected

--case_process_neg_statement =
--   let assignment = Assign [VarName (Name ( "x"))] [Unop Neg (PrefixExp (PEVar (VarName (Name ( "y")))))]
--       expected =
--               [ LuaStatement{fIn = [ "y"], fOut = [ "a"], fValues = [], fName = "neg", fInt = []}]
--       (_str :: String, AlgBuilder{algGraph}) = runState (processStatement ( "_") assignment) defaultAlgBuilder
--    in algGraph @?= expected
--
--case_process_funcall_statement =
--   let assignment = FunCall (NormalFunCall (PEVar (VarName (Name ( "send")))) (Args [PrefixExp (PEVar (VarName (Name ( "x"))))]))
--       expected =
--               [ LuaStatement{fIn = [ "a"], fOut = [ "a"], fValues = [], fName = "constant", fInt = []}]
--       (_str :: String, AlgBuilder{algGraph}) = runState (processStatement ( "_") assignment) defaultAlgBuilder
--    in algGraph @?= expected

case_temporary_variable =
    let assignment = Assign [VarName (Name "a")] [Binop Language.Lua.Add (Binop Language.Lua.Add (Number IntNum "1") (Number IntNum "2")) (Number IntNum "3")]
        expected =
            [ LuaStatement{fIn = ["_0#a", "!3#0"], fOut = [LuaValueVersion{luaValueVersionName = "a", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}], fValues = [], fName = "add", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = "3", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [3], fName = "constant", fInt = []}
            , LuaStatement{fIn = ["!1#0", "!2#0"], fOut = [LuaValueVersion{luaValueVersionName = "_0#a", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}], fValues = [], fName = "add", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = "2", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [2], fName = "constant", fInt = []}
            , LuaStatement{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = "1", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [1], fName = "constant", fInt = []}
            ]
        (_, LuaAlgBuilder{algGraph}) = runState (processStatement "_" assignment) defaultAlgBuilder
     in algGraph @?= expected

case_lua_two_name_for_same_constant =
    let src =
            [__i|
                function sum(a)
                    local t = 1
                    local r = 1
                    sum(a + t + r)
                end
                sum(0)
            |]
        dfg =
            [ constant 1 ["t^0#0"] :: F T.Text Int
            , constant 1 ["r^0#0"]
            , add "a^0#0" "t^0#0" ["_1#loop"]
            , add "_1#loop" "r^0#0" ["_0#loop"]
            , loop 0 "_0#loop" ["a^0#0"]
            ]
     in functions (frDataFlow $ lua2functions src) @?= dfg

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
            [ neg "a^0#0" ["b^0#0"] :: F T.Text Int
            , loop 0 "b^0#0" ["a^0#0"]
            ]
     in functions (frDataFlow $ lua2functions src) @?= dfg

defaultAlgBuilder =
    LuaAlgBuilder
        { algGraph = []
        , algLatestLuaValueVersion = HM.empty
        , algVarCounters = HM.empty
        , algVars = HM.empty
        , algStartupArgs = HM.empty
        , algTraceFuncs = []
        } ::
        LuaAlgBuilder String Int

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
            [ constant 2 ["t^0#0"] :: F T.Text Int
            , constant (-1) ["r^0#0"]
            , add "a^0#0" "t^0#0" ["_1#loop"]
            , add "_1#loop" "r^0#0" ["_0#loop"]
            , loop 0 "_0#loop" ["a^0#0"]
            ]
     in functions (frDataFlow $ lua2functions src) @?= dfg

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
            [ constant 2 ["!2#0"] :: F T.Text Int
            , add "b^0#0" "!2#0" ["a&^0#0"]
            , constant 1 ["!1#0"]
            , add "a^0#0" "!1#0" ["b&^0#0"]
            , loop 0 "a&^0#0" ["a^0#0"]
            , loop 0 "b&^0#0" ["b^0#0"]
            ]
     in functions (frDataFlow $ lua2functions src) @?= dfg

--test_lua_two_name_for_same_constant =
--    let src =
--            [__i|
--                function sum(a)
--                    local t = 1
--                    local r = 1
--                    sum(a + t + r)
--                end
--                sum(0)
--            |]
--     in -- TODO: it is a correct code and should be translated to dataflow graph
--        -- correctly, but in the current LuaFrontend stage, it cannot be done
--        -- without a huge redesign of LuaFrontend
--        [ expectFail $
--            testCase "contant with same name in lua source code" $ do
--                catch
--                    ( do
--                        let !_ = functions $ frDataFlow $ lua2functionsNew src :: [F String Int]
--                        return ()
--                    )
--                    (\(_ :: ErrorCall) -> assertFailure "fail")
--                assertBool "check temporal restriction" True
--        ]

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
        -- TODO: traceLuaSimulationTestCase pInt "variable before and after changing"
    ]

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
