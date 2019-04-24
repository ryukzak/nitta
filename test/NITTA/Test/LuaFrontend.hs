{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Test.LuaFrontend
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Test.LuaFrontend
    ( intLuaTestCases
    , luaTests
    ) where

import           Data.Default
import           Data.Either                   (isRight)
import           Data.FileEmbed                (embedStringFile)
import           Data.Proxy
import           NITTA.SynthesisMethod
import           NITTA.Test.Microarchitectures
import           NITTA.Types
import           NITTA.Utils.Test
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.TH
import           Text.InterpolatedString.Perl6 (qc)


test_support =
    [ luaSimpleTestCase "a = b + 1;" "lua_rexp_not_in_paren"
        [qc|function f(b)
                a = b + 1
                f(a)
            end
            f(0)
        |]
    , luaSimpleTestCase "a = (b + 1);" "lua_rexp_in_paren"
        [qc|function f(b)
                a = (b + 1)
                f(a)
            end
            f(0)
        |]
    , intLuaTestCases "counter(i + 1);" "counter"
        [qc|function counter(i)
                counter(i + 1)
            end
            counter(0)
        |]
    , intLuaTestCases "i = i + 1; counter(i);" "counter_redefinition"
        [qc|function counter(i)
                i = i + 1
                counter(i)
            end
            counter(0)
        |]
    , intLuaTestCases "local i2 = i + 1; counter(i2);" "counter_local_var"
        [qc|function counter(i)
                local i2 = i + 1
                counter(i2)
            end
            counter(0)
        |]
    , intLuaTestCases "i = reg(i + 1); counter(i);" "counter_reg"
        [qc|function counter(i)
                i = reg(i + 1)
                counter(i)
            end
            counter(0)
        |]
    , intLuaTestCases "i = i + 1; counter(reg(i));" "counter_reg"
        [qc|function counter(i)
                i = i + 1
                counter(reg(i))
            end
            counter(0)
        |]
    , intLuaTestCases "send(10 - 20); send(-30 + 40);" "subtraction"
        [qc|function f()
                send(10 - 20)
                send(-30 + 40)
            end
            f()
        |]
    , intLuaTestCases "send(10 * -1); send(-20 * -30);" "multiplication"
        [qc|function f()
                send(10 * -1)
                send(-20 * -30)
            end
            f()
        |]
    , intLuaTestCases "x = 10; send(x * x);" "quad"
        [qc|function f()
                local x = 10
                send(x * x)
            end
            f()
        |]
    , intLuaTestCases "a, b = -10 / 2; c, d = 10 / -2" "division"
        [qc|function f()
                a, b = -10 / 2
                send(a)
                send(b)
                c, d = 10 / -2
                send(c)
                send(d)
            end
            f()
        |]
    ]


test_fibonacci =
    [ intLuaTestCases "b, a = a + b, b" "def_b_a"
        [qc|function fib(a, b)
                b, a = a + b, b
                fib(a, b)
            end
            fib(0, 1)
        |]
    , intLuaTestCases "a, b = b, reg(reg(a) + reg(b))" "nested_reg"
        [qc|function fib(a, b)
                a, b = b, reg(reg(a) + reg(b))
                fib(a, b)
            end
            fib(0, 1)|]
    , intLuaTestCases "a, b = b, reg(a + reg(b + 0)) + 0" "nested_reg_and_0"
        [qc|function fib(a, b)
                a, b = b, reg(a + reg(b + 0)) + 0
                fib(a, b)
            end
            fib(0, 1)|]
    ]


test_examples =
    [ fixpLuaTestCases "examples/teacup.lua" "teacup" $(embedStringFile "examples/teacup.lua")
    , fixpLuaTestCases "examples/pid.lua" "pid" $(embedStringFile "examples/pid.lua")
    , intLuaTestCases "examples/fibonacci.lua" "fibonacci" $(embedStringFile "examples/fibonacci.lua")
    ]


test_fixpoint =
    [ fixpLuaTestCases "send(0.5 - 0.25); send(-1.25 + 2.5);" "add"
        [qc|function f()
                send(0.5 - 0.25)
                send(-1.25 + 2.5)
            end
            f()
        |]
    , fixpLuaTestCases "send(0.5 * -0.5); send(-20.5 * -2);" "mul"
        [qc|function f()
                send(0.5 * -0.5)
                send(-20.5 * -2)
            end
            f()
        |]
    , fixpLuaTestCases "a, b = -1.25 / 0.5; c, d = 75 / -2;" "div"
        [qc|function f()
                a, b = -1.25 / 0.5
                send(a)
                send(b)
                c, d = 75 / -2
                send(c)
                send(d)
            end
            f()
        |]
    ]


test_io =
    [ intIOLuaTestCases "sum of received variables" "sum_of_receive" [ ("a:0", [10..15]),("b:0", [20..25] )] [qc|
        function f()
           local a = receive()
           local b = receive()
           local c = a + b
           send(c)
           f()
        end
        f()
        |]
    ]


test_refactor =
    [ testCase "insert register before binding (y = x + x + x)" $ do
        report <- runTest' ((def :: Test String String Int Int)
            { testProjectName="regBeforeBind"
            , microarchitecture=march
            , synthesisMethod=smartBindSynthesisIO
            , sourceCode=Just [qc|
                    function f(x)
                        y = x + x + x
                        f(y)
                    end
                    f(1)
                |]
            })
        isRight report @? show report
    ]


luaTests :: TestTree
luaTests = $(testGroupGenerator)


-----------------------------------------------------------


luaSimpleTestCase testCaseName testProjectName src
    = testCase testCaseName $ do
        report <- runTest' def
            { testProjectName
            , microarchitecture=marchSPIDropData (Proxy :: Proxy Int)
            , sourceCode=Just src
            }
        isRight report @? show report


genericLuaTestCase testProjectName receiveValues src ma xProxy
    = testCase (showTypeOf xProxy) $ do
        report <- runTest' def
            { testProjectName="generic_lua_" ++ testProjectName
            , microarchitecture=ma xProxy
            , sourceCode=Just src
            , receiveValues=map (\(v, x) -> (v, map fromInteger x)) receiveValues
            }
        isRight report @? show report


intLuaTestCases testName projectName src = testGroup testName
    [ genericLuaTestCase projectName [] src marchSPIDropData (Proxy :: Proxy Int)
    , genericLuaTestCase projectName [] src marchSPIDropData (Proxy :: Proxy (IntX 32))
    , genericLuaTestCase projectName [] src marchSPIDropData (Proxy :: Proxy (IntX 48))
    , genericLuaTestCase projectName [] src marchSPIDropData (Proxy :: Proxy (IntX 128))
    ]


fixpLuaTestCases testName projectName src = testGroup testName
    [ genericLuaTestCase projectName [] src marchSPIDropData (Proxy :: Proxy (FX 22 32))
    , genericLuaTestCase projectName [] src marchSPIDropData (Proxy :: Proxy (FX 42 64))
    ]


intIOLuaTestCases testName projectName receiveValues src = testGroup testName
    [ genericLuaTestCase projectName receiveValues src marchSPI (Proxy :: Proxy Int)
    , genericLuaTestCase projectName receiveValues src marchSPI (Proxy :: Proxy (IntX 24))
    , genericLuaTestCase projectName receiveValues src marchSPI (Proxy :: Proxy (IntX 32))
    , genericLuaTestCase projectName receiveValues src marchSPI (Proxy :: Proxy (IntX 40))
    , genericLuaTestCase projectName receiveValues src marchSPI (Proxy :: Proxy (IntX 48))
    , genericLuaTestCase projectName receiveValues src marchSPI (Proxy :: Proxy (IntX 64))
    , genericLuaTestCase projectName receiveValues src marchSPI (Proxy :: Proxy (IntX 96))
    , genericLuaTestCase projectName receiveValues src marchSPI (Proxy :: Proxy (IntX 128))
    ]
