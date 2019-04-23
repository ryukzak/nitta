{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
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

import           Control.Monad                 (unless)
import           Data.Default
import           Data.Either                   (isRight)
import           Data.FileEmbed                (embedStringFile)
import           Data.Proxy
import           Data.Text                     (pack)
import           NITTA.DataFlow
import           NITTA.Frontend
import           NITTA.Project
import           NITTA.SynthesisMethod
import           NITTA.Test.Microarchitectures
import           NITTA.Types
import           NITTA.Types.Project
import           NITTA.Types.Synthesis         (Node (..), mkNodeIO)
import           NITTA.Utils                   (fixIndent)
import           NITTA.Utils.Test
import           System.FilePath               (joinPath)
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.TH
import           Text.InterpolatedString.Perl6 (qc)


test_luaSupport =
    [ luaSimpleTestCase "a = b + 1" "lua_rexp_not_in_paren"
        [qc|function f(b)
                a = b + 1
                f(a)
            end
            f(0)
        |]
    , luaSimpleTestCase "a = (b + 1)" "lua_rexp_in_paren"
        [qc|function f(b)
                a = (b + 1)
                f(a)
            end
            f(0)
        |]
    ]


test_counter =
    [ intLuaTestCases "simple" "simple"
        [qc|function counter(i)
                i = i + 1
                counter(i)
            end
            counter(0)
        |]
    , intLuaTestCases "with_local_var" "with_local_var"
        [qc|function counter(i)
                local i2 = i + 1
                counter(i2)
            end
            counter(0)
        |]
    , intLuaTestCases "with_reg" "with_reg"
        [qc|function counter(i)
                i = reg(i + 1)
                counter(i)
            end
            counter(0)
        |]
    ]


test_signed =
    [ intLuaTestCases "sub" "sub"
        [qc|function counter()
                send(10 - 20)
                send(-30 + 40)
            end
            counter()
        |]
    , intLuaTestCases "mul" "mul"
        [qc|function counter()
                send(10 * -1)
                send(-20 * -30)
            end
            counter()
        |]
    , intLuaTestCases "quad" "quad"
        [qc|function counter()
                local x = 10
                send(x * x)
            end
            counter()
        |]
    , intLuaTestCases "div" "div"
        [qc|function counter()
                a, b = -10 / 2
                send(a)
                send(b)
                c, d = 10 / -2
                send(c)
                send(d)
            end
            counter()
        |]
    ]


test_fibonacci =
    [ intLuaTestCases "def_b_a" "def_b_a"
        [qc|function fib(a, b)
                b, a = a + b, b
                fib(a, b)
            end
            fib(0, 1)
        |]
    , intLuaTestCases "nested_reg" "nested_reg"
        [qc|function fib(a, b)
                a, b = b, reg(reg(a) + reg(b))
                fib(a, b)
            end
            fib(0, 1)|]
    , intLuaTestCases "nested_reg_and_0" "nested_reg_and_0"
        [qc|function fib(a, b)
                a, b = b, reg(a + reg(b + 0)) + 0
                fib(a, b)
            end
            fib(0, 1)|]
    ]


test_teacup =
    [ fixpLuaTestCases "example" "example" $(embedStringFile "examples/teacup.lua")
    ]


test_fixedpoint =
    [ fixpLuaTestCases "sub" "sub"
        [qc|function counter()
                send(0.5 - 0.25)
                send(-1.25 + 2.5)
            end
            counter()
        |]
    , fixpLuaTestCases "mul" "mul"
        [qc|function counter()
                send(0.5 * -0.5)
                send(-20.5 * -2)
            end
            counter()
        |]
    , fixpLuaTestCases "div" "div"
        [qc|function counter()
                a, b = -1.25 / 0.5
                send(a)
                send(b)
                c, d = 75 / -2
                send(c)
                send(d)
            end
            counter()
        |]
    ]


test_io =
    [ intIOLuaTestCases "double_receive" [("a:0", [10..15]),("b:0", [20..25])] $ pack $ fixIndent [qc|
|       function fib()
|          local a = receive()
|          local b = receive()
|          local c = a + b
|          send(c)
|          fib()
|       end
|       fib()
|       |]
    ]


test_refactor =
    [ testCase "insertOutRegister" $ do
        let alg = lua2functions
                [qc|function fib(x)
                    y = x + x + x
                    fib(y)
                end
                fib(1)|]
            ma = march
            symthesisMethod = smartBindSynthesisIO
        node <- symthesisMethod =<< mkNodeIO (mkModelWithOneNetwork ma alg)

        unless (isSchedulingComplete $ nModel node) $ error "synthesis process is not completed!"

        let prj = Project
                { projectName="insertOutRegister"
                , libraryPath="../.."
                , projectPath=joinPath ["hdl", "gen", "insertOutRegister"]
                , processorModel=processor $ nModel node
                , testCntx=Nothing
                , targetPlatforms=[ Makefile ]
                }
        TestBenchReport{ tbStatus } <- writeAndRunTestBench prj
        unless tbStatus $ error "simulation do not complience to the functional model"
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
    , genericLuaTestCase projectName [] src marchSPIDropData (Proxy :: Proxy (IntX 40))
    , genericLuaTestCase projectName [] src marchSPIDropData (Proxy :: Proxy (IntX 48))
    , genericLuaTestCase projectName [] src marchSPIDropData (Proxy :: Proxy (IntX 64))
    , genericLuaTestCase projectName [] src marchSPIDropData (Proxy :: Proxy (IntX 96))
    , genericLuaTestCase projectName [] src marchSPIDropData (Proxy :: Proxy (IntX 128))
    , genericLuaTestCase projectName [] src marchSPIDropData (Proxy :: Proxy (FX 22 32))
    , genericLuaTestCase projectName [] src marchSPIDropData (Proxy :: Proxy (FX 42 64))
    ]


fixpLuaTestCases testName projectName src = testGroup testName
    [ genericLuaTestCase projectName [] src marchSPIDropData (Proxy :: Proxy (FX 22 32))
    , genericLuaTestCase projectName [] src marchSPIDropData (Proxy :: Proxy (FX 40 64))
    ]


intIOLuaTestCases projectName receiveValues src = testGroup projectName
    [ genericLuaTestCase projectName receiveValues src marchSPI (Proxy :: Proxy Int)
    , genericLuaTestCase projectName receiveValues src marchSPI (Proxy :: Proxy (IntX 24))
    , genericLuaTestCase projectName receiveValues src marchSPI (Proxy :: Proxy (IntX 32))
    , genericLuaTestCase projectName receiveValues src marchSPI (Proxy :: Proxy (IntX 40))
    , genericLuaTestCase projectName receiveValues src marchSPI (Proxy :: Proxy (IntX 48))
    , genericLuaTestCase projectName receiveValues src marchSPI (Proxy :: Proxy (IntX 64))
    , genericLuaTestCase projectName receiveValues src marchSPI (Proxy :: Proxy (IntX 96))
    , genericLuaTestCase projectName receiveValues src marchSPI (Proxy :: Proxy (IntX 128))
    ]
