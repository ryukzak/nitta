{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures  -fno-warn-partial-type-signatures #-}

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
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Project
import           NITTA.Project.Parts.TestBench
import           NITTA.Synthesis.Method
import           NITTA.Test.Microarchitectures
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.TH
import           Text.InterpolatedString.Perl6 (qc)


test_support =
    [ testCase "a = b + 1" $ either assertFailure return
        =<< lua "rexp_not_in_paren"
            [qc|function f(b)
                    a = b + 1
                    f(a)
                end
                f(0)
            |]
    , testCase "a = (b + 1)" $ either assertFailure return
        =<< lua "lua_rexp_in_paren"
            [qc|function f(b)
                    a = (b + 1)
                    f(a)
                end
                f(0)
            |]
    , testCase "counter(i + 1)" $ either assertFailure return
        =<< lua "counter"
            [qc|function counter(i)
                    counter(i + 1)
                end
                counter(0)
            |]
    , testCase "i = i + 1; counter(i)" $ either assertFailure return
        =<< lua "counter_redefinition"
            [qc|function counter(i)
                    i = i + 1
                    counter(i)
                end
                counter(0)
            |]
    , testCase "local i2 = i + 1; counter(i2)" $ either assertFailure return
        =<< lua "counter_local_var"
            [qc|function counter(i)
                    local i2 = i + 1
                    counter(i2)
                end
                counter(0)
            |]
    , testCase "i = reg(i + 1); counter(i)" $ either assertFailure return
        =<< lua "counter_reg"
            [qc|function counter(i)
                    i = reg(i + 1)
                    counter(i)
                end
                counter(0)
            |]
    , testCase "i = i + 1; counter(reg(i))" $ either assertFailure return
        =<< lua "counter_reg"
            [qc|function counter(i)
                    i = i + 1
                    counter(reg(i))
                end
                counter(0)
            |]
    , testCase "send(10 - 20); send(-30 + 40)" $ either assertFailure return
        =<< lua "subtraction"
            [qc|function f()
                    send(10 - 20)
                    send(-30 + 40)
                end
                f()
            |]
    , testCase "send(10 * -1); send(-20 * -30)" $ either assertFailure return
        =<< lua "multiplication"
            [qc|function f()
                    send(10 * -1)
                    send(-20 * -30)
                end
                f()
            |]
    , testCase "x = 10; send(x * x)" $ either assertFailure return
        =<< lua "quad"
            [qc|function f()
                    local x = 10
                    send(x * x)
                end
                f()
        |]
    , testCase "a, b = -10 / 2; c, d = 10 / -2" $ either assertFailure return
        =<< lua "division"
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
    [ testCase "b, a = a + b, b" $ either assertFailure return
        =<< lua "def_b_a"
            [qc|function fib(a, b)
                    b, a = a + b, b
                    fib(a, b)
                end
                fib(0, 1)
            |]
    , testCase "a, b = b, reg(reg(a) + reg(b))" $ either assertFailure return
        =<< lua "nested_reg"
            [qc|function fib(a, b)
                    a, b = b, reg(reg(a) + reg(b))
                    fib(a, b)
                end
                fib(0, 1)|]
    , testCase "a, b = b, reg(a + reg(b + 0)) + 0" $ either assertFailure return
        =<< lua "nested_reg_and_0"
            [qc|function fib(a, b)
                    a, b = b, reg(a + reg(b + 0)) + 0
                    fib(a, b)
                end
                fib(0, 1)|]
    ]


test_examples =
    [ fixpLuaTestCases "examples/teacup.lua" "teacup" $(embedStringFile "examples/teacup.lua")
    -- FIXME: , fixpLuaTestCases "examples/pid.lua" "pid" $(embedStringFile "examples/pid.lua")
    -- FIXME: , intLuaTestCases "examples/fibonacci.lua" "fibonacci" $(embedStringFile "examples/fibonacci.lua")
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
        report <- runTargetSynthesis' ((def :: TargetSynthesis (BusNetwork String String Int Int) String Int Int)
            { tName="regBeforeBind"
            , tMicroArch=march
            , tSynthesisMethod=smartBindSynthesisIO
            , tSourceCode=Just [qc|
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

lua tName src = do
    report <- runTargetSynthesis' (def :: TargetSynthesis _ _ _ Int)
        { tName="lua_" ++ tName
        , tMicroArch=marchSPIDropData True (Proxy :: Proxy Int)
        , tSourceCode=Just src
        }
    return $ case report of
        Right TestbenchReport{ tbStatus=True } -> Right ()
        Right _ -> Left "test bench fail"
        Left err -> Left $ "synthesis process fail: " ++ err


checkSynthesisResult (Right TestbenchReport{ tbStatus=True }) = return ()
checkSynthesisResult (Right _) = assertFailure "test bench FAIL"
checkSynthesisResult (Left err) = assertFailure $ "synthesis process FAIL: " ++ err


genericLuaTestCase tName tReceivedValues src ma xProxy
    = testCase (showTypeOf xProxy) $ do
        report <- runTargetSynthesis' (def :: TargetSynthesis _ _ _ Int)
            { tName="generic_lua_" ++ tName
            , tMicroArch=ma xProxy
            , tSourceCode=Just src
            , tReceivedValues=map (\(v, x) -> (v, map fromInteger x)) tReceivedValues
            }
        checkSynthesisResult report


intLuaTestCases testName pName src = testGroup testName
    [ genericLuaTestCase pName [] src (marchSPIDropData True) (Proxy :: Proxy Int)
    , genericLuaTestCase pName [] src (marchSPIDropData True) (Proxy :: Proxy (IntX 32))
    , genericLuaTestCase pName [] src (marchSPIDropData True) (Proxy :: Proxy (IntX 48))
    , genericLuaTestCase pName [] src (marchSPIDropData True) (Proxy :: Proxy (IntX 128))
    ]


fixpLuaTestCases testName pName src = testGroup testName
    [ genericLuaTestCase pName [] src (marchSPIDropData True) (Proxy :: Proxy (FX 22 32))
    , genericLuaTestCase pName [] src (marchSPIDropData True) (Proxy :: Proxy (FX 42 64))
    ]


intIOLuaTestCases testName pName tReceivedValues src = testGroup testName
    [ genericLuaTestCase pName tReceivedValues src (marchSPI True) (Proxy :: Proxy Int)
    , genericLuaTestCase pName tReceivedValues src (marchSPI True) (Proxy :: Proxy (IntX 24))
    , genericLuaTestCase pName tReceivedValues src (marchSPI True) (Proxy :: Proxy (IntX 32))
    , genericLuaTestCase pName tReceivedValues src (marchSPI True) (Proxy :: Proxy (IntX 40))
    , genericLuaTestCase pName tReceivedValues src (marchSPI True) (Proxy :: Proxy (IntX 48))
    , genericLuaTestCase pName tReceivedValues src (marchSPI True) (Proxy :: Proxy (IntX 64))
    , genericLuaTestCase pName tReceivedValues src (marchSPI True) (Proxy :: Proxy (IntX 96))
    , genericLuaTestCase pName tReceivedValues src (marchSPI True) (Proxy :: Proxy (IntX 128))
    , genericLuaTestCase pName tReceivedValues src (marchSPI False) (Proxy :: Proxy Int)
    , genericLuaTestCase pName tReceivedValues src (marchSPI False) (Proxy :: Proxy (IntX 24))
    , genericLuaTestCase pName tReceivedValues src (marchSPI False) (Proxy :: Proxy (IntX 32))
    , genericLuaTestCase pName tReceivedValues src (marchSPI False) (Proxy :: Proxy (IntX 40))
    , genericLuaTestCase pName tReceivedValues src (marchSPI False) (Proxy :: Proxy (IntX 48))
    , genericLuaTestCase pName tReceivedValues src (marchSPI False) (Proxy :: Proxy (IntX 64))
    , genericLuaTestCase pName tReceivedValues src (marchSPI False) (Proxy :: Proxy (IntX 96))
    , genericLuaTestCase pName tReceivedValues src (marchSPI False) (Proxy :: Proxy (IntX 128))
    ]
