{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
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
    ( luaTests
    ) where

import           Data.Default
import           Data.Either                   (isRight)
import           Data.FileEmbed                (embedStringFile)
import           Data.Proxy
import qualified Data.String.Utils             as S
import qualified Data.Text                     as T
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
        =<< lua "rexp_not_in_paren" (pIntX32, microarch True SlaveSPI)
            [qc|function f(b)
                    a = b + 1
                    f(a)
                end
                f(0)
            |]
    , testCase "a = b + 1" $ either assertFailure return
        =<< lua "rexp_not_in_paren" (pIntX32, microarch False SlaveSPI)
            [qc|function f(b)
                    a = b + 1
                    f(a)
                end
                f(0)
            |]
    , testCase "a = (b + 1)" $ either assertFailure return
        =<< lua "lua_rexp_in_paren" (pIntX32, microarch True SlaveSPI)
            [qc|function f(b)
                    a = (b + 1)
                    f(a)
                end
                f(0)
            |]
    , testCase "counter(i + 1)" $ either assertFailure return
        =<< lua "counter" (pIntX32, microarch True SlaveSPI)
            [qc|function counter(i)
                    counter(i + 1)
                end
                counter(0)
            |]
    , testCase "i = i + 1; counter(i)" $ either assertFailure return
        =<< lua "counter_redefinition" (pIntX32, microarch True SlaveSPI)
            [qc|function counter(i)
                    i = i + 1
                    counter(i)
                end
                counter(0)
            |]
    , testCase "local i2 = i + 1; counter(i2)" $ either assertFailure return
        =<< lua "counter_local_var" (pIntX32, microarch True SlaveSPI)
            [qc|function counter(i)
                    local i2 = i + 1
                    counter(i2)
                end
                counter(0)
            |]
    , testCase "i = reg(i + 1); counter(i)" $ either assertFailure return
        =<< lua "counter_reg" (pIntX32, microarch True SlaveSPI)
            [qc|function counter(i)
                    i = reg(i + 1)
                    counter(i)
                end
                counter(0)
            |]
    , testCase "i = i + 1; counter(reg(i))" $ either assertFailure return
        =<< lua "counter_reg" (pIntX32, microarch True SlaveSPI)
            [qc|function counter(i)
                    i = i + 1
                    counter(reg(i))
                end
                counter(0)
            |]
    , testCase "send(10 - 20); send(-30 + 40)" $ either assertFailure return
        =<< lua "subtraction" (pIntX32, microarch True SlaveSPI)
            [qc|function f()
                    send(10 - 20)
                    send(-30 + 40)
                end
                f()
            |]
    , testCase "send(10 * -1); send(-20 * -30)" $ either assertFailure return
        =<< lua "multiplication" (pIntX32, microarch True SlaveSPI)
            [qc|function f()
                    send(10 * -1)
                    send(-20 * -30)
                end
                f()
            |]
    , testCase "x = 10; send(x * x)" $ either assertFailure return
        =<< lua "quad" (pIntX32, microarch True SlaveSPI)
            [qc|function f()
                    local x = 10
                    send(x * x)
                end
                f()
        |]
    , testCase "a, b = -10 / 2; c, d = 10 / -2" $ either assertFailure return
        =<< lua "division" (pIntX32, microarch True SlaveSPI)
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
        =<< lua "def_b_a" (pIntX32, microarch True SlaveSPI)
            [qc|function fib(a, b)
                    b, a = a + b, b
                    fib(a, b)
                end
                fib(0, 1)
            |]
    , testCase "a, b = b, reg(reg(a) + reg(b))" $ either assertFailure return
        =<< lua "nested_reg" (pIntX32, microarch True SlaveSPI)
            [qc|function fib(a, b)
                    a, b = b, reg(reg(a) + reg(b))
                    fib(a, b)
                end
                fib(0, 1)|]
    , testCase "a, b = b, reg(a + reg(b + 0)) + 0" $ either assertFailure return
        =<< lua "nested_reg_and_0" (pIntX32, microarch True SlaveSPI)
            [qc|function fib(a, b)
                    a, b = b, reg(a + reg(b + 0)) + 0
                    fib(a, b)
                end
                fib(0, 1)|]
    ]


test_examples =
    [ testCase "examples/teacup.lua wait" $ either assertFailure return
        =<< lua "teacup_wait" (pFX22_32, microarch False SlaveSPI) $(embedStringFile "examples/teacup.lua")
    , testCase "examples/teacup.lua drop" $ either assertFailure return
        =<< lua "teacup_drop" (pFX22_32, microarch True SlaveSPI) $(embedStringFile "examples/teacup.lua")
    , testCase "examples/fibonacci.lua wait" $ either assertFailure return
        =<< lua "fibonacci_wait" (pFX22_32, microarch False SlaveSPI) $(embedStringFile "examples/fibonacci.lua")
    -- FIXME: uncomment when IO synchronization propogation and SPI will be fixed.
    -- , testCase "examples/fibonacci.lua drop" $ either assertFailure return
    --     =<< lua "fibonacci_drop" (pFX22_32, microarch True SlaveSPI) $(embedStringFile "examples/fibonacci.lua")
    -- , testCase "examples/pid.lua wait" $ either assertFailure return
    --     =<< lua "pid_wait" (pFX22_32, microarch False SlaveSPI) $(embedStringFile "examples/pid.lua")
    -- , testCase "examples/pid.lua drop" $ either assertFailure return
    --     =<< lua "pid_drop" (pFX22_32, microarch True SlaveSPI) $(embedStringFile "examples/pid.lua")
    ]


test_fixpoint_add =
    [ testCase "send(0.5 - 0.25); send(-1.25 + 2.5)" $ either assertFailure return
        =<< lua "add" (pFX22_32, microarch True SlaveSPI) add
    , testCase "send(0.5 - 0.25); send(-1.25 + 2.5)" $ either assertFailure return
        =<< lua "add" (pFX42_64, microarch True SlaveSPI) add
    ] where add =
                [qc|function f()
                        send(0.5 - 0.25)
                        send(-1.25 + 2.5)
                    end
                    f()
                |]


test_fixpoint_mul =
    [ testCase "send(0.5 * -0.5); send(-20.5 * -2)" $ either assertFailure return
        =<< lua "mul" (pFX22_32, microarch True SlaveSPI) mul
    , testCase "send(0.5 * -0.5); send(-20.5 * -2)" $ either assertFailure return
        =<< lua "mul" (pFX42_64, microarch True SlaveSPI) mul
    ] where mul =
                [qc|function f()
                        send(0.5 * -0.5)
                        send(-20.5 * -2)
                    end
                    f()
                |]


test_fixpoint_div =
    [ testCase "one time" $ either assertFailure return
        =<< lua "one_time" (pIntX32, microarch True SlaveSPI)
            [qc|function f(a)
                    a, _b = a / 2
                    f(a)
                end
                f(1024)
            |]
    , testCase "two time" $ either assertFailure return
        =<< lua "two_time" (pIntX32, microarch True SlaveSPI)
            [qc|function f(a, b)
                    a, _ = a / 2
                    b, _ = b / 3
                    f(a, b)
                end
                f(1024, 1024)
            |]
    , testCase "a, b = -1.25 / 0.5; c, d = 75 / -2" $ either assertFailure return
        =<< lua "div" (pFX22_32, microarch True SlaveSPI) alg
    , testCase "a, b = -1.25 / 0.5; c, d = 75 / -2" $ either assertFailure return
        =<< lua "div" (pFX42_64, microarch True SlaveSPI) alg
    ] where alg =
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


test_sum_of_received_values =
    [ testCase "sum of received pIntX32" $ either assertFailure return
        =<< luaEx "sum_of_received_pIntX32" (pIntX32, microarch False SlaveSPI) received alg
    , testCase "sum of received pIntX48" $ either assertFailure return
        =<< luaEx "sum_of_received_pIntX48" (pIntX48, microarch False SlaveSPI) received alg
    , testCase "sum of received pIntX64" $ either assertFailure return
        =<< luaEx "sum_of_received_pIntX64" (pIntX64, microarch False SlaveSPI) received alg
    , testCase "sum of received pIntX128" $ either assertFailure return
        =<< luaEx "sum_of_received_pIntX128" (pIntX128, microarch False SlaveSPI) received alg
    , testCase "sum of received pIntX32" $ either assertFailure return
        =<< luaEx "sum_of_received_pIntX32" (pIntX32, microarch False MasterSPI) received alg
    , testCase "sum of received pIntX48" $ either assertFailure return
        =<< luaEx "sum_of_received_pIntX48" (pIntX48, microarch False MasterSPI) received alg
    , testCase "sum of received pIntX64" $ either assertFailure return
        =<< luaEx "sum_of_received_pIntX64" (pIntX64, microarch False MasterSPI) received alg
    , testCase "sum of received pIntX128" $ either assertFailure return
        =<< luaEx "sum_of_received_pIntX128" (pIntX128, microarch False MasterSPI) received alg
    ] where
        received = [ ("a:0", [10..15]), ("b:0", [20..25]) ]
        alg = $(embedStringFile "examples/sum.lua")


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


lua tName (_proxy, ma) src = luaEx tName (_proxy, ma) [] src


luaEx :: ( Val x, Integral x ) => String -> (Proxy x, BusNetwork String String x Int) -> [(String, [x])] -> T.Text -> IO ( Either String () )
luaEx tName (_proxy, ma) received src = do
    report <- runTargetSynthesis' (def :: TargetSynthesis _ _ _ Int)
        { tName="lua_" ++ tName
        , tMicroArch=ma
        , tSourceCode=Just src
        , tReceivedValues=received
        }
    return $ case report of
        Right TestbenchReport{ tbStatus=True } -> Right ()
        Left err -> Left $ "synthesis process fail: " ++ err
        Right TestbenchReport{ tbCompilerDump } | length tbCompilerDump > 2 -- [ "stdout:", "stderr:" ]
            -> Left $ "icarus synthesis error:\n" ++ S.join "\n" tbCompilerDump
        Right TestbenchReport{ tbSimulationDump }
            -> Left $ "icarus simulation error:\n" ++ S.join "\n" tbSimulationDump
