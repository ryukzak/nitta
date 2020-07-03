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
Module      : NITTA.LuaFrontend.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.LuaFrontend.Tests
    ( tests
    , luaTestCase, typedLuaTestCase, typedIOLuaTestCase
    ) where

import           Data.CallStack
import           Data.Default
import           Data.Either
import           Data.FileEmbed                      (embedStringFile)
import           Data.Proxy
import qualified Data.String.Utils                   as S
import qualified Data.Text                           as T
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Networks.Types
import           NITTA.Model.Tests.Microarchitecture
import           NITTA.Project
import           NITTA.TargetSynthesis
import           NITTA.Utils
import           Test.Tasty                          (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.TH
import           Text.InterpolatedString.Perl6       (qc)


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


test_examples =
    [ typedLuaTestCase (microarch Sync SlaveSPI) pFX22_32 "teacup io wait"
        $(embedStringFile "examples/teacup.lua")
    , typedLuaTestCase (microarch ASync SlaveSPI) pFX22_32 "teacup io drop"
        $(embedStringFile "examples/teacup.lua")

    , typedLuaTestCase (microarch Sync SlaveSPI) pFX22_32 "fibonacci io wait"
        $(embedStringFile "examples/fibonacci.lua")

    , typedLuaTestCase (microarch ASync SlaveSPI) pFX22_32 "self sending 1 io drop"
        $(embedStringFile "test/lua/self-send1.lua")
    , typedLuaTestCase (microarch Sync SlaveSPI) pFX22_32 "self sending2 io wait"
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


-----------------------------------------------------------


luaTestCase :: HasCallStack => String -> T.Text -> TestTree
luaTestCase name src = typedIOLuaTestCase (microarch ASync SlaveSPI) pInt name def src


typedLuaTestCase ::
    ( HasCallStack, Val x, Integral x
    ) => BusNetwork String String x Int -> Proxy x -> String -> T.Text -> TestTree
typedLuaTestCase arch proxy name src = typedIOLuaTestCase arch proxy name def src


typedIOLuaTestCase ::
    ( HasCallStack, Val x, Integral x
    ) => BusNetwork String String x Int -> Proxy x -> String
    -> [ ( String, [x] ) ] -> T.Text -> TestTree
typedIOLuaTestCase arch proxy name received src = testCase name $ do
    let wd = "lua_" ++ toModuleName name
    status <- runLua arch proxy wd received src
    let errMsg = codeBlock (T.unpack src) ++ "-- runTargetSynthesis fail with: " ++ fromLeft undefined status
    assertBool errMsg $ isRight status


runLua :: forall x.
    ( Val x, Integral x
    ) => BusNetwork String String x Int -> Proxy x -> String
    -> [ ( String, [x] ) ] -> T.Text -> IO ( Either String () )
runLua arch _proxy wd received src = do
    report <- runTargetSynthesisWithUniqName (def :: TargetSynthesis String String x Int)
        { tName=wd
        , tMicroArch=arch
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
