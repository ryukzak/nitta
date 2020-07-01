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
    ) where

import           Data.CallStack
import           Data.Default
import           Data.Either
import qualified Data.String.Utils             as S
import qualified Data.Text                     as T
import           NITTA.Model.Networks.Types
import           NITTA.Project
import           NITTA.TargetSynthesis
import           NITTA.Test.Microarchitectures
import           NITTA.Utils
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.TH
import           Text.InterpolatedString.Perl6 (qc)


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


tests :: TestTree
tests = $(testGroupGenerator)


-----------------------------------------------------------


luaTestCase :: HasCallStack => String -> T.Text -> TestTree
luaTestCase name src = testCase name $ do
    let wd = "lua_" ++ toModuleName name
    status <- runLua wd src
    let errMsg = codeBlock (T.unpack src) ++ "-- runTargetSynthesis fail with: " ++ fromLeft undefined status
    assertBool errMsg $ isRight status

runLua wd src = do
    report <- runTargetSynthesis (def :: TargetSynthesis String String Int Int)
        { tName=wd
        , tMicroArch=microarch ASync SlaveSPI
        , tSourceCode=Just src
        , tReceivedValues=def
        }
    return $ case report of
        Right TestbenchReport{ tbStatus=True } -> Right ()
        Left err -> Left $ "synthesis process fail: " ++ err
        Right TestbenchReport{ tbCompilerDump } | length tbCompilerDump > 2 -- [ "stdout:", "stderr:" ]
            -> Left $ "icarus synthesis error:\n" ++ S.join "\n" tbCompilerDump
        Right TestbenchReport{ tbSimulationDump }
            -> Left $ "icarus simulation error:\n" ++ S.join "\n" tbSimulationDump
