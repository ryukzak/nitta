{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Test.LuaFrontend
Description :
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Test.LuaFrontend
    ( luaTestCase
    , luaTests
    ) where

import           Data.Either                   (isRight)
import           Data.FileEmbed                (embedStringFile)
import           Data.Proxy
import           Data.Text                     (pack)
import           NITTA.Test.Microarchitectures
import           NITTA.Types
import           NITTA.Utils                   (fixIndent)
import           NITTA.Utils.Test
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.TH
import           Text.InterpolatedString.Perl6 (qc)


test_counter =
    [ luaTestCase "simple"
        [qc|function counter(i)
                i = i + 1
                counter(i)
            end
            counter(0)
        |]
    , luaTestCase "with_local_var"
        [qc|function counter(i)
                local i2 = i + 1
                counter(i2)
            end
            counter(0)
        |]
    , luaTestCase "with_reg"
        [qc|function counter(i)
                i = reg(i + 1)
                counter(i)
            end
            counter(0)
        |]
    ]


test_signed =
    [ luaTestCase "sub"
        [qc|function counter()
                send(10 - 20)
                send(-30 + 40)
            end
            counter()
        |]
    , luaTestCase "mul"
        [qc|function counter()
                send(10 * -1)
                send(-20 * -30)
            end
            counter()
        |]
    , luaTestCase "quad"
        [qc|function counter()
                local x = 10
                send(x * x)
            end
            counter()
        |]
    , luaTestCase "div"
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
    [ luaTestCase "def_b_a"
        [qc|function fib(a, b)
                b, a = a + b, b
                fib(a, b)
            end
            fib(0, 1)
        |]
    , luaTestCase "nested_reg"
        [qc|function fib(a, b)
                a, b = b, reg(reg(a) + reg(b))
                fib(a, b)
            end
            fib(0, 1)|]
    , luaTestCase "nested_reg_and_0"
        [qc|function fib(a, b)
                a, b = b, reg(a + reg(b + 0)) + 0
                fib(a, b)
            end
            fib(0, 1)|]
    ]


test_teacup =
    [ luaTestCaseFP "example" $(embedStringFile "examples/teacup.lua")
    ]


test_fixedpoint =
    [ luaTestCaseFP "sub"
        [qc|function counter()
                send(0.5 - 0.25)
                send(-1.25 + 2.5)
            end
            counter()
        |]
    , luaTestCaseFP "mul"
        [qc|function counter()
                send(0.5 * -0.5)
                send(-20.5 * -2)
            end
            counter()
        |]
    , luaTestCaseFP "div"
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
    [ luaTestCaseWithInput "double_receive" [("a:0", [10..15]),("b:0", [20..25])] $ pack $ fixIndent [qc|
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

luaTests :: TestTree
luaTests = $(testGroupGenerator)


-----------------------------------------------------------

luaTestCase name lua = testGroup name
        [ inner marchSPIDropData (Proxy :: Proxy Int)
        , inner marchSPIDropData (Proxy :: Proxy (IntX 32))
        , inner marchSPIDropData (Proxy :: Proxy (IntX 40))
        , inner marchSPIDropData (Proxy :: Proxy (IntX 48))
        , inner marchSPIDropData (Proxy :: Proxy (IntX 64))
        , inner marchSPIDropData (Proxy :: Proxy (IntX 96))
        , inner marchSPIDropData (Proxy :: Proxy (IntX 128))
        , inner marchSPIDropData (Proxy :: Proxy (FX 22 32))
        , inner marchSPIDropData (Proxy :: Proxy (FX 42 64))
        ]
    where
        fn = "lua_" ++ name
        inner ma xProxy
            = testCase (showTypeOf xProxy ++ " <" ++ fn' ++ ">") $ do
                res <- testLuaWithInput fn' [] (ma xProxy) lua
                isRight res @? show res
            where
                fn' = fn ++ "_" ++ showTypeOf xProxy

luaTestCaseFP name lua = testGroup name
        [ inner marchSPIDropData (Proxy :: Proxy (FX 22 32))
        , inner marchSPIDropData (Proxy :: Proxy (FX 40 64))
        ]
    where
        fn = "lua_" ++ name
        inner ma xProxy
            = testCase (showTypeOf xProxy ++ " <" ++ fn' ++ ">") $ do
                res <- testLuaWithInput fn' [] (ma xProxy) lua
                isRight res @? show res
            where
                fn' = fn ++ "_" ++ showTypeOf xProxy


luaTestCaseWithInput name is lua = testGroup name
        [ inner marchSPI (Proxy :: Proxy Int)
        , inner marchSPI (Proxy :: Proxy (IntX 24))
        , inner marchSPI (Proxy :: Proxy (IntX 32))
        , inner marchSPI (Proxy :: Proxy (IntX 40))
        , inner marchSPI (Proxy :: Proxy (IntX 48))
        , inner marchSPI (Proxy :: Proxy (IntX 64))
        , inner marchSPI (Proxy :: Proxy (IntX 96))
        , inner marchSPI (Proxy :: Proxy (IntX 128))
        ]
    where
        fn = "lua_" ++ name
        inner ma xProxy
            = testCase (showTypeOf xProxy ++ " <" ++ fn' ++ ">") $ do
                res <- testLuaWithInput fn' (map (\(v, x) -> (v, map toEnum x)) is) (ma xProxy) lua
                isRight res @? show res
            where
                fn' = fn ++ "_" ++ showTypeOf xProxy
