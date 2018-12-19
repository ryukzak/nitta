{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

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


test_fibonacci =
    [ luaTestCase "example" $(embedStringFile "examples/teacup.lua")
    , luaTestCase "def_b_a"
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


test_io =
    [ luaTestCaseWithInput "double_receive" [("a_0", [10..15]),("b_0", [20..25])] $ pack $ fixIndent [qc|
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
