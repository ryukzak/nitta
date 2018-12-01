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
import           NITTA.Test.Microarchitectures
import           NITTA.Types
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

luaTests :: TestTree
luaTests = $(testGroupGenerator)


-----------------------------------------------------------


luaTestCase name lua
    = let
        fn = "lua_" ++ name
    in testGroup name
        [ luaTestCase' fn (Proxy :: Proxy Int) lua
        , luaTestCase' fn (Proxy :: Proxy (IntX 32)) lua
        , luaTestCase' fn (Proxy :: Proxy (IntX 48)) lua
        ]

luaTestCase' fn proxy lua
    = let
        name = showTypeOf proxy
        fn' = fn ++ "_" ++ showTypeOf proxy
    in testCase (name ++ " <" ++ fn' ++ ">") $ do
        res <- testLua fn' (marchSPIDropData proxy) lua
        isRight res @? show res
