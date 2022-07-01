{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-partial-type-signatures #-}

{- |
Module      : NITTA.Model.ProcessorUnits.IO.SPI.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.IO.SPI.Tests (
    tests,
) where

import Control.Monad (void)
import Data.Default
import Data.String.Interpolate
import Data.Text qualified as T
import NITTA.Frontends.Lua.Tests.Providers
import NITTA.Intermediate.DataFlow
import NITTA.Model.Tests.Internals
import NITTA.Model.Tests.Providers
import NITTA.Synthesis
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.TH

-- TODO:  https://github.com/ryukzak/nitta/issues/122

test_multiple_receives =
    [ testCase "receive two variables" $
        void $
            runTargetSynthesisWithUniqName
                (def :: TargetSynthesis _ _ _ Int)
                    { tName = "receive_two_variables"
                    , tMicroArch = marchSPI True pInt
                    , tReceivedValues = [("a", [10 .. 15]), ("b", [20 .. 25])]
                    , tDFG =
                        fsToDataFlowGraph
                            [ receive ["a"]
                            , receive ["b"]
                            , add "a" "b" ["c"]
                            , send "c"
                            ]
                    }
    , testCase "receive variable two times" $
        void $
            runTargetSynthesisWithUniqName
                (def :: TargetSynthesis _ _ _ Int)
                    { tName = "receive_variable_two_times"
                    , tMicroArch = marchSPI True pInt
                    , tReceivedValues = [("a", [10 .. 15]), ("b", [20 .. 25])]
                    , tDFG =
                        fsToDataFlowGraph
                            [ receive ["a", "b"]
                            , add "a" "b" ["c"]
                            , send "c"
                            ]
                    }
    ]

test_SPI_slave =
    [ typedIOLuaTestCase
        (microarch Sync SlaveSPI)
        pIntX32
        "pIntX32 IO by sync slave"
        received
        alg
    , typedIOLuaTestCase
        (microarch Sync SlaveSPI)
        pIntX48
        "pIntX48 IO by sync slave"
        received
        alg
    , typedIOLuaTestCase
        (microarch Sync SlaveSPI)
        pIntX64
        "pIntX64 IO by sync slave"
        received
        alg
    , typedIOLuaTestCase
        (microarch Sync SlaveSPI)
        pIntX128
        "pIntX128 IO by sync slave"
        received
        alg
    , typedIOLuaTestCase
        (microarch Sync SlaveSPI)
        pAttrIntX32
        "pAttrIntX32 IO by sync slave"
        received
        alg
    , typedIOLuaTestCase
        (microarch Sync SlaveSPI)
        pAttrFX22_32
        "pAttrFX22_32 IO by sync slave"
        received
        alg
    ]

test_SPI_master =
    [ typedIOLuaTestCase
        (microarch Sync MasterSPI)
        pIntX32
        "pIntX32 IO by sync master"
        received
        alg
    , typedIOLuaTestCase
        (microarch Sync MasterSPI)
        pIntX48
        "pIntX48 IO by sync master"
        received
        alg
    , typedIOLuaTestCase
        (microarch Sync MasterSPI)
        pIntX64
        "pIntX64 IO by sync master"
        received
        alg
    , typedIOLuaTestCase
        (microarch Sync MasterSPI)
        pIntX128
        "pIntX128 IO by sync master"
        received
        alg
    ]

received =
    [ ("a:0", [10 .. 15])
    , ("b:0", [20 .. 25])
    ]

alg =
    [__i|
        function sum()
            local a = receive()
            local b = receive()
            send(a + b)
            sum()
        end
        sum()
    |] ::
        T.Text

tests :: TestTree
tests = $(testGroupGenerator)
