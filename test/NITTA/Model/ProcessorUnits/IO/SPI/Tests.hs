{- FOURMOLU_DISABLE -}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -fno-warn-partial-type-signatures #-}

{-|
Module      : NITTA.Model.ProcessorUnits.IO.SPI.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.IO.SPI.Tests
    ( tests
    ) where

import           Control.Monad ( void )
import           Data.Default
import           NITTA.Intermediate.DataFlow
import qualified NITTA.Intermediate.Functions as F
import           NITTA.Intermediate.Tests.Functions ()
import           NITTA.LuaFrontend.Tests.Utils
import           NITTA.Model.Networks.Types
import           NITTA.Model.Tests.Microarchitecture
import           NITTA.TargetSynthesis
import           Test.Tasty ( TestTree, testGroup )
import           Test.Tasty.HUnit
import           Test.Tasty.TH
import           Text.InterpolatedString.Perl6 ( qc )


test_multiple_receives =
    [ testCase "receive two variables" $ void $ runTargetSynthesisWithUniqName (def :: TargetSynthesis _ _ _ Int)
        { tName="receive_two_variables"
        , tMicroArch=marchSPI True pInt
        , tReceivedValues=[ ("a", [10..15]), ("b", [20..25]) ]
        , tDFG=fsToDataFlowGraph
            [ F.receive ["a"]
            , F.receive ["b"]
            , F.add "a" "b" ["c"]
            , F.send "c"
            ]
        }

    , testCase "receive variable two times" $ void $ runTargetSynthesisWithUniqName (def :: TargetSynthesis _ _ _ Int)
        { tName="receive_variable_two_times"
        , tMicroArch=marchSPI True pInt
        , tReceivedValues=[ ("a", [10..15]), ("b", [20..25]) ]
        , tDFG=fsToDataFlowGraph
            [ F.receive ["a", "b"]
            , F.add "a" "b" ["c"]
            , F.send "c"
            ]
        }

    ]


-- TODO: proof test work
test_different_type =
    [ typedIOLuaTestCase (microarch Sync SlaveSPI) pIntX32 "pIntX32 IO by sync slave"
        received alg
    , typedIOLuaTestCase (microarch Sync SlaveSPI) pIntX48 "pIntX48 IO by sync slave"
        received alg
    , typedIOLuaTestCase (microarch Sync SlaveSPI) pIntX64 "pIntX64 IO by sync slave"
        received alg
    , typedIOLuaTestCase (microarch Sync SlaveSPI) pIntX128 "pIntX128 IO by sync slave"
        received alg
    , typedIOLuaTestCase (microarch Sync MasterSPI) pIntX32 "pIntX32 IO by sync master"
        received alg
    , typedIOLuaTestCase (microarch Sync MasterSPI) pIntX48 "pIntX48 IO by sync master"
        received alg
    , typedIOLuaTestCase (microarch Sync MasterSPI) pIntX64 "pIntX64 IO by sync master"
        received alg
    , typedIOLuaTestCase (microarch Sync MasterSPI) pIntX128 "pIntX128 IO by sync master"
        received alg
    ]
    where
        received =
            [ ( "a:0", [ 10..15 ] )
            , ( "b:0", [ 20..25 ] )
            ]
        alg = [qc|
            function sum()
                local a = receive()
                local b = receive()
                send(a + b)
                sum()
            end
            sum()
            |]


tests :: TestTree
tests = $(testGroupGenerator)
