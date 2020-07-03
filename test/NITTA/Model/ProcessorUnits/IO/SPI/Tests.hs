{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}

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

import           NITTA.Intermediate.Tests.Functions  ()
import           NITTA.LuaFrontend.Tests             hiding (tests)
import           NITTA.Model.Networks.Types
import           NITTA.Model.Tests.Microarchitecture
import           Test.Tasty                          (testGroup)
import           Text.InterpolatedString.Perl6       (qc)


tests = testGroup "SPI PU"
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
