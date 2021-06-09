{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Shift.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Shift.Tests (
    tests,
) where

import Data.String.Interpolate
import NITTA.LuaFrontend.Tests.Providers
import NITTA.Model.Tests.Providers
import Test.Tasty (testGroup)

tests =
    testGroup
        "Shift PU"
        [ nittaCoSimTestCase
            "shift test"
            march
            [ loop 16 "g1" ["f1"]
            , shiftL 1 "f1" ["g1"]
            , loop 16 "g2" ["f2"]
            , shiftR 1 "f2" ["g2"]
            ]
        , luaTestCase
            "shift lua 1"
            [__i|
                function shift(x)
                    local tmp = x << 1
                    shift(tmp)
                end
                shift(1)
            |]
        , luaTestCase
            "shift lua 2"
            [__i|
                function shift(x)
                    local tmp = x << 2
                    shift(tmp)
                end
                shift(1)
            |]
        , luaTestCase
            "shift left 16 and shift right 8"
            [__i|
                function shift(x)
                    local tmp = x << 16
                    local tmp2 = tmp >> 8
                    shift(tmp2)
                end
                shift(1)
            |]
        , luaTestCase
            "shift left 9 and shift right 7"
            [__i|
                function shift(x)
                    local tmp = x << 9
                    local tmp2 = tmp >> 7
                    shift(tmp2)
                end
                shift(1)
            |]
        ]
