{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module      : NITTA.Model.ProcessorUnits.Serial.Shift.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Serial.Shift.Tests
    ( tests
    ) where

import           NITTA.Intermediate.Functions
import           NITTA.Model.ProcessorUnits.Tests.Utils
import           NITTA.Model.Tests.Microarchitecture
import           Test.Tasty ( testGroup )
import           Text.InterpolatedString.Perl6 ( qc )
import           NITTA.LuaFrontend.Tests.Utils


tests = testGroup "Shift PU"
    [ nittaCoSimTestCase "shift test" march
        [ loop 16 "g1" ["f1"]
        , shiftL 1 "f1" ["g1"]
        , loop 16 "g2" ["f2"]
        , shiftR 1 "f2" ["g2"]
        ]

    , luaTestCase "shift lua 1" [qc|
        function shift(x)
            local tmp = x << 1
            shift(tmp)
        end
        shift(1)
        |]

    , luaTestCase "shift lua 2" [qc|
        function shift(x)
            local tmp = x << 2
            shift(tmp)
        end
        shift(1)
        |]

    , luaTestCase "shift left 4 and shift right 3" [qc|
        function shift(x)
            local tmp = x << 4
            local tmp2 = tmp >> 3
            shift(tmp2)
        end
        shift(1)
        |]

    , luaTestCase "shift left 8 and shift right 7" [qc|
        function shift(x)
            local tmp = x << 8
            local tmp2 = tmp >> 7
            shift(tmp2)
        end
        shift(1)
        |]

    ]
