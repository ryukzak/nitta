{-# LANGUAGE PartialTypeSignatures #-}

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


tests = testGroup "Shift PU"
    [ nittaCoSimTestCase "shift test" march
        [ constant 1 ["1"]
        , constant 2 ["2"]
        , constant 3 ["3"]
        , loop 16 "g1" ["f1"]
        , shiftL "f1" "3" ["g1"]
        , loop 16 "g2" ["f2"]
        , shiftR "f2" "3" ["g2"]
        ]
    ]
