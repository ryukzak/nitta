{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}

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
import           Test.Tasty                             (testGroup)


tests = testGroup "Shift PU"
    [ nittaCoSimTestCase "smoke test" march
        [ loop 16 "g1" ["f1"]
        , shiftL "f1" ["g1"]
        , loop 16 "g2" ["f2"]
        , shiftR "f2" ["g2"]
        ]
    ]
