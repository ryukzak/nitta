{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}

{-|
Module      : NITTA.ProcessorUnits.Serial.Accum.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.ProcessorUnits.Serial.Accum.Tests
    ( tests
    ) where

import           Data.Default
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           NITTA.Model.ProcessorUnits
import           NITTA.ProcessorUnits.Tests.Utils
import           NITTA.Test.Microarchitectures
import           Test.QuickCheck
import           Test.Tasty                       (testGroup)


tests = testGroup "Accum"
    [ nittaCoSimTestCase "alg_simple_acc" march
        [ constant 5 ["a"]
        , loop 1 "d" ["b", "c"]
        , accFromStr "+a + b + c = d"
        ]
    , nittaCoSimTestCase "alg_medium_acc" march
        [ constant (-1) ["a"]
        , loop 1 "i" ["b", "c", "e", "f", "g", "h"]
        , accFromStr "+a + b + c = d; +e + f -g -h = i;"
        ]
    , nittaCoSimTestCase "alg_hard_acc" march
        [ constant (-10) ["a", "e", "k"]
        , loop 1 "l" ["b", "c", "f", "g", "h", "j"]
        , accFromStr "+a + b + c = d; +e + f -g -h = i; -j + k = l = m"
        ]
    , puCoSimTestCase "coSimulationTest0" accumDef [("a", 99)]
        [ accFromStr "+a = c;"
        ]
    , puCoSimTestCase "coSimulationTest1" accumDef [("a", 1), ("b", 2)]
        [ accFromStr "+a +b = c;"
        ]
    , puCoSimTestCase "coSimulationTest2" accumDef [("a", 1), ("b", 2), ("e", 4)]
        [ accFromStr "+a +b -e = c;"
        ]
    , puCoSimTestCase "coSimulationTest3" accumDef [("a", 1), ("b", 2), ("e", 4)]
        [ accFromStr "+a +b -e = c = d;"
        ]
    , puCoSimTestCase "coSimulationTest4" accumDef [("a", 1), ("b", 2), ("e", 4), ("f", -4)]
        [ accFromStr "+a +b = c = d; +e -f = g;"
        ]
    , puCoSimTestCase "coSimulationTest5" accumDef [("a", 1), ("b", 2), ("e", 4), ("f", -4), ("j", 8)]
        [ accFromStr "+a +b = c = d; +e -f = g; +j = k"
        ]
    , finitePUSynthesisProp "acc_isFinish" accumDef fsGen
    , puCoSimProp "acc_coSimulation" accumDef fsGen
    ]
        where
            accumDef = def :: Accum String Int Int
            fsGen = algGen [packF <$> (arbitrary :: Gen (Acc _ _))]
