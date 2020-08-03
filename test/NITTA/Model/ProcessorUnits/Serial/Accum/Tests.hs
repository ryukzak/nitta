{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-partial-type-signatures #-}

{-|
Module      : NITTA.Model.ProcessorUnits.Serial.Accum.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Serial.Accum.Tests
    ( tests
    ) where

import           Data.Default
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Tests.Functions ()
import           NITTA.Intermediate.Types
import           NITTA.LuaFrontend.Tests.Utils
import           NITTA.Model.Networks.Types
import           NITTA.Model.ProcessorUnits
import           NITTA.Model.ProcessorUnits.Tests.Utils
import           NITTA.Model.Tests.Microarchitecture
import           Test.QuickCheck
import           Test.Tasty ( testGroup )
import           Text.InterpolatedString.Perl6 ( qc )


tests = testGroup "Accum PU"
    [ nittaCoSimTestCase "simple" march
        [ constant 5 ["a"]
        , loop 1 "d" ["b", "c"]
        , accFromStr "+a + b + c = d"
        ]
    , nittaCoSimTestCase "medium" march
        [ constant (-1) ["a"]
        , loop 1 "i" ["b", "c", "e", "f", "g", "h"]
        , accFromStr "+a + b + c = d; +e + f -g -h = i;"
        ]
    , nittaCoSimTestCase "hard" march
        [ constant (-10) ["a", "e", "k"]
        , loop 1 "l" ["b", "c", "f", "g", "h", "j"]
        , accFromStr "+a + b + c = d; +e + f -g -h = i; -j + k = l = m"
        ]
    , puCoSimTestCase "as register" accumDef [("a", 99)]
        [ accFromStr "+a = c;"
        ]
    , puCoSimTestCase "as add" accumDef [("a", 1), ("b", 2)]
        [ accFromStr "+a +b = c;"
        ]
    , puCoSimTestCase "triple" accumDef [("a", 1), ("b", 2), ("e", 4)]
        [ accFromStr "+a +b -e = c;"
        ]
    , puCoSimTestCase "two same outputs" accumDef [("a", 1), ("b", 2), ("e", 4)]
        [ accFromStr "+a +b -e = c = d;"
        ]
    , puCoSimTestCase "multiple outputs" accumDef [("a", 1), ("b", 2), ("e", 4), ("f", -4)]
        [ accFromStr "+a +b = c = d; +e -f = g;"
        ]
    , puCoSimTestCase "complex test" accumDef [("a", 1), ("b", 2), ("e", 4), ("f", -4), ("j", 8)]
        [ accFromStr "+a +b = c = d; +e -f = g; +j = k"
        ]

    , typedLuaTestCase (microarch ASync SlaveSPI) pFX22_32 "fixpoint 22 32" [qc|
        function f()
            send(0.5 - 0.25)
            send(-1.25 + 2.5)
        end
        f()
        |]

    , typedLuaTestCase (microarch ASync SlaveSPI) pFX42_64 "fixpoint 42 64" [qc|
        function f()
            send(0.5 - 0.25)
            send(-1.25 + 2.5)
        end
        f()
        |]

    , finitePUSynthesisProp "finite synthesis process" accumDef fsGen
    , puCoSimProp "co simulation" accumDef fsGen
    ]
        where
            accumDef = def :: Accum String Int Int
            fsGen = algGen [packF <$> (arbitrary :: Gen (Acc _ _))]
