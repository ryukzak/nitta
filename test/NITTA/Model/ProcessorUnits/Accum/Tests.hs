{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS -fno-warn-partial-type-signatures #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Accum.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Accum.Tests (
    tests,
) where

import Data.Default
import qualified Data.Set as S
import Data.String.Interpolate
import NITTA.Intermediate.Functions as F
import NITTA.LuaFrontend.Tests.Providers
import NITTA.Model.ProcessorUnits.Tests.Providers
import NITTA.Model.Tests.Providers
import Test.QuickCheck
import Test.Tasty (testGroup)

tests =
    testGroup
        "Accum PU"
        [ nittaCoSimTestCase
            "simple"
            march
            [ constant 5 ["a"]
            , loop 1 "d" ["b", "c"]
            , accFromStr "+a + b + c = d"
            ]
        , nittaCoSimTestCase
            "medium"
            march
            [ loop 1 "i" ["a"]
            , loop 1 "d" ["b"]
            , constant (-1) ["c", "e", "f", "g", "h"]
            , accFromStr "+a + b + c = d; +e + f -g -h = i;"
            ]
        , nittaCoSimTestCase
            "hard"
            march
            [ loop 1 "d" ["a"]
            , loop 1 "i" ["b"]
            , loop 1 "l" ["c"]
            , loop 1 "m" ["e"]
            , constant (-10) ["f", "g", "h", "j", "k"]
            , accFromStr "+a + b + c = d; +e + f -g -h = i; -j + k = l = m"
            ]
        , nittaCoSimTestCase
            "many_simul_outputs_grouped"
            march
            [ loop 1 "d" ["a"]
            , loop 1 "e" ["b"]
            , loop 1 "f" ["c"]
            , acc
                [ Push Plus (I "a")
                , Push Plus (I "b")
                , Push Plus (I "c")
                , Pull (O $ S.fromList ["d", "e", "f"])
                ]
            ]
        , nittaCoSimTestCase
            "many_simul_outputs_not_grouped"
            march
            [ loop 1 "d" ["a"]
            , loop 1 "e" ["b"]
            , loop 1 "f" ["c"]
            , acc
                [ Push Plus (I "a")
                , Push Plus (I "b")
                , Push Plus (I "c")
                , Pull (O $ S.fromList ["d"])
                , Pull (O $ S.fromList ["e"])
                , Pull (O $ S.fromList ["f"])
                ]
            ]
        , puCoSimTestCase
            "as buffer"
            accumDef
            [("a", 99)]
            [ accFromStr "+a = c;"
            ]
        , puCoSimTestCase
            "add with overflow"
            u2
            [("a", 100), ("b", 100)]
            [ accFromStr "+a +b = c;"
            ]
        , puCoSimTestCase
            "as add"
            accumDef
            [("a", 1), ("b", 2)]
            [ accFromStr "+a +b = c;"
            ]
        , puCoSimTestCase
            "triple"
            accumDef
            [("a", 1), ("b", 2), ("e", 4)]
            [ accFromStr "+a +b -e = c;"
            ]
        , puCoSimTestCase
            "two same outputs"
            accumDef
            [("a", 1), ("b", 2), ("e", 4)]
            [ accFromStr "+a +b -e = c = d;"
            ]
        , puCoSimTestCase
            "multiple outputs"
            accumDef
            [("a", 1), ("b", 2), ("e", 4), ("f", -4)]
            [ accFromStr "+a +b = c = d; +e -f = g;"
            ]
        , puCoSimTestCase
            "complex test"
            accumDef
            [("a", 1), ("b", 2), ("e", 4), ("f", -4), ("j", 8)]
            [ accFromStr "+a +b = c = d; +e -f = g; +j = k"
            ]
        , luaTestCase
            "test_accum_optimization_and_deadlock_resolve"
            -- TODO: We need to check that synthesis process do all needed refactoring
            [__i|
                function sum(a, b, c)
                    local d = a + b + c -- should AccumOptimization
                    local e = d + 1 -- e and d should be buffered
                    local f = -d
                    sum(d, f, e)
                end
                sum(0,0,0)
            |]
        , typedLuaTestCase
            (microarch ASync SlaveSPI)
            pFX22_32
            "fixpoint 22 32"
            [__i|
                function f()
                    send(0.5 - 0.25)
                    send(-1.25 + 2.5)
                end
                f()
            |]
        , typedLuaTestCase
            (microarch ASync SlaveSPI)
            pFX42_64
            "fixpoint 42 64"
            [__i|
                function f()
                    send(0.5 - 0.25)
                    send(-1.25 + 2.5)
                end
                f()
            |]
        , finitePUSynthesisProp "finite synthesis process" accumDef fsGen
        , puCoSimProp "co simulation" accumDef fsGen
        , unitTestCase "accum smoke test" accumDef $ do
            assign $ sub "a" "b" ["c"]
            assertBindFullness
            decide $ consume "a"
            decide $ consume "b"
            decide $ provide ["c"]
            assertSynthesisDone
        , unitTestCase "accum neg func test" accumDef $ do
            assign $ F.neg "a" ["b"]
            setValue "a" 2

            assertEndpoint 1 maxBound $ consume "a"
            --assertLocks [Lock{locked = "b", lockBy = "a"}]
            decideAt 1 1 $ consume "a"

            assertEndpoint 4 maxBound $ provide ["b"]
            assertLocks []
            decideAt 4 4 $ provide ["b"]

            assertLocks []
            assertCoSimulation
        ]
    where
        accumDef = def :: Accum String Int Int
        u2 = def :: Accum String (Attr (IntX 8)) Int
        fsGen = algGen [packF <$> (arbitrary :: Gen (Acc _ _))]
