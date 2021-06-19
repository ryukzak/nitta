{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Text as T
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
        , unitTestCase "accum detail test" accumDef $ do
            assign $
                acc
                    [ Push Plus $ I "a"
                    , Push Plus $ I "b"
                    , Pull $ O $ S.fromList ["c", "d"]
                    , Push Plus $ I "e"
                    , Pull $ O $ S.fromList ["f"]
                    ]
            setValues [("a", 2), ("b", 12), ("e", 3)]

            assertLocks
                [ Lock{locked = "c", lockBy = "a"}
                , Lock{locked = "d", lockBy = "a"}
                , Lock{locked = "e", lockBy = "a"}
                , Lock{locked = "f", lockBy = "a"}
                , Lock{locked = "c", lockBy = "b"}
                , Lock{locked = "d", lockBy = "b"}
                , Lock{locked = "e", lockBy = "b"}
                , Lock{locked = "f", lockBy = "b"}
                ]
            assertEndpoint 0 maxBound $ consume "a"
            assertEndpoint 0 maxBound $ consume "b"
            decideAt 0 0 $ consume "a"

            assertLocks
                [ Lock{locked = "c", lockBy = "b"}
                , Lock{locked = "d", lockBy = "b"}
                , Lock{locked = "e", lockBy = "b"}
                , Lock{locked = "f", lockBy = "b"}
                ]
            assertEndpoint 1 maxBound $ consume "b"
            decideAt 1 1 $ consume "b"

            assertLocks
                [ Lock{locked = "e", lockBy = "c"}
                , Lock{locked = "f", lockBy = "c"}
                , Lock{locked = "e", lockBy = "d"}
                , Lock{locked = "f", lockBy = "d"}
                ]
            assertEndpoint 4 maxBound $ provide ["c", "d"]
            decideAt 4 4 $ provide ["c"]

            assertLocks
                [ Lock{locked = "e", lockBy = "d"}
                , Lock{locked = "f", lockBy = "d"}
                ]
            assertEndpoint 5 maxBound $ provide ["d"]
            decideAt 5 5 $ provide ["d"]

            assertLocks [Lock{locked = "f", lockBy = "e"}]
            assertEndpoint 6 maxBound $ consume "e"
            decideAt 6 6 $ consume "e"

            assertLocks []
            assertEndpoint 9 maxBound $ provide ["f"]
            decideAt 9 9 $ provide ["f"]

            assertSynthesisDone
            assertCoSimulation
        , unitTestCase "accum detail two function test" accumDef $ do
            assign $ add "a" "b" ["c"]
            setValue "a" 2
            setValue "b" 12

            assign $ add "d" "e" ["f"]
            setValue "d" 3
            setValue "e" 5

            assertLocks
                [ Lock{locked = "c", lockBy = "a"}
                , Lock{locked = "c", lockBy = "b"}
                , Lock{locked = "f", lockBy = "d"}
                , Lock{locked = "f", lockBy = "e"}
                ]
            assertEndpoint 0 maxBound $ consume "a"
            assertEndpoint 0 maxBound $ consume "b"
            assertEndpoint 0 maxBound $ consume "d"
            assertEndpoint 0 maxBound $ consume "e"
            decideAt 0 0 $ consume "a"

            assertLocks
                [ Lock{locked = "c", lockBy = "b"}
                , Lock{locked = "d", lockBy = "b"}
                , Lock{locked = "e", lockBy = "b"}
                , Lock{locked = "f", lockBy = "b"}
                ]
            assertEndpoint 1 maxBound $ consume "b"
            assertAllEndpointRoles [consume "b"]
            decideAt 1 1 $ consume "b"

            assertLocks
                [ Lock{locked = "d", lockBy = "c"}
                , Lock{locked = "e", lockBy = "c"}
                , Lock{locked = "f", lockBy = "c"}
                ]
            assertEndpoint 4 maxBound $ provide ["c"]
            assertAllEndpointRoles [provide ["c"]]

            decideAt 4 4 $ provide ["c"]
            assertLocks
                [ Lock{locked = "f", lockBy = "e"}
                , Lock{locked = "f", lockBy = "d"}
                ]
            assertEndpoint 5 maxBound $ consume "d"
            assertEndpoint 5 maxBound $ consume "e"
            decideAt 5 5 $ consume "d"

            assertLocks [Lock{locked = "f", lockBy = "e"}]
            assertEndpoint 6 maxBound $ consume "e"
            decideAt 6 6 $ consume "e"

            assertLocks []
            assertEndpoint 9 maxBound $ provide ["f"]
            decideAt 9 9 $ provide ["f"]

            assertCoSimulation
            assertSynthesisDone
        , unitTestCase "accum neg test" accumDef $ do
            assign $ F.neg "a" ["c"]
            setValue "a" 2

            assertEndpoint 0 maxBound $ consume "a"
            assertLocks [Lock{locked = "c", lockBy = "a"}]
            decideAt 0 0 $ consume "a"

            assertEndpoint 3 maxBound $ provide ["c"]
            assertLocks []
            decideAt 3 3 $ provide ["c"]

            assertLocks []
            assertCoSimulation
        ]
    where
        accumDef = def :: Accum T.Text Int Int
        u2 = def :: Accum T.Text (Attr (IntX 8)) Int
        fsGen = algGen [packF <$> (arbitrary :: Gen (Acc _ _))]
