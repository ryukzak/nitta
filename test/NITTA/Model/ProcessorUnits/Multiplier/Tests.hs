{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS -fno-warn-partial-type-signatures #-}

{-|
Module      : NITTA.Model.ProcessorUnits.Multiplier.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Multiplier.Tests
    ( tests
    ) where

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


tests = testGroup "Multiplier PU"
    [ nittaCoSimTestCase "smoke test" march
        [ constant 2 ["a"]
        , loop 1 "c" ["b"]
        , multiply "a" "b" ["c"]

        , constant 3 ["x"]
        , loop 1 "z" ["y"]
        , multiply "y" "x" ["z"]
        ]

    , luaTestCase "geometric progression" [qc|
        function f(x)
            local tmp = reg(2 * x)
            f(tmp)
        end
        f(1)
        |]

    , typedLuaTestCase (microarch ASync SlaveSPI) pFX22_32 "fixpoint 22 32" [qc|
        function f()
            send(0.5 * -0.5)
            send(-20.5 * -2)
        end
        f()
        |]

    , typedLuaTestCase (microarch ASync SlaveSPI) pFX42_64 "fixpoint 42 64" [qc|
        function f()
            send(0.5 * -0.5)
            send(-20.5 * -2)
        end
        f()
        |]

    , finitePUSynthesisProp "isFinish" u fsGen
    , puCoSimProp "multiplier_coSimulation" u fsGen
    ]
    where
        u = multiplier True :: Multiplier String Int Int
        fsGen = algGen
            [ fmap packF (arbitrary :: Gen (Multiply _ _))
            ]
