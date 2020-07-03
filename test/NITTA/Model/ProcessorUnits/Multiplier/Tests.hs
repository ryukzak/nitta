{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}

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
import           NITTA.Intermediate.Tests.Functions     ()
import           NITTA.Intermediate.Types
import           NITTA.Model.ProcessorUnits
import           NITTA.Model.ProcessorUnits.Tests.Utils
import           NITTA.Test.Microarchitectures
import           Test.QuickCheck
import           Test.Tasty                             (testGroup)


tests = testGroup "Multiplier PU"
    [ nittaCoSimTestCase "smoke test" march
        [ constant 2 ["a"]
        , loop 1 "c" ["b"]
        , multiply "a" "b" ["c"]

        , constant 3 ["x"]
        , loop 1 "z" ["y"]
        , multiply "y" "x" ["z"]
        ]
    , finitePUSynthesisProp "isFinish" u fsGen
    , puCoSimProp "multiplier_coSimulation" u fsGen
    ]
    where
        u = multiplier True :: Multiplier String Int Int
        fsGen = algGen
            [ fmap packF (arbitrary :: Gen (Multiply _ _))
            ]
