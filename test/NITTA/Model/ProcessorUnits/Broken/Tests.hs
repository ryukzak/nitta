{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS -fno-warn-partial-type-signatures #-}

{-|
Module      : NITTA.Model.ProcessorUnits.Broken.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Broken.Tests
    ( tests
    ) where

import           Data.Default
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Tests.Functions ()
import           NITTA.Intermediate.Types
import           NITTA.Model.ProcessorUnits
import           NITTA.Model.ProcessorUnits.Tests.Utils
import           NITTA.Model.Tests.Microarchitecture
import           Test.QuickCheck
import           Test.Tasty ( testGroup )
import           Test.Tasty.ExpectedFailure

tests = testGroup "Broken PU"
    [ nittaCoSimTestCase "correct test" (maBroken def)
        [ loop 1 "b" ["a"]
        , brokenReg "a" ["b"]
        ]
    , finitePUSynthesisProp "isFinish" u fsGen
    , puCoSimProp "correct coSimulation" u fsGen

    , expectFail
        $ nittaCoSimTestCase "generated verilog with syntax error" (maBroken def{ brokeVerilog=True })
            [ loop 1 "b" ["a"]
            , brokenReg "a" ["b"]
            ]
    , expectFail
        $ nittaCoSimTestCase "generated verilog with error" (maBroken def{ wrongVerilogSimulationValue=True })
            [ loop 1 "b" ["a"]
            , brokenReg "a" ["b"]
            ]
    , expectFail $ puCoSimProp "CoSim verilog with syntax error" u{ brokeVerilog=True } fsGen
    , expectFail $ puCoSimProp "CoSim generated verilog with error" u{ wrongVerilogSimulationValue=True } fsGen
    ]
    where
        u = def :: Broken String Int Int
        fsGen = algGen
            [ fmap packF (arbitrary :: Gen (BrokenReg _ _))
            ]
