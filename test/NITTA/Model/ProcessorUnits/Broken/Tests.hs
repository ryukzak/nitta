{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
import           NITTA.LuaFrontend.Tests.Utils
import           NITTA.Model.ProcessorUnits
import           NITTA.Model.ProcessorUnits.Tests.Utils
import           NITTA.Model.Tests.Microarchitecture
import           Test.QuickCheck
import           Test.Tasty ( testGroup )
import           Test.Tasty.ExpectedFailure
import           Text.InterpolatedString.Perl6 ( qc )

tests = testGroup "Broken PU"
    [ nittaCoSimTestCase "correct test" (maBroken def) alg
    , finitePUSynthesisProp "isFinish" u fsGen
    , puCoSimProp "correct coSimulation" u fsGen
    , typedLuaTestCase (maBroken def) pInt "correct lua test" lua

    , expectFail $ nittaCoSimTestCase "generated verilog with syntax error"
        (maBroken def{ brokeVerilog=True }) alg
    , expectFail $ nittaCoSimTestCase "generated verilog with error"
        (maBroken def{ wrongVerilogSimulationValue=True }) alg
    , expectFail $ nittaCoSimTestCase "generated software with wrong control sequence for data push"
        (maBroken def{ wrongControlOnPush=True }) alg
    , expectFail $ nittaCoSimTestCase "generated software with wrong control sequence for data pull"
        (maBroken def{ wrongControlOnPull=True }) alg

    , expectFail $ puCoSimProp "CoSim verilog with syntax error" u{ brokeVerilog=True } fsGen
    , expectFail $ puCoSimProp "CoSim generated verilog with error" u{ wrongVerilogSimulationValue=True } fsGen
    , expectFail $ puCoSimProp "CoSim generated software with wrong control sequence for data push" u{ wrongControlOnPush=True } fsGen
    , expectFail $ puCoSimProp "CoSim generated software with wrong control sequence for data pull" u{ wrongControlOnPull=True } fsGen

    , expectFail $ typedLuaTestCase (maBroken def{ brokeVerilog=True }) pInt "lua verilog with syntax error" lua
    , expectFail $ typedLuaTestCase (maBroken def{ wrongVerilogSimulationValue=True }) pInt "lua generated verilog with error" lua
    , expectFail $ typedLuaTestCase (maBroken def{ wrongControlOnPush=True }) pInt "lua generated software with wrong control sequence for data push" lua
    , expectFail $ typedLuaTestCase (maBroken def{ wrongControlOnPull=True }) pInt "lua generated software with wrong control sequence for data pull" lua
    ]
    where
        u = def :: Broken String Int Int
        alg = [ loop 1 "b" ["a"], brokenReg "a" ["b"] ]
        lua = [qc|
            function foo(a)
                b = brokenReg(a)
                foo(b)
            end
            foo(1)
        |]
        fsGen = algGen
            [ fmap packF (arbitrary :: Gen (BrokenReg _ _))
            ]
