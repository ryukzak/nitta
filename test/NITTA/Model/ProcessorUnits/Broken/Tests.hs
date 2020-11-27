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

tests = testGroup "Broken PU and negative tests"
    [ testGroup "positive tests"
        [ finitePUSynthesisProp "finitePUSynthesisProp" u fsGen
        , puCoSimProp "puCoSimProp" u fsGen
        , nittaCoSimTestCase "nittaCoSimTestCase" (maBroken def) alg
        , typedLuaTestCase (maBroken def) pInt "typedLuaTestCase" lua
        ]

    , testGroup "verilog with syntax error"
        [ expectFail $ puCoSimProp "puCoSimProp verilog syntax error" u{ brokeVerilog=True } fsGen
        , expectFail $ nittaCoSimTestCase "nittaCoSimTestCase verilog syntax error" (maBroken def{ brokeVerilog=True }) alg
        , expectFail $ typedLuaTestCase (maBroken def{ brokeVerilog=True }) pInt "typedLuaTestCase verilog syntax error" lua
        ]

    , testGroup "verilog with error"
        [ expectFail $ puCoSimProp "puCoSimProp verilog error" u{ wrongVerilogSimulationValue=True } fsGen
        , expectFail $ nittaCoSimTestCase "nittaCoSimTestCase verilog error" (maBroken def{ wrongVerilogSimulationValue=True }) alg
        , expectFail $ typedLuaTestCase (maBroken def{ wrongVerilogSimulationValue=True }) pInt "typedLuaTestCase verilog error" lua
        ]

    , testGroup "wrong control sequence for data push"
        [ expectFail $ puCoSimProp "puCoSimProp wrong control push" u{ wrongControlOnPush=True } fsGen
        , expectFail $ nittaCoSimTestCase "nittaCoSimTestCase wrong control push" (maBroken def{ wrongControlOnPush=True }) alg
        , expectFail $ typedLuaTestCase (maBroken def{ wrongControlOnPush=True }) pInt "typedLuaTestCase wrong control push" lua
        ]

    , testGroup "wrong control sequence for data pull"
        [ expectFail $ puCoSimProp "puCoSimProp wrong control pull" u{ wrongControlOnPull=True } fsGen
        , expectFail $ nittaCoSimTestCase "nittaCoSimTestCase wrong control pull" (maBroken def{ wrongControlOnPull=True }) alg
        , expectFail $ typedLuaTestCase (maBroken def{ wrongControlOnPull=True }) pInt "wrongControlOnPull wrong control pull" lua
        ]

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
