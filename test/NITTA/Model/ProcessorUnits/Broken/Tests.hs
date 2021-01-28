{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-partial-type-signatures #-}

{- |
 Module      : NITTA.Model.ProcessorUnits.Broken.Tests
 Description :
 Copyright   : (c) Aleksandr Penskoi, 2020
 License     : BSD3
 Maintainer  : aleksandr.penskoi@gmail.com
 Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Broken.Tests (
    tests,
) where

import Data.Default
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Tests.Functions ()
import NITTA.Intermediate.Types
import NITTA.LuaFrontend.Tests.Utils
import NITTA.Model.ProcessorUnits
import NITTA.Model.ProcessorUnits.Tests.Utils
import NITTA.Model.Tests.Microarchitecture
import Test.QuickCheck
import Test.Tasty (testGroup)
import Test.Tasty.ExpectedFailure
import Text.InterpolatedString.Perl6 (qc)

tests =
    testGroup
        "Broken PU and negative tests"
        [ testGroup
            "positive tests"
            [ puCoSimTestCase "broken reg" u [("a", 42)] [brokenReg "a" ["b"]]
            , puCoSimProp "puCoSimProp" u fsGen
            , nittaCoSimTestCase "nittaCoSimTestCase" (maBroken u) alg
            , finitePUSynthesisProp "finitePUSynthesisProp" u fsGen
            , typedLuaTestCase (maBroken def) pInt "typedLuaTestCase" lua
            ]
        , testGroup
            "positive tests with attributes"
            [ puCoSimTestCase
                "invalid value"
                u2
                [("a", Attr{value = 42, invalid = True})]
                [brokenReg "a" ["b"]]
            , puCoSimTestCase
                "unknown output with invalid flag"
                u2{unknownDataOut = True}
                [("a", Attr{value = 42, invalid = True})]
                [brokenReg "a" ["b"]]
            , nittaCoSimTestCase "with attr" (maBroken u2) alg
            ]
        , testGroup
            "positive tests with attributes"
            [ expectFail $
                puCoSimTestCase
                    "invalid value with broken attr"
                    u2{wrongAttr = True}
                    [("a", Attr{value = 42, invalid = True})]
                    [brokenReg "a" ["b"]]
            , expectFail $
                puCoSimTestCase
                    "unknown output without invalid flag"
                    u2{unknownDataOut = True}
                    [("a", Attr{value = 42, invalid = False})]
                    [brokenReg "a" ["b"]]
            , expectFail $ nittaCoSimTestCase "with broken attr" (maBroken u2{wrongAttr = True}) alg
            ]
        , testGroup
            "verilog with syntax error"
            [ expectFail $ puCoSimProp "puCoSimProp verilog syntax error" u{brokeVerilog = True} fsGen
            , expectFail $ nittaCoSimTestCase "nittaCoSimTestCase verilog syntax error" (maBroken u{brokeVerilog = True}) alg
            , expectFail $ typedLuaTestCase (maBroken def{brokeVerilog = True}) pInt "typedLuaTestCase verilog syntax error" lua
            ]
        , testGroup
            "verilog with error"
            [ expectFail $ puCoSimProp "puCoSimProp verilog error" u{wrongVerilogSimulationValue = True} fsGen
            , expectFail $ nittaCoSimTestCase "nittaCoSimTestCase verilog error" (maBroken u{wrongVerilogSimulationValue = True}) alg
            , expectFail $ typedLuaTestCase (maBroken def{wrongVerilogSimulationValue = True}) pInt "typedLuaTestCase verilog error" lua
            ]
        , testGroup
            "wrong control sequence for data push"
            [ expectFail $ puCoSimProp "puCoSimProp wrong control push" u{wrongControlOnPush = True} fsGen
            , expectFail $ nittaCoSimTestCase "nittaCoSimTestCase wrong control push" (maBroken u{wrongControlOnPush = True}) alg
            , expectFail $ typedLuaTestCase (maBroken def{wrongControlOnPush = True}) pInt "typedLuaTestCase wrong control push" lua
            ]
        , testGroup
            "wrong control sequence for data pull"
            [ expectFail $ puCoSimProp "puCoSimProp wrong control pull" u{wrongControlOnPull = True} fsGen
            , expectFail $ nittaCoSimTestCase "nittaCoSimTestCase wrong control pull" (maBroken u{wrongControlOnPull = True}) alg
            , expectFail $ typedLuaTestCase (maBroken def{wrongControlOnPull = True}) pInt "wrongControlOnPull wrong control pull" lua
            ]
        , testGroup
            "lost target endpoint due synthesis"
            [ expectFail $ finitePUSynthesisProp "finitePUSynthesisProp lost target endpoint" u{lostEndpointTarget = True} fsGen
            , expectFail $ puCoSimProp "puCoSimProp lost target endpoint" u{lostEndpointTarget = True} fsGen
            , expectFail $ nittaCoSimTestCase "nittaCoSimTestCase lost target endpoint" (maBroken u{lostEndpointTarget = True}) alg
            , expectFail $ typedLuaTestCase (maBroken def{lostEndpointTarget = True}) pInt "typedLuaTestCase lost target endpoint" lua
            ]
        , testGroup
            "lost source endpoint due synthesis"
            [ expectFail $ finitePUSynthesisProp "finitePUSynthesisProp lost source endpoint" u{lostEndpointSource = True} fsGen
            , expectFail $ puCoSimProp "puCoSimProp lost source endpoint" u{lostEndpointSource = True} fsGen
            , expectFail $ nittaCoSimTestCase "nittaCoSimTestCase lost source endpoint" (maBroken u{lostEndpointSource = True}) alg
            , expectFail $ typedLuaTestCase (maBroken def{lostEndpointSource = True}) pInt "typedLuaTestCase lost source endpoint" lua
            ]
        ]
    where
        u = def :: Broken String Int Int
        u2 = def :: Broken String (Attr (IntX 32)) Int
        alg = [loop 1 "b" ["a"], brokenReg "a" ["b"]]
        lua =
            [qc|
            function foo(a)
                b = brokenReg(a)
                foo(b)
            end
            foo(1)
        |]
        fsGen =
            algGen
                [ fmap packF (arbitrary :: Gen (BrokenReg _ _))
                ]
