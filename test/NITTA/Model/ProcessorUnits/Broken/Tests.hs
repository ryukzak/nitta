{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.String.Interpolate
import NITTA.LuaFrontend.Tests.Providers
import NITTA.Model.ProcessorUnits.Tests.Providers
import NITTA.Model.Tests.Providers
import Test.QuickCheck
import Test.Tasty (testGroup)
import Test.Tasty.ExpectedFailure

tests =
    testGroup
        "Broken PU and negative tests"
        [ testGroup
            "positive tests"
            [ puCoSimTestCase "broken buffer" u [("a", 42)] [brokenBuffer "a" ["b"]]
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
                [brokenBuffer "a" ["b"]]
            , puCoSimTestCase
                "unknown output with invalid flag"
                u2{unknownDataOut = True}
                [("a", Attr{value = 42, invalid = True})]
                [brokenBuffer "a" ["b"]]
            , nittaCoSimTestCase "with attr" (maBroken u2) alg
            ]
        , testGroup
            "positive tests with attributes"
            [ expectFail $
                puCoSimTestCase
                    "invalid value with broken attr"
                    u2{wrongAttr = True}
                    [("a", Attr{value = 42, invalid = True})]
                    [brokenBuffer "a" ["b"]]
            , expectFail $
                puCoSimTestCase
                    "unknown output without invalid flag"
                    u2{unknownDataOut = True}
                    [("a", Attr{value = 42, invalid = False})]
                    [brokenBuffer "a" ["b"]]
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
        , testGroup
            "broken relations integrity check"
            [ nittaCoSimTestCase "nittaCoSimTestCase positive test" (maBroken u) alg
            , typedLuaTestCase (maBroken def) pInt "typedLuaTestCase positive test" lua
            , puCoSimTestCase "puCoSimTestCase positive test" u [("a", 42)] [brokenBuffer "a" ["b"]]
            ]
        , testGroup
            "broken relations integrity check"
            [ --- TODO fix case when we have lost Intstruction but integrityCheck is OK
              expectFail $ finitePUSynthesisProp "finitePUSynthesisProp lost instr and ep" u{lostInstructionRelation = True, lostEndpointRelation = True} fsGen
            , expectFail $ finitePUSynthesisProp "finitePUSynthesisProp lost Endpoints" u{lostEndpointRelation = True} fsGen
            , expectFail $ finitePUSynthesisProp "finitePUSynthesisProp lost Instruction" u{lostInstructionRelation = True} fsGen
            , expectFail $ puCoSimProp "puCoSimProp lost Endpoints" u{lostEndpointRelation = True} fsGen
            , expectFail $ puCoSimProp "puCoSimProp lost Instruction" u{lostInstructionRelation = True} fsGen
            ]
        , testGroup
            "broken relations integrity check"
            [ expectFail $ puCoSimTestCase "puCoSimTestCase lost Instructions" u{lostInstructionRelation = True} [("a", 42)] [brokenBuffer "a" ["b"]]
            , expectFail $ nittaCoSimTestCase "nittaCoSimTestCase lost Endpoints" (maBroken u{lostEndpointRelation = True}) alg
            , expectFail $ nittaCoSimTestCase "nittaCoSimTestCase lost Instruction" (maBroken u{lostInstructionRelation = True}) alg
            , expectFail $ typedLuaTestCase (maBroken def{lostEndpointRelation = True}) pInt "typedLuaTestCase lost Endpoints" lua
            , expectFail $ typedLuaTestCase (maBroken def{lostInstructionRelation = True}) pInt "typedLuaTestCase lost Instruction" lua
            ]
        , testGroup
            "broken relations integrity check fails"
            [ finitePUSynthesisProp "finitePUSynthesisProp relation positive test" u fsGen
            , puCoSimProp "puCoSimProp relation positive test" u fsGen
            , expectFail $ puCoSimProp "puCoSimProp lost Function" u{lostFunctionRelation = True} fsGen
            , expectFail $ finitePUSynthesisProp "finitePUSynthesisProp lost Function" u{lostFunctionRelation = True} fsGen
            ]
            {-
            , testGroup
                "broken puCoSimTestCase"
                [ expectFail $ puCoSimTestCase "puCoSimTestCase lost Function" u{lostFunctionRelation = True} [("a", 42)] [brokenBuffer "a" ["b"]]
                , expectFail $ puCoSimTestCase "puCoSimTestCase lost Endpoints" u{lostEndpointRelation = True} [("a", 42)] [brokenBuffer "a" ["b"]]
                , expectFail $ nittaCoSimTestCase "nittaCoSimTestCase lost Function" (maBroken u{lostFunctionRelation = True}) alg
                , expectFail $ typedLuaTestCase (maBroken def{lostFunctionRelation = True}) pInt "typedLuaTestCase lost Function" lua
                ]
                -}
        ]
    where
        u = def :: Broken String Int Int
        u2 = def :: Broken String (Attr (IntX 32)) Int
        alg = [loop 1 "b" ["a"], brokenBuffer "a" ["b"]]
        lua =
            [__i|
                function foo(a)
                    b = brokenBuffer(a)
                    foo(b)
                end
                foo(1)
            |]
        fsGen =
            algGen
                [ fmap packF (arbitrary :: Gen (BrokenBuffer _ _))
                ]
