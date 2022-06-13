{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : NITTA.Frontends.XMILE.MathParserTests
Description :
Copyright   : (c) Artur Gogiyan, 2022
License     : BSD3
Maintainer  : artur.gogiyan@gmail.com
Stability   : experimental
-}
module NITTA.Frontends.XMILE.MathParserTests (
    tests,
) where

import Data.HashMap.Strict qualified as HM
import NITTA.Frontends.XMILE.MathParser
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

tests =
    testGroup
        "NITTA.Frontends.XMILE.MathParserTests"
        [ testCase "xmileMathParserDigitParsingTest" $
            parseXmileEquation "2E5"
                @?= Val 200000
        , testCase "xmileMathParserAdditionTest" $
            parseXmileEquation "A + B"
                @?= Duo Add (Var "A") (Var "B")
        , testCase "xmileMathParserSubtractionTest" $
            parseXmileEquation "A - B"
                @?= Duo Sub (Var "A") (Var "B")
        , testCase "xmileMathParserMultiplicationTest" $
            parseXmileEquation "A * B"
                @?= Duo Mul (Var "A") (Var "B")
        , testCase "xmileMathParserDivisionTest" $
            parseXmileEquation "A / B"
                @?= Duo Div (Var "A") (Var "B")
        , testCase "xmileMathParserMultiplicationPriorityTest" $
            parseXmileEquation "A + B * C"
                @?= Duo Add (Var "A") (Duo Mul (Var "B") (Var "C"))
        , testCase "xmileMathParserDivisionPriorityTest" $
            parseXmileEquation "A + B / C"
                @?= Duo Add (Var "A") (Duo Div (Var "B") (Var "C"))
        , testCase "xmileMathParserBracketsTest" $
            parseXmileEquation "A + B / C"
                @?= Duo Add (Var "A") (Duo Div (Var "B") (Var "C"))
        , testCase "xmileMathParserCalculateAddition" $
            calculateDefaultValue HM.empty (Duo Add (Val 5) (Val 6)) @?= 11
        , testCase "xmileMathParserCalculateSubtraction" $
            calculateDefaultValue HM.empty (Duo Sub (Val 5) (Val 6)) @?= -1
        , testCase "xmileMathParserCalculateMultiplication" $
            calculateDefaultValue HM.empty (Duo Mul (Val 5) (Val 6)) @?= 30
        , testCase "xmileMathParserCalculateDivision" $
            calculateDefaultValue HM.empty (Duo Div (Val 6) (Val 2)) @?= 3
        , expectFail $
            testCase "xmileMathParserDivisionToZero" $
                calculateDefaultValue HM.empty (Duo Div (Val 6) (Val 0)) @?= 1 / 0
        ]
