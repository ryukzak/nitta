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
import NITTA.Utils.Tests (testCaseM)
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

tests =
    testGroup
        "NITTA.Frontends.XMILE.MathParserTests"
        [ testCaseM "xmileMathParserAdditionTest" $
            parseXmileEquation "A + B"
                @?= Duo Add (Var "A") (Var "B")
        , testCaseM "xmileMathParserSubtractionTest" $
            parseXmileEquation "A - B"
                @?= Duo Sub (Var "A") (Var "B")
        , testCaseM "xmileMathParserMultiplicationTest" $
            parseXmileEquation "A * B"
                @?= Duo Mul (Var "A") (Var "B")
        , testCaseM "xmileMathParserDivisionTest" $
            parseXmileEquation "A / B"
                @?= Duo Div (Var "A") (Var "B")
        , testCaseM "xmileMathParserMultiplicationPriorityTest" $
            parseXmileEquation "A + B * C"
                @?= Duo Add (Var "A") (Duo Mul (Var "B") (Var "C"))
        , testCaseM "xmileMathParserDivisionPriorityTest" $
            parseXmileEquation "A + B / C"
                @?= Duo Add (Var "A") (Duo Div (Var "B") (Var "C"))
        , testCaseM "xmileMathParserBracketsTest" $
            parseXmileEquation "A + B / C"
                @?= Duo Add (Var "A") (Duo Div (Var "B") (Var "C"))
        , testCaseM "xmileMathParserCalculateAddition" $
            calculateDefaultValue HM.empty (Duo Add (Val 5) (Val 6)) @?= 11
        , testCaseM "xmileMathParserCalculateSubtraction" $
            calculateDefaultValue HM.empty (Duo Sub (Val 5) (Val 6)) @?= -1
        , testCaseM "xmileMathParserCalculateMultiplication" $
            calculateDefaultValue HM.empty (Duo Mul (Val 5) (Val 6)) @?= 30
        , testCaseM "xmileMathParserCalculateDivision" $
            calculateDefaultValue HM.empty (Duo Div (Val 6) (Val 2)) @?= 3
        , expectFail $
            testCaseM "xmileMathParserDivisionToZero" $
                calculateDefaultValue HM.empty (Duo Div (Val 6) (Val 0)) @?= 1 / 0
        ]
