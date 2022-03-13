{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : NITTA.FrontEnds.XMILEFrontend.MathParserTests
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.FrontEnds.XMILEFrontend.MathParserTests (
    tests,
) where

import qualified Data.HashMap.Strict as HM
import NITTA.FrontEnds.XMILE.MathParser
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Tasty.TH

case_xmileMathParserAdditionTest =
    let expr = parseXmileEquation "A + B"
     in expr @?= Duo Add (Var "A") (Var "B")

case_xmileMathParserSubtractionTest =
    let expr = parseXmileEquation "A - B"
     in expr @?= Duo Sub (Var "A") (Var "B")

case_xmileMathParserMultiplicationTest =
    let expr = parseXmileEquation "A * B"
     in expr @?= Duo Mul (Var "A") (Var "B")

case_xmileMathParserDivisionTest =
    let expr = parseXmileEquation "A / B"
     in expr @?= Duo Div (Var "A") (Var "B")

case_xmileMathParserMultiplicationPriorityTest =
    let expr = parseXmileEquation "A + B * C"
     in expr @?= Duo Add (Var "A") (Duo Mul (Var "B") (Var "C"))

case_xmileMathParserDivisionPriorityTest =
    let expr = parseXmileEquation "A + B / C"
     in expr @?= Duo Add (Var "A") (Duo Div (Var "B") (Var "C"))

case_xmileMathParserBracketsTest =
    let expr = parseXmileEquation "(A + B) / C"
     in expr @?= Duo Div (Duo Add (Var "A") (Var "B")) (Var "C")

case_xmileMathParserCalculateAddition =
    let val = calculateDefaultValue HM.empty (Duo Add (Val 5) (Val 6))
     in val @?= 11

case_xmileMathParserCalculateSubtraction =
    let val = calculateDefaultValue HM.empty (Duo Sub (Val 5) (Val 6))
     in val @?= -1

case_xmileMathParserCalculateMultiplication =
    let val = calculateDefaultValue HM.empty (Duo Mul (Val 5) (Val 6))
     in val @?= 30

case_xmileMathParserCalculateDivision =
    let val = calculateDefaultValue HM.empty (Duo Div (Val 6) (Val 2))
     in val @?= 3

tests :: TestTree
tests = $(testGroupGenerator)
