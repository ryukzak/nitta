{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : NITTA.FrontEnds.XMILEFrontend.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.FrontEnds.XMILEFrontend.Tests (
    tests,
) where

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

tests :: TestTree
tests = $(testGroupGenerator)
