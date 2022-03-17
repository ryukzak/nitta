{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : NITTA.Frontends.XMILE.DocumentParserTests
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Frontends.XMILE.DocumentParserTests (
    tests,
) where

import Data.String.Interpolate
import NITTA.Frontends.XMILE.DocumentParser
import NITTA.Frontends.XMILE.MathParser
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Tasty.TH

_xmileSample =
    [__i| 
                <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
                <xmile version="1.0" xmlns="http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
                    <sim_specs>
                        <stop>30.0</stop>
                        <start>0.0</start>
                        <dt>0.125</dt>
                    </sim_specs>
                    <model>
                        <variables>
                            <flow name="Heat Loss to Room">
                                <eqn>("Teacup Temperature"-"Room Temperature")/"Characteristic Time"</eqn>
                            </flow>
                            <aux name="Room Temperature">
                                <eqn>70</eqn>
                            </aux>
                            <stock name="Teacup Temperature">
                                <outflow>"Heat Loss to Room"</outflow>
                                <eqn>180</eqn>
                            </stock>
                            <aux name="Characteristic Time">
                                <eqn>10</eqn>
                            </aux>
                        </variables>
                    </model>
                </xmile>
            |]

case_teacupExampleParsingTest =
    let parsedContent = parseXMILEDocument _xmileSample
        expectedContent =
            XMILEContent
                { xcSimSpecs =
                    XMILESimSpec
                        { xssDt = 0.125
                        , xssStart = 0
                        , xssStop = 30
                        }
                , xcAuxs =
                    [ XMILEAux
                        { xaName = "Room_Temperature"
                        , xaEquation = Val 70
                        }
                    , XMILEAux
                        { xaName = "Characteristic_Time"
                        , xaEquation = Val 10
                        }
                    ]
                , xcStocks =
                    [ XMILEStock
                        { xsName = "Teacup_Temperature"
                        , xsEquation = Val 180
                        , xsInflow = Nothing
                        , xsOutflow = Just "Heat_Loss_to_Room"
                        }
                    ]
                , xcFlows =
                    [ XMILEFlow
                        { xfName = "Heat_Loss_to_Room"
                        , xfEquation = Duo Div (Duo Sub (Var "Teacup_Temperature") (Var "Room_Temperature")) (Var "Characteristic_Time")
                        }
                    ]
                }
     in parsedContent @?= expectedContent

tests :: TestTree
tests = $(testGroupGenerator)
