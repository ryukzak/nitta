{-# LANGUAGE Arrows #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
Module      : NITTA.XMILEFrontend
Description : XMILE frontend prototype
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.FrontEnds.XMILE.XMILEFrontend (
    xmile2functions,
    FrontendResult (..),
    TraceVar (..),
) where

import Data.String.Interpolate
import qualified Data.Text as T
import NITTA.FrontEnds.Common
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.Core
import NITTA.FrontEnds.XMILE.MathParser
import Data.Either

data XMILEAlgBuilder x = XMILEAlgBuilder
    { algSimSpecs :: XMILESimSpec
    , algFlows :: [XMILEFlow]
    }
    deriving (Show)

_xmileSample =
    [__i| 
                <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
                <xmile version="1.0" xmlns="http://docs.oasis-open.org/xmile/ns/XMILE/v1.0">
                    <header>
                        <vendor>James Houghton</vendor>
                        <name>Teacup</name>
                        <options>
                            <uses_outputs/>
                        </options>
                        <product version="1.0">Hand Coded XMILE</product>
                    </header>
                    <sim_specs>
                        <stop>30.0</stop>
                        <start>0.0</start>
                        <dt>0.125</dt>
                    </sim_specs>
                    <model>
                        <variables>
                            <flow name="Heat Loss to Room">
                                <doc>Heat Loss to Room</doc>
                                <eqn>("Teacup Temperature"-"Room Temperature")/"Characteristic Time"</eqn>
                            </flow>
                            <aux name="Room Temperature">
                                <doc>Ambient Room Temperature</doc>
                                <eqn>70</eqn>
                            </aux>
                            <stock name="Teacup Temperature">
                                <doc>The average temperature of the tea and the cup</doc>
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

xmile2functions src = do
    [_] <- parseXMILEDocument src
    return FrontendResult{frDataFlow = undefined, frTrace = undefined, frPrettyLog = undefined frTrace}

parseXMILEDocument src =

    runX $
        readString [withValidate no] src
            >>> removeAllWhiteSpace
            >>> proc state -> do
                simSpec <- parseSimSpec -< state
                flows <- parseFlows -< state
                returnA -< XMILEAlgBuilder { algSimSpecs = simSpec, algFlows = [flows] }

parseSimSpec =
    atTag "sim_specs"
        >>> proc x -> do
            stop <- text <<< atTag "stop" -< x
            start <- text <<< atTag "start" -< x
            dt <- text <<< atTag "dt" -< x
            returnA -< XMILESimSpec{xssStart = T.pack start, xssStop = T.pack stop, xssDt = T.pack dt}

parseFlows =
    atTag "variables"
        >>> atTag "flow"
        >>> proc flow -> do
            eqn <- text <<< atTag "eqn" -< flow
            name <- atAttr "name" -< flow
            returnA -< XMILEFlow{xfEquation = fromRight undefined $ parseXmileEquation eqn, xfName = T.pack name}

atTag tag = deep (isElem >>> hasName tag)
atAttr attrName = deep (isElem >>> getAttrValue attrName)
text = getChildren >>> getText

data XMILESimSpec = XMILESimSpec
    { xssStart :: T.Text
    , xssStop :: T.Text
    , xssDt :: T.Text
    }
    deriving (Show)

newtype XMILEModel = XMILEModel
    {xmVariables :: XMILEVariables}
    deriving (Show)

data XMILEVariables = XMILEVariables
    { xvFlows :: [XMILEFlow]
    , xvAuxs :: [XMILEAux]
    , xvStocks :: [XMILEStock]
    }
    deriving (Show)

data XMILEFlow = XMILEFlow
    { xfName :: T.Text
    , xfEquation :: XMExpr
    }
    deriving (Show)

data XMILEAux = XMILEAux
    { xaName :: T.Text
    , xaEquation :: T.Text
    }
    deriving (Show)

data XMILEStock = XMILEStock
    { xsName :: T.Text
    , xsEquation :: T.Text
    }
    deriving (Show)

data XMILEAlgState v x = XMILEAlgState
    { xasSimSpec :: XMILESimSpec
    , xasModel :: XMILEModel
    }
    deriving (Show)
