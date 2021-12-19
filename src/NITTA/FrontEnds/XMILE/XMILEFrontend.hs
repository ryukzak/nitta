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
import NITTA.FrontEnds.XMILE.MathParser
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.Core

data XMILEAlgBuilder x = XMILEAlgBuilder
    { algSimSpecs :: XMILESimSpec
    , algFlows :: [XMILEFlow]
    , algAuxs :: [XMILEAux]
    , algStocks :: [XMILEStock]
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
                auxs <- parseAuxs -< state
                stocks <- parseStocks -< state
                returnA -< XMILEAlgBuilder{algSimSpecs = simSpec, algFlows = [flows], algAuxs = [auxs], algStocks = [stocks]}

parseSimSpec =
    atTag "sim_specs"
        >>> proc x -> do
            stop <- text <<< atTag "stop" -< x
            start <- text <<< atTag "start" -< x
            dt <- text <<< atTag "dt" -< x
            returnA -< XMILESimSpec{xssStart = read start, xssStop = read stop, xssDt = read dt}

parseFlows =
    atTag "variables"
        >>> atTag "flow"
        >>> proc flow -> do
            eqn <- text <<< atTag "eqn" -< flow
            name <- atAttr "name" -< flow
            returnA -< XMILEFlow{xfEquation = parseXmileEquation eqn, xfName = T.pack name}

parseAuxs =
    atTag "variables"
        >>> atTag "aux"
        >>> proc aux -> do
            eqn <- text <<< atTag "eqn" -< aux
            name <- atAttr "name" -< aux
            returnA -< XMILEAux{xaEquation = parseXmileEquation eqn, xaName = T.pack name}

parseStocks =
    atTag "variables"
        >>> atTag "stock"
        >>> proc stock -> do
            eqn <- text <<< atTag "eqn" -< stock
            outflow <- getTagOrNothing "outflow" -< stock
            inflow <- getTagOrNothing "inflow" -< stock
            name <- atAttr "name" -< stock
            returnA
                -<
                    XMILEStock
                        { xsEquation = parseXmileEquation eqn
                        , xsName = T.pack name
                        , xsOutflow = if outflow == "" then Nothing else Just $ T.pack outflow
                        , xsInflow = if inflow == "" then Nothing else Just $ T.pack inflow
                        }
    where
        getTagOrNothing name =
            (atTag name >>> text)
                `orElse` arr (const "")

atTag tag = deep (isElem >>> hasName tag)
atAttr attrName = deep (isElem >>> getAttrValue attrName)
text = getChildren >>> getText

data XMILESimSpec = XMILESimSpec
    { xssStart :: Double
    , xssStop :: Double
    , xssDt :: Double
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
    , xaEquation :: XMExpr
    }
    deriving (Show)

data XMILEStock = XMILEStock
    { xsName :: T.Text
    , xsEquation :: XMExpr
    , xsInflow :: Maybe T.Text
    , xsOutflow :: Maybe T.Text
    }
    deriving (Show)

data XMILEAlgState v x = XMILEAlgState
    { xasSimSpec :: XMILESimSpec
    , xasModel :: XMILEModel
    }
    deriving (Show)
