{-# LANGUAGE Arrows #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : NITTA.Frontends.XMILE.DocumentParser
Description : Parses XMILE source file to internal tree structure
Copyright   : (c) Artur Gogiyan, 2022
License     : BSD3
Maintainer  : artur.gogiyan@gmail.com
Stability   : experimental
-}
module NITTA.Frontends.XMILE.DocumentParser (
    parseXMILEDocument,
    XMILEContent (..),
    XMILEStock (..),
    XMILEAux (..),
    XMILEFlow (..),
    XMILESimSpec (..),
) where

import qualified Data.Text as T
import NITTA.Frontends.XMILE.MathParser
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Core

data XMILEContent = XMILEContent
    { xcSimSpecs :: XMILESimSpec
    , xcFlows :: [XMILEFlow]
    , xcAuxs :: [XMILEAux]
    , xcStocks :: [XMILEStock]
    }
    deriving (Show, Eq)

data XMILESimSpec = XMILESimSpec
    { xssStart :: Double
    , xssStop :: Double
    , xssDt :: Double
    }
    deriving (Show, Eq)

newtype XMILEModel = XMILEModel
    {xmVariables :: XMILEVariables}
    deriving (Show, Eq)

data XMILEVariables = XMILEVariables
    { xvFlows :: [XMILEFlow]
    , xvAuxs :: [XMILEAux]
    , xvStocks :: [XMILEStock]
    }
    deriving (Show, Eq)

data XMILEFlow = XMILEFlow
    { xfName :: T.Text
    , xfEquation :: XMExpr
    }
    deriving (Show, Eq)

data XMILEAux = XMILEAux
    { xaName :: T.Text
    , xaEquation :: XMExpr
    }
    deriving (Show, Eq)

data XMILEStock = XMILEStock
    { xsName :: T.Text
    , xsEquation :: XMExpr
    , xsInflow :: Maybe T.Text
    , xsOutflow :: Maybe T.Text
    }
    deriving (Show, Eq)

data XMILEAlgState v x = XMILEAlgState
    { xasSimSpec :: XMILESimSpec
    , xasModel :: XMILEModel
    }
    deriving (Show, Eq)

parseXMILEDocument src =
    head $
        runLA
            ( xreadDoc
                >>> removeAllWhiteSpace
                >>> proc st -> do
                    xcSimSpecs <- parseSimSpec -< st
                    xcFlows <- parseFlows -< st
                    xcAuxs <- parseAuxs -< st
                    xcStocks <- parseStocks -< st
                    returnA -< XMILEContent{xcSimSpecs, xcFlows, xcAuxs, xcStocks}
            )
            src

parseSimSpec =
    atTag "sim_specs"
        >>> proc x -> do
            stop <- text <<< atTag "stop" -< x
            start <- text <<< atTag "start" -< x
            dt <- text <<< atTag "dt" -< x
            returnA -< XMILESimSpec{xssStart = read start, xssStop = read stop, xssDt = read dt}

parseFlows =
    atTag "variables"
        >>> listA
            ( atTag "flow"
                >>> proc flow -> do
                    eqn <- text <<< atTag "eqn" -< flow
                    name <- atAttr "name" -< flow
                    returnA -< XMILEFlow{xfEquation = parseXmileEquation eqn, xfName = replaceSpaces $ T.pack name}
            )

parseAuxs =
    atTag "variables"
        >>> listA
            ( atTag "aux"
                >>> proc aux -> do
                    eqn <- text <<< atTag "eqn" -< aux
                    name <- atAttr "name" -< aux
                    returnA -< XMILEAux{xaEquation = parseXmileEquation eqn, xaName = replaceSpaces $ T.pack name}
            )

parseStocks =
    atTag "variables"
        >>> listA
            ( atTag "stock"
                >>> proc stock -> do
                    eqn <- text <<< atTag "eqn" -< stock
                    outflow <- getTagOrNothing "outflow" -< stock
                    inflow <- getTagOrNothing "inflow" -< stock
                    name <- atAttr "name" -< stock
                    returnA
                        -<
                            XMILEStock
                                { xsEquation = parseXmileEquation eqn
                                , xsName = replaceSpaces $ T.pack name
                                , xsOutflow = replaceSpaces <$> outflow
                                , xsInflow = replaceSpaces <$> inflow
                                }
            )
    where
        getTagOrNothing name =
            (atTag name >>> text >>> arr (\x -> Just (T.pack x)))
                `orElse` arr (const Nothing)

replaceSpaces str = T.filter (/= '"') $ T.replace " " "_" str

atTag tag = deep (isElem >>> hasName tag)
atAttr attrName = deep (isElem >>> getAttrValue attrName)
text = getChildren >>> getText
