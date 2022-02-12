{-# LANGUAGE Arrows #-}

{- |
Module      : NITTA.XMILEFrontend
Description : XMILE frontend prototype
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.FrontEnds.XMILE.XMILEDocumentParser (
    parseXMILEDocument,
    XMILEContent (..),
    XMILEStock (..),
    XMILEAux (..),
    XMILEFlow (..),
) where

import qualified Data.Text as T
import NITTA.FrontEnds.XMILE.MathParser
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Core

data XMILEContent = XMILEContent
    { xcSimSpecs :: XMILESimSpec
    , xcFlows :: [XMILEFlow]
    , xcAuxs :: [XMILEAux]
    , xcStocks :: [XMILEStock]
    }
    deriving (Show)

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

parseXMILEDocument src =
    head $
        runLA
            ( xreadDoc
                >>> removeAllWhiteSpace
                >>> proc st -> do
                    simSpec <- parseSimSpec -< st
                    flows <- parseFlows -< st
                    auxs <- parseAuxs -< st
                    stocks <- parseStocks -< st
                    returnA -< XMILEContent{xcSimSpecs = simSpec, xcFlows = flows, xcAuxs = auxs, xcStocks = stocks}
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
                                , xsOutflow = if outflow == "" then Nothing else Just $ replaceSpaces $ T.pack outflow
                                , xsInflow = if inflow == "" then Nothing else Just $ replaceSpaces $ T.pack inflow
                                }
            )
    where
        getTagOrNothing name =
            (atTag name >>> text)
                `orElse` arr (const "")

replaceSpaces str = T.filter (/= '"') $ T.map repl str
    where
        repl ' ' = '_'
        repl c = c

atTag tag = deep (isElem >>> hasName tag)
atAttr attrName = deep (isElem >>> getAttrValue attrName)
text = getChildren >>> getText
