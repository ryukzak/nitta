-- All extensions should be enabled explicitly due to doctest in this module.
{-# LANGUAGE Arrows #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
    parseDocument,
    Content (..),
    Stock (..),
    Aux (..),
    Flow (..),
    SimSpec (..),
) where

import Data.Text qualified as T
import NITTA.Frontends.XMILE.MathParser
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Core

data Content = Content
    { xcSimSpecs :: SimSpec
    , xcFlows :: [Flow]
    , xcAuxs :: [Aux]
    , xcStocks :: [Stock]
    }
    deriving (Show, Eq)

data SimSpec = SimSpec
    { xssStart :: Double
    , xssStop :: Double
    , xssDt :: Double
    }
    deriving (Show, Eq)

newtype Model = Model
    {xmVariables :: Variables}
    deriving (Show, Eq)

data Variables = Variables
    { xvFlows :: [Flow]
    , xvAuxs :: [Aux]
    , xvStocks :: [Stock]
    }
    deriving (Show, Eq)

data Flow = Flow
    { xfName :: T.Text
    , xfEquation :: XMExpr
    }
    deriving (Show, Eq)

data Aux = Aux
    { xaName :: T.Text
    , xaEquation :: XMExpr
    }
    deriving (Show, Eq)

data Stock = Stock
    { xsName :: T.Text
    , xsEquation :: XMExpr
    , xsInflow :: Maybe T.Text
    , xsOutflow :: Maybe T.Text
    }
    deriving (Show, Eq)

data AlgState v x = AlgState
    { xasSimSpec :: SimSpec
    , xasModel :: Model
    }
    deriving (Show, Eq)

parseDocument src =
    head $
        runLA
            ( xreadDoc
                >>> removeAllWhiteSpace
                >>> proc st -> do
                    xcSimSpecs <- parseSimSpec -< st
                    xcFlows <- parseFlows -< st
                    xcAuxs <- parseAuxs -< st
                    xcStocks <- parseStocks -< st
                    returnA -< Content{xcSimSpecs, xcFlows, xcAuxs, xcStocks}
            )
            src

parseSimSpec =
    atTag "sim_specs"
        >>> proc x -> do
            stop <- text <<< atTag "stop" -< x
            start <- text <<< atTag "start" -< x
            dt <- text <<< atTag "dt" -< x
            returnA -< SimSpec{xssStart = read start, xssStop = read stop, xssDt = read dt}

parseFlows =
    atTag "variables"
        >>> listA
            ( atTag "flow"
                >>> proc flow -> do
                    eqn <- text <<< atTag "eqn" -< flow
                    name <- atAttr "name" -< flow
                    returnA -< Flow{xfEquation = parseXmileEquation eqn, xfName = replaceSpaces $ T.pack name}
            )

parseAuxs =
    atTag "variables"
        >>> listA
            ( atTag "aux"
                >>> proc aux -> do
                    eqn <- text <<< atTag "eqn" -< aux
                    name <- atAttr "name" -< aux
                    returnA -< Aux{xaEquation = parseXmileEquation eqn, xaName = replaceSpaces $ T.pack name}
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
                            Stock
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
