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

import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import Data.String.Interpolate
import qualified Data.Text as T
import NITTA.FrontEnds.Common
import NITTA.FrontEnds.XMILE.MathParser
import NITTA.Intermediate.DataFlow
import qualified NITTA.Intermediate.Functions as F
import NITTA.Utils (showText)
import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Core

data XMILEContent = XMILEContent
    { xcSimSpecs :: XMILESimSpec
    , xcFlows :: [XMILEFlow]
    , xcAuxs :: [XMILEAux]
    , xcStocks :: [XMILEStock]
    }
    deriving (Show)

data XMILEAlgBuilder v x = XMILEAlgBuilder
    { algDataFlowGraph :: DataFlowGraph v x
    , algUsagesCount :: HM.HashMap T.Text Int
    , algDefaultValues :: HM.HashMap T.Text Double
    , algNextFreeNameIndex :: HM.HashMap T.Text Int
    , algNextArgIndex :: Int
    }
    deriving (Show)

_xmileSample :: String
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
    xmContent <- parseXMILEDocument src
    let graph = algDataFlowGraph $ processXMILEGraph xmContent
    return graph
    where
        --return FrontendResult{frDataFlow = graph, frTrace = [], frPrettyLog = const [HM.empty]}

        processXMILEGraph xmContent = flip execState emptyBuilder $ do
            getDefaultValuesAndUsages xmContent
            createDataFlowGraph xmContent
            where
                emptyBuilder =
                    XMILEAlgBuilder
                        { algDataFlowGraph = DFCluster []
                        , algNextFreeNameIndex = HM.empty
                        , algDefaultValues = HM.empty
                        , algUsagesCount = HM.empty
                        , algNextArgIndex = 0
                        }

createDataFlowGraph xmContent = do
    x@XMILEAlgBuilder{algUsagesCount} <- get
    put x{algNextFreeNameIndex = foldl (\hm key -> HM.insert key 0 hm) HM.empty $ HM.keys algUsagesCount}
    mapM_ processStock $ xcStocks xmContent
    mapM_ processAux $ xcAuxs xmContent
    mapM_ processFlow $ xcFlows xmContent
    where
        processStock XMILEStock{xsName, xsOutflow, xsInflow} = do
            outputs <- getAllOutGraphNodes xsName
            case (xsOutflow, xsInflow) of
                (Nothing, Nothing) -> do
                    x@XMILEAlgBuilder{algDataFlowGraph} <- get
                    input <- getUniqueName xsName
                    argIndex <- getNextArgIndex
                    put x{algDataFlowGraph = addFuncToDataFlowGraph (F.loop argIndex input outputs) algDataFlowGraph}
                    return ()
                (Nothing, Just _) ->
                    return ()
                (Just outflow, Nothing) -> do
                    x@XMILEAlgBuilder{algDataFlowGraph} <- get
                    stockUniqueName <- getUniqueName xsName
                    flowUniqueName <- getUniqueName outflow
                    argIndex <- getNextArgIndex
                    let tmpName = xsName <> "_tmp"
                    let algDataFlowGraph' = addFuncToDataFlowGraph (F.sub stockUniqueName flowUniqueName [tmpName]) algDataFlowGraph
                    put x{algDataFlowGraph = addFuncToDataFlowGraph (F.loop argIndex tmpName outputs) algDataFlowGraph'}
                    return ()
                (Just _, Just _) ->
                    return ()

            return ()

        processAux XMILEAux{xaName, xaEquation} = do
            case xaEquation of
                (Val value) -> do
                    x@XMILEAlgBuilder{algDataFlowGraph} <- get
                    outputs <- getAllOutGraphNodes xaName
                    put x{algDataFlowGraph = addFuncToDataFlowGraph (F.constant (round value) outputs) algDataFlowGraph}
                    return ()
                _ -> return ()

        processFlow XMILEFlow{xfName, xfEquation} = do
            x@XMILEAlgBuilder{algDataFlowGraph} <- get
            (flowName, _) <- processFlowEquation xfEquation (1 :: Int)
            outputs <- getAllOutGraphNodes xfName
            argIndex <- getNextArgIndex
            put x{algDataFlowGraph = addFuncToDataFlowGraph (F.loop argIndex flowName outputs) algDataFlowGraph}

            return ()
            where
                processFlowEquation (Var name) index = do
                    n <- getUniqueName $ T.pack name
                    return (n, index)
                processFlowEquation (Duo op leftExpr rightExpr) tempNameIndex = do
                    (leftName, tempNameIndex') <- processFlowEquation leftExpr tempNameIndex
                    (rightName, tempNameIndex'') <- processFlowEquation rightExpr tempNameIndex'
                    let tmpName = xfName <> T.pack "_tmp" <> showText tempNameIndex''
                    x@XMILEAlgBuilder{algDataFlowGraph} <- get
                    case op of
                        Add -> do
                            put x{algDataFlowGraph = addFuncToDataFlowGraph (F.add leftName rightName [tmpName]) algDataFlowGraph}
                        Sub -> do
                            put x{algDataFlowGraph = addFuncToDataFlowGraph (F.sub leftName rightName [tmpName]) algDataFlowGraph}
                        Mul -> do
                            put x{algDataFlowGraph = addFuncToDataFlowGraph (F.multiply leftName rightName [tmpName]) algDataFlowGraph}
                        Div -> do
                            let divName = tmpName <> "div"
                            put x{algDataFlowGraph = addFuncToDataFlowGraph (F.division leftName rightName [tmpName] [divName]) algDataFlowGraph}
                    return (tmpName, tempNameIndex'' + 1)
                processFlowEquation _ _ = undefined

        getUniqueName name = do
            x@XMILEAlgBuilder{algNextFreeNameIndex} <- get
            let index = fromMaybe (error $ "name not found : " <> T.unpack name) $ HM.lookup name algNextFreeNameIndex
            put x{algNextFreeNameIndex = HM.insert name (index + 1) algNextFreeNameIndex}
            return $ getGraphName name index

        getAllOutGraphNodes name = do
            XMILEAlgBuilder{algUsagesCount} <- get
            let usages = fromMaybe (error $ "name not found : " <> T.unpack name) $ HM.lookup name algUsagesCount
            return $ map (\num -> getGraphName name num) [0 .. usages]

        getNextArgIndex = do
            x@XMILEAlgBuilder{algNextArgIndex} <- get
            put x{algNextArgIndex = algNextArgIndex + 1}
            return algNextArgIndex

        getGraphName name index = name <> T.pack "#" <> showText index

getDefaultValuesAndUsages algBuilder = do
    mapM_ processStock $ xcStocks algBuilder
    where
        nameToEquationMap = flip execState HM.empty $ do
            mapM_ (addToMap . (\a -> (xaName a, xaEquation a))) (xcAuxs algBuilder)
            mapM_ (addToMap . (\a -> (xfName a, xfEquation a))) (xcFlows algBuilder)
            mapM (addToMap . (\a -> (xsName a, xsEquation a))) (xcStocks algBuilder)
            where
                addToMap (name, eqn) = do
                    hm <- get
                    put $ HM.insert name eqn hm
        processStock XMILEStock{xsEquation, xsName, xsInflow, xsOutflow} = do
            let val = computeDefaultValue xsEquation
            alg@XMILEAlgBuilder{algDefaultValues} <- get
            put alg{algDefaultValues = HM.insert xsName val algDefaultValues}
            processFlow xsInflow
            processFlow xsOutflow

            return ()
            where
                computeDefaultValue (Val v) = v
                computeDefaultValue _ = undefined

                processFlow Nothing = do return ()
                processFlow (Just name) = do
                    addUsages name
                    processEquation $ xfEquation $ findFlow name

                findFlow name =
                    fromMaybe
                        (error $ "cannot find expected flow flow with name " <> T.unpack name <> show (xcFlows algBuilder))
                        (L.find (\XMILEFlow{xfName} -> xfName == name) $ xcFlows algBuilder)

                processEquation (Val _) = do return ()
                processEquation (Duo _ expr expl) = do
                    processEquation expr
                    processEquation expl
                processEquation (Var name) = do
                    addUsages $ T.pack name
                    addDefaultValueIfNeeded
                    where
                        addDefaultValueIfNeeded = do
                            x@XMILEAlgBuilder{algDefaultValues} <- get
                            case HM.lookup (T.pack name) algDefaultValues of
                                Just _ -> do return ()
                                Nothing -> do
                                    case HM.lookup (T.pack name) nameToEquationMap of
                                        Just val -> do
                                            put x{algDefaultValues = HM.insert (T.pack name) (computeDefaultValue val) algDefaultValues}
                                        Nothing -> error $ "equation for name " <> name <> " not found."
                            return ()

                addUsages name = do
                    x@XMILEAlgBuilder{algUsagesCount} <- get
                    case HM.lookup name algUsagesCount of
                        Just val -> do
                            put x{algUsagesCount = HM.insert name (val + 1) algUsagesCount}
                        Nothing -> do
                            put x{algUsagesCount = HM.insert name 1 algUsagesCount}
                    return ()

parseXMILEDocument src =
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
