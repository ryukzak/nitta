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
Module      : NITTA.FrontEnds.XMILE.XMILEFrontend
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
import NITTA.FrontEnds.XMILE.XMILEDocumentParser
import NITTA.Intermediate.DataFlow
import qualified NITTA.Intermediate.Functions as F
import NITTA.Utils (showText)

data XMILEAlgBuilder v x = XMILEAlgBuilder
    { algDataFlowGraph :: DataFlowGraph v x
    , algTraceVars :: [TraceVar]
    , algUsagesCount :: HM.HashMap T.Text Int
    , algDefaultValues :: HM.HashMap T.Text Double
    , algNextFreeNameIndex :: HM.HashMap T.Text Int
    , algNextArgIndex :: Int
    }
    deriving (Show)

_xmileSample :: T.Text
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

xmile2functions src =
    let xmContent = parseXMILEDocument $ T.unpack src
        builder = processXMILEGraph xmContent
        frTrace = algTraceVars' builder
     in FrontendResult{frDataFlow = algDataFlowGraph builder, frTrace = frTrace, frPrettyLog = prettyLog frTrace}
    where
        algTraceVars' :: XMILEAlgBuilder T.Text Int -> [TraceVar]
        algTraceVars' = algTraceVars

processXMILEGraph xmContent = flip execState emptyBuilder $ do
    getDefaultValuesAndUsages xmContent
    createDataFlowGraph xmContent
    where
        emptyBuilder =
            XMILEAlgBuilder
                { algDataFlowGraph = DFCluster []
                , algTraceVars = []
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
                    input <- getUniqueName xsName
                    addStockLoop input outputs
                (Nothing, Just inflow) -> do
                    stockUniqueName <- getUniqueName xsName
                    tmpName <- processStockFlow inflow F.add "In" stockUniqueName
                    addStockLoop tmpName outputs
                (Just outflow, Nothing) -> do
                    stockUniqueName <- getUniqueName xsName
                    tmpName <- processStockFlow outflow F.sub "Out" stockUniqueName
                    addStockLoop tmpName outputs
                (Just outflow, Just inflow) -> do
                    stockUniqueName <- getUniqueName xsName
                    tmpNameOut <- processStockFlow outflow F.sub "Out" stockUniqueName
                    tmpNameIn <- processStockFlow inflow F.add "In" tmpNameOut
                    addStockLoop tmpNameIn outputs
            return ()
            where
                processStockFlow flowName func ending stockName = do
                    flowUniqueName <- getUniqueName flowName
                    let tmpName = xsName <> "_" <> ending
                    x@XMILEAlgBuilder{algDataFlowGraph} <- get
                    put x{algDataFlowGraph = addFuncToDataFlowGraph (func stockName flowUniqueName [tmpName]) algDataFlowGraph}
                    return tmpName
                addStockLoop stockName outputs = do
                    let traceVarName = TraceVar{tvFmt = defaultFmt, tvVar = T.takeWhile (/= '#') $ head outputs}
                    defaultValue <- getDefaultValue xsName
                    x@XMILEAlgBuilder{algDataFlowGraph, algTraceVars} <- get
                    put
                        x
                            { algDataFlowGraph = addFuncToDataFlowGraph (F.loop (read $ show defaultValue) stockName outputs) algDataFlowGraph
                            , algTraceVars = traceVarName : algTraceVars
                            }
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
            (flowName, _) <- processFlowEquation xfEquation (0 :: Int)
            outputs <- getAllOutGraphNodes xfName
            x@XMILEAlgBuilder{algDataFlowGraph} <- get
            put x{algDataFlowGraph = addFuncToDataFlowGraph (F.loop 0 flowName outputs) algDataFlowGraph}

            return ()
            where
                processFlowEquation (Var name) index = do
                    n <- getUniqueName $ T.pack name
                    return (n, index)
                processFlowEquation (Duo op leftExpr rightExpr) tempNameIndex = do
                    (leftName, tempNameIndex') <- processFlowEquation leftExpr tempNameIndex
                    (rightName, tempNameIndex'') <- processFlowEquation rightExpr tempNameIndex'
                    let tmpName = T.pack "_" <> showText tempNameIndex'' <> T.pack "#" <> xfName <> showText tempNameIndex''
                    x@XMILEAlgBuilder{algDataFlowGraph = a} <- get
                    case op of
                        Add -> do
                            put x{algDataFlowGraph = addFuncToDataFlowGraph (F.add leftName rightName [tmpName]) a}
                            return (tmpName, tempNameIndex'' + 1)
                        Sub -> do
                            put x{algDataFlowGraph = addFuncToDataFlowGraph (F.sub leftName rightName [tmpName]) a}
                            return (tmpName, tempNameIndex'' + 1)
                        Mul -> do
                            put x{algDataFlowGraph = addFuncToDataFlowGraph (F.multiply leftName rightName [tmpName]) a}
                            return (tmpName, tempNameIndex'' + 1)
                        Div -> do
                            let divName = tmpName <> "div"
                            put x{algDataFlowGraph = addFuncToDataFlowGraph (F.division leftName rightName [tmpName] [divName]) a}
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

        getDefaultValue name = do
            XMILEAlgBuilder{algDefaultValues} <- get
            return $ fromMaybe (error $ "name not found :" <> T.unpack name) $ HM.lookup name algDefaultValues

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
                    addUsages xsName
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
                            put x{algUsagesCount = HM.insert name 0 algUsagesCount}
                    return ()
