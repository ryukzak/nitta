{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
Module      : NITTA.Frontends.XMILE.Frontend
Description : XMILE frontend prototype
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Frontends.XMILE.Frontend (
    translateXMILE,
    FrontendResult (..),
    TraceVar (..),
) where

import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import NITTA.Frontends.Common
import NITTA.Frontends.XMILE.DocumentParser
import NITTA.Frontends.XMILE.MathParser
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

deltaTimeVarName = T.pack "time_delta"

translateXMILE src =
    let xmContent = parseXMILEDocument $ T.unpack src
        builder = processXMILEGraph xmContent
        frTrace = algTraceVars' builder
     in FrontendResult{frDataFlow = algDataFlowGraph builder, frTrace, frPrettyLog = prettyLog frTrace}
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
    addTimeIncrement
    where
        addTimeIncrement = do
            x@XMILEAlgBuilder{algUsagesCount} <- get
            put x{algUsagesCount = HM.insert deltaTimeVarName ((algUsagesCount HM.! deltaTimeVarName) + 1) algUsagesCount}
            dtUniqueName <- getUniqueName deltaTimeVarName
            dtAllGraphNodes <- getAllOutGraphNodes deltaTimeVarName
            x'@XMILEAlgBuilder{algDataFlowGraph, algTraceVars} <- get
            let dt = xssDt $ xcSimSpecs xmContent
                startTime = xssStart $ xcSimSpecs xmContent
                graph = addFuncToDataFlowGraph (F.add "time" dtUniqueName [T.pack "time_inc"]) algDataFlowGraph
                graph' = addFuncToDataFlowGraph (F.loop (read $ show startTime) "time_inc" [T.pack "time"]) graph
                graph'' = addFuncToDataFlowGraph (F.constant (read $ show dt) dtAllGraphNodes) graph'
            put x'{algDataFlowGraph = graph'', algTraceVars = TraceVar{tvFmt = defaultFmt, tvVar = "time"} : algTraceVars}
            return ()

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
                    let tmpName = xsName <> "_" <> ending
                    let dt = xssDt $ xcSimSpecs xmContent
                    flowUniqueName <- skaleToDeltaTime dt
                    x@XMILEAlgBuilder{algDataFlowGraph} <- get
                    put x{algDataFlowGraph = addFuncToDataFlowGraph (func stockName flowUniqueName [tmpName]) algDataFlowGraph}
                    return tmpName
                    where
                        skaleToDeltaTime 1 = do getUniqueName flowName
                        skaleToDeltaTime _ = do
                            flowUniqueName <- getUniqueName flowName
                            dtUniqueNmae <- getUniqueName deltaTimeVarName
                            x@XMILEAlgBuilder{algDataFlowGraph} <- get
                            let skaledFlowName = stockName <> "_" <> flowName <> "_" <> ending
                            put x{algDataFlowGraph = addFuncToDataFlowGraph (F.multiply dtUniqueNmae flowUniqueName [skaledFlowName]) algDataFlowGraph}
                            return skaledFlowName

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
                    put x{algDataFlowGraph = addFuncToDataFlowGraph (F.constant (read $ show value) outputs) algDataFlowGraph}
                    return ()
                _ -> return ()

        processFlow XMILEFlow{xfName, xfEquation} = do
            _ <- processFlowEquation xfEquation (0 :: Int) True
            return ()
            where
                processFlowEquation (Var name) index _ = do
                    n <- getUniqueName $ T.pack name
                    return (n, index)
                processFlowEquation (Duo op leftExpr rightExpr) tempNameIndex isTopLevel = do
                    (leftName, tempNameIndex') <- processFlowEquation leftExpr tempNameIndex False
                    (rightName, tempNameIndex'') <- processFlowEquation rightExpr tempNameIndex' False
                    tmpName <- getTempName tempNameIndex'' xfName isTopLevel
                    x@XMILEAlgBuilder{algDataFlowGraph = a} <- get
                    case op of
                        Add -> do
                            put x{algDataFlowGraph = addFuncToDataFlowGraph (F.add leftName rightName tmpName) a}
                            return (head tmpName, tempNameIndex'' + 1)
                        Sub -> do
                            put x{algDataFlowGraph = addFuncToDataFlowGraph (F.sub leftName rightName tmpName) a}
                            return (head tmpName, tempNameIndex'' + 1)
                        Mul -> do
                            put x{algDataFlowGraph = addFuncToDataFlowGraph (F.multiply leftName rightName tmpName) a}
                            return (head tmpName, tempNameIndex'' + 1)
                        Div -> do
                            put x{algDataFlowGraph = addFuncToDataFlowGraph (F.division leftName rightName tmpName []) a}
                            return (head tmpName, tempNameIndex'' + 1)
                    where
                        getTempName _ name True = do getAllOutGraphNodes name
                        getTempName index name _ = do return [T.pack "_" <> showText index <> T.pack "#" <> name]
                processFlowEquation _ _ _ = undefined

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
            alg@XMILEAlgBuilder{algDefaultValues} <- get
            let val = calculateDefaultValue algDefaultValues xsEquation
            put alg{algDefaultValues = HM.insert xsName val algDefaultValues}
            processFlow xsInflow
            processFlow xsOutflow

            return ()
            where
                processFlow Nothing = do return ()
                processFlow (Just name) = do
                    addUsages name
                    addUsages xsName
                    addDtUsagesIfNeeded $ xssDt $ xcSimSpecs algBuilder
                    processEquation $ xfEquation $ findFlow name
                    where
                        addDtUsagesIfNeeded 1 = do return ()
                        addDtUsagesIfNeeded _ = do
                            addUsages deltaTimeVarName

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
                                            put x{algDefaultValues = HM.insert (T.pack name) (calculateDefaultValue algDefaultValues val) algDefaultValues}
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
