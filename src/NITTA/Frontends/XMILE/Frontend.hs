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
Copyright   : (c) Artur Gogiyan, 2022
License     : BSD3
Maintainer  : artur.gogiyan@gmail.com
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
import Data.String
import qualified Data.Text as T
import NITTA.Frontends.Common
import NITTA.Frontends.XMILE.DocumentParser
import NITTA.Frontends.XMILE.MathParser
import NITTA.Intermediate.DataFlow
import qualified NITTA.Intermediate.Functions as F
import NITTA.Utils.Base

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
    modify
        ( \st@XMILEAlgBuilder{algUsagesCount} ->
            st{algNextFreeNameIndex = foldl (\hm key -> HM.insert key 0 hm) HM.empty $ HM.keys algUsagesCount}
        )
    mapM_ processStock $ xcStocks xmContent
    mapM_ processAux $ xcAuxs xmContent
    mapM_ processFlow $ xcFlows xmContent
    addTimeIncrement
    where
        addTimeIncrement = do
            modify
                ( \st@XMILEAlgBuilder{algUsagesCount} ->
                    st
                        { algUsagesCount =
                            HM.insert
                                deltaTimeVarName
                                ((algUsagesCount HM.! deltaTimeVarName) + 1)
                                algUsagesCount
                        }
                )
            dtUniqueName <- getUniqueName deltaTimeVarName
            dtAllGraphNodes <- getAllOutGraphNodes deltaTimeVarName
            XMILEAlgBuilder{algDataFlowGraph} <- get
            let dt = xssDt $ xcSimSpecs xmContent
                startTime = xssStart $ xcSimSpecs xmContent
                graph =
                    L.foldl'
                        (flip addFuncToDataFlowGraph)
                        algDataFlowGraph
                        [ F.constant (read $ show dt) (map fromText dtAllGraphNodes)
                        , F.loop (read $ show startTime) (fromString "time_inc") [fromString "time"]
                        , F.add (fromString "time") (fromText dtUniqueName) [fromString "time_inc"]
                        ]
            modify
                ( \st@XMILEAlgBuilder{algTraceVars} ->
                    st
                        { algDataFlowGraph = graph
                        , algTraceVars = TraceVar{tvFmt = Nothing, tvVar = "time"} : algTraceVars
                        }
                )

        processStock XMILEStock{xsName, xsOutflow, xsInflow} = do
            outputs <- getAllOutGraphNodes xsName
            case (xsOutflow, xsInflow) of
                (Nothing, Nothing) -> do
                    input <- getUniqueName xsName
                    addStockLoop input outputs
                (Nothing, Just inflow) -> do
                    stockUniqueName <- getUniqueName xsName
                    tmpName <- processStockFlow inflow F.add (fromString "In") stockUniqueName
                    addStockLoop tmpName outputs
                (Just outflow, Nothing) -> do
                    stockUniqueName <- getUniqueName xsName
                    tmpName <- processStockFlow outflow F.sub (fromString "Out") stockUniqueName
                    addStockLoop tmpName outputs
                (Just outflow, Just inflow) -> do
                    stockUniqueName <- getUniqueName xsName
                    tmpNameOut <- processStockFlow outflow F.sub (fromString "Out") stockUniqueName
                    tmpNameIn <- processStockFlow inflow F.add (fromString "In") tmpNameOut
                    addStockLoop tmpNameIn outputs
            where
                processStockFlow flowName func ending stockName = do
                    let tmpName = xsName <> "_" <> ending
                        dt = xssDt $ xcSimSpecs xmContent
                    flowUniqueName <- skaleToDeltaTime dt
                    modify
                        ( \st@XMILEAlgBuilder{algDataFlowGraph} ->
                            st
                                { algDataFlowGraph =
                                    addFuncToDataFlowGraph
                                        (func (fromText stockName) (fromText flowUniqueName) [fromText tmpName])
                                        algDataFlowGraph
                                }
                        )
                    return tmpName
                    where
                        skaleToDeltaTime 1 = do getUniqueName flowName
                        skaleToDeltaTime _ = do
                            flowUniqueName <- getUniqueName flowName
                            dtUniqueName <- getUniqueName deltaTimeVarName
                            let skaledFlowName = stockName <> "_" <> flowName <> "_" <> ending
                            modify
                                ( \st@XMILEAlgBuilder{algDataFlowGraph} ->
                                    st
                                        { algDataFlowGraph =
                                            addFuncToDataFlowGraph
                                                (F.multiply (fromText dtUniqueName) (fromText flowUniqueName) [fromText skaledFlowName])
                                                algDataFlowGraph
                                        }
                                )
                            return skaledFlowName

                addStockLoop stockName outputs = do
                    let traceVarName = TraceVar{tvFmt = Nothing, tvVar = T.takeWhile (/= '#') $ head outputs}
                    defaultValue <- getDefaultValue xsName
                    modify
                        ( \st@XMILEAlgBuilder{algDataFlowGraph, algTraceVars} ->
                            st
                                { algDataFlowGraph =
                                    addFuncToDataFlowGraph
                                        (F.loop (read $ show defaultValue) (fromText stockName) (map fromText outputs))
                                        algDataFlowGraph
                                , algTraceVars = traceVarName : algTraceVars
                                }
                        )

        processAux XMILEAux{xaName, xaEquation} = do
            case xaEquation of
                (Val value) -> do
                    outputs <- getAllOutGraphNodes xaName
                    modify
                        ( \st@XMILEAlgBuilder{algDataFlowGraph} ->
                            st
                                { algDataFlowGraph =
                                    addFuncToDataFlowGraph
                                        (F.constant (read $ show value) (map fromText outputs))
                                        algDataFlowGraph
                                }
                        )
                expr -> error $ "non supported equation part: " <> show expr

        processFlow XMILEFlow{xfName, xfEquation} = do
            void (processFlowEquation xfEquation (0 :: Int) True)
            where
                processFlowEquation (Var name) index _ = do
                    n <- getUniqueName $ T.pack name
                    return (n, index)
                processFlowEquation (Duo op leftExpr rightExpr) tempNameIndex isTopLevel = do
                    (leftNameText, tempNameIndex') <- processFlowEquation leftExpr tempNameIndex False
                    (rightNameText, tempNameIndex'') <- processFlowEquation rightExpr tempNameIndex' False
                    tmpNameText <- getTempName tempNameIndex'' xfName isTopLevel
                    let leftName = fromText leftNameText
                        rightName = fromText rightNameText
                        tmpName = map fromText tmpNameText

                    st@XMILEAlgBuilder{algDataFlowGraph = graph} <- get
                    case op of
                        Add -> put st{algDataFlowGraph = addFuncToDataFlowGraph (F.add leftName rightName tmpName) graph}
                        Sub -> put st{algDataFlowGraph = addFuncToDataFlowGraph (F.sub leftName rightName tmpName) graph}
                        Mul -> put st{algDataFlowGraph = addFuncToDataFlowGraph (F.multiply leftName rightName tmpName) graph}
                        Div -> put st{algDataFlowGraph = addFuncToDataFlowGraph (F.division leftName rightName tmpName []) graph}
                    return (head tmpName, tempNameIndex'' + 1)
                    where
                        getTempName _ name True = do getAllOutGraphNodes name
                        getTempName index name _ = do return [T.pack "_" <> showText index <> T.pack "#" <> name]
                processFlowEquation _ _ _ = undefined

        getUniqueName name = do
            XMILEAlgBuilder{algNextFreeNameIndex} <- get
            let index = fromMaybe (error $ "name not found : " <> T.unpack name) $ HM.lookup name algNextFreeNameIndex
            modify (\st -> st{algNextFreeNameIndex = HM.insert name (index + 1) algNextFreeNameIndex})
            return $ getGraphName name index

        getAllOutGraphNodes name = do
            XMILEAlgBuilder{algUsagesCount} <- get
            let usages = fromMaybe (error $ "name not found : " <> T.unpack name) $ HM.lookup name algUsagesCount
            return $ map (\num -> getGraphName name num) [0 .. usages]

        getDefaultValue name = do
            XMILEAlgBuilder{algDefaultValues} <- get
            return $ fromMaybe (error $ "name not found :" <> T.unpack name) $ HM.lookup name algDefaultValues

        getGraphName name index = name <> fromString ("#" <> show index)

getDefaultValuesAndUsages algBuilder = do
    mapM_ processStock $ xcStocks algBuilder
    where
        nameToEquationMap = flip execState HM.empty $ do
            mapM_ (addToMap . (\a -> (xaName a, xaEquation a))) (xcAuxs algBuilder)
            mapM_ (addToMap . (\a -> (xfName a, xfEquation a))) (xcFlows algBuilder)
            mapM_ (addToMap . (\a -> (xsName a, xsEquation a))) (xcStocks algBuilder)
            where
                addToMap (name, eqn) = modify (\st -> HM.insert name eqn st)
        processStock XMILEStock{xsEquation, xsName, xsInflow, xsOutflow} = do
            XMILEAlgBuilder{algDefaultValues} <- get
            let val = calculateDefaultValue algDefaultValues xsEquation
            modify (\st -> st{algDefaultValues = HM.insert xsName val algDefaultValues})
            processFlow xsInflow
            processFlow xsOutflow
            where
                processFlow Nothing = do return ()
                processFlow (Just name) = do
                    addUsages name
                    addUsages xsName
                    addDtUsagesIfNeeded $ xssDt $ xcSimSpecs algBuilder
                    processEquation $ xfEquation $ findFlow name
                    where
                        addDtUsagesIfNeeded 1 = do return ()
                        addDtUsagesIfNeeded _ = addUsages deltaTimeVarName

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
                            st@XMILEAlgBuilder{algDefaultValues} <- get
                            case HM.lookup (T.pack name) algDefaultValues of
                                Just _ -> do return ()
                                Nothing -> do
                                    case HM.lookup (T.pack name) nameToEquationMap of
                                        Just val -> do
                                            put
                                                st
                                                    { algDefaultValues =
                                                        HM.insert
                                                            (T.pack name)
                                                            (calculateDefaultValue algDefaultValues val)
                                                            algDefaultValues
                                                    }
                                        Nothing -> error $ "equation for name " <> name <> " not found."

                addUsages name = do
                    XMILEAlgBuilder{algUsagesCount} <- get
                    let val = maybe 0 (+ 1) $ HM.lookup name algUsagesCount
                    modify (\st -> st{algUsagesCount = HM.insert name val algUsagesCount})
