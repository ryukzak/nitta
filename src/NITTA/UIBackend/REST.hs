{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.UIBackend.REST
Description : REST API description for NITTA backend
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.UIBackend.REST (
    SynthesisAPI,
    synthesisServer,
    BackendCtx (..),
) where

import Control.Monad.Except
import Data.Aeson
import Data.Bifunctor
import Data.Default
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Generics
import NITTA.Intermediate.Simulation
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Model.Types
import NITTA.Project (Project (..), writeAndRunTestbench)
import NITTA.Synthesis.Method
import NITTA.Synthesis.Tree
import NITTA.UIBackend.Marshalling
import NITTA.UIBackend.Timeline
import NITTA.UIBackend.VisJS (VisJS, algToVizJS)
import NITTA.Utils
import Servant
import Servant.Docs
import System.FilePath (joinPath)

data BackendCtx tag v x t = BackendCtx
    { -- |root synthesis node
      root :: G Node tag v x t
    , -- |lists of received by IO values
      receivedValues :: [(v, [x])]
    }

type SynthesisAPI tag v x t =
    ( Description "Get whole synthesis tree"
        :> "synthesisTree"
        :> Get '[JSON] (TreeView SynthesisNodeView)
    )
        :<|> ( "node" :> Capture "nId" NId
                :> ( SynthesisTreeNavigationAPI tag v x t
                        :<|> NodeInspectionAPI tag v x t
                        :<|> TestBenchAPI v x
                        :<|> SynthesisMethodsAPI tag v x t
                        :<|> SynthesisPracticesAPI tag v x t
                   )
             )

synthesisServer ctx@BackendCtx{root} =
    liftIO (viewNodeTree root)
        :<|> \nid ->
            synthesisTreeNavigation ctx nid
                :<|> nodeInspection ctx nid
                :<|> testBench ctx nid
                :<|> synthesisMethods ctx nid
                :<|> synthesisPractices ctx nid

type SynthesisTreeNavigationAPI tag v x t =
    Summary "Synthesis tree navigation"
        :> ( ( Description "Get list of synthesis nodes from the root to the specific node"
                :> "rootPath"
                :> Get '[JSON] [NodeView tag v x t]
             )
                :<|> ( Description "Get edge to the parent"
                        :> "parentEdge"
                        :> Get '[JSON] (Maybe (EdgeView tag v x t))
                     )
                :<|> ( Description "Get edges to all childs with static indexes"
                        :> "childEdges"
                        :> Get '[JSON] [EdgeView tag v x t]
                     )
           )

synthesisTreeNavigation BackendCtx{root} nid =
    liftIO (map view <$> getNodePathIO root nid)
        :<|> liftIO (fmap view . nOrigin <$> getNodeIO root nid)
        :<|> liftIO (map view <$> (getEdgesIO =<< getNodeIO root nid))

type NodeInspectionAPI tag v x t =
    Summary "Synthesis node inspection"
        :> ( ( Description "Get node info\n(see: NITTA.Synthesis.Tree.Node)"
                :> Get '[JSON] (NodeView tag v x t)
             )
                :<|> ( Description "Intermidiate reperesentation of the algorithm"
                        :> "intermediateView"
                        :> Get '[JSON] VisJS
                     )
                -- TODO: Replace by raw process fetching or add typescript types.
                :<|> ( Description "Computational process representation"
                        :> "processTimelines"
                        :> Get '[JSON] (ProcessTimelines t)
                     )
                :<|> ( Description "Enpoint options for all process units"
                        :> "endpoints"
                        :> Get '[JSON] [UnitEndpointView tag v]
                     )
                :<|> ("debug" :> DebugAPI tag v t)
           )

nodeInspection ctx@BackendCtx{root} nid =
    liftIO (view <$> getNodeIO root nid)
        :<|> liftIO (algToVizJS . functions . mDataFlowGraph . nModel <$> getNodeIO root nid)
        :<|> liftIO (processTimelines . process . mUnit . nModel <$> getNodeIO root nid)
        :<|> liftIO (dbgEndpointOptions <$> debug ctx nid)
        :<|> debug ctx nid

type SynthesisMethodsAPI tag v x t =
    Summary
        "Synthesis methods is a method for full synthesis tree exploration. \
        \Usually, it is more complicated than synthesis practice, but it is \
        \not an essential difference."
        :> ( ( Description "Composition of all available synthesis methods"
                :> "stateOfTheArtSynthesisIO"
                :> Post '[JSON] NId
             )
                :<|> "simpleSynthesis" :> Post '[JSON] NId
                :<|> "smartBindSynthesisIO" :> Post '[JSON] NId
           )

synthesisMethods BackendCtx{root} nid =
    liftIO (nId <$> (stateOfTheArtSynthesisIO =<< getNodeIO root nid))
        :<|> liftIO (nId <$> (simpleSynthesisIO =<< getNodeIO root nid))
        :<|> liftIO (nId <$> (smartBindSynthesisIO =<< getNodeIO root nid))

type SynthesisPracticesAPI tag v x t =
    Summary "SynthesisPractice is a set of small elements of the synthesis process."
        :> ( ( Description "Make the best synthesis step by the objective function"
                :> "bestStep"
                :> Post '[JSON] NId
             )
                :<|> ( Description "Make all possible oblivious binds"
                        :> "obviousBindThread"
                        :> Post '[JSON] NId
                     )
                :<|> ( Description "Make all possible binds and refactorings"
                        :> "allBindsAndRefsIO"
                        :> Post '[JSON] NId
                     )
                :<|> ( Description
                        "Explore all best synthesis threads from current \
                        \and `deep` nested levels."
                        :> "allBestThreads"
                        :> QueryParam' '[Required] "deep" Int
                        :> Post '[JSON] NId
                     )
           )

synthesisPractices BackendCtx{root} nid =
    liftIO (nId <$> (bestStepIO =<< getNodeIO root nid))
        :<|> liftIO (nId <$> (obviousBindThreadIO =<< getNodeIO root nid))
        :<|> liftIO (nId <$> (allBindsAndRefsIO =<< getNodeIO root nid))
        :<|> (\deep -> liftIO (nId <$> (allBestThreadIO deep =<< getNodeIO root nid)))

type TestBenchAPI v x =
    Summary "Get report of testbench execution for the current node."
        :> "testbench"
        :> QueryParam' '[Required] "pName" String
        :> QueryParam' '[Required] "loopsNumber" Int
        :> Post '[JSON] (TestbenchReportView v x)

testBench BackendCtx{root, receivedValues} nid pName loopsNumber = liftIO $ do
    node <- getNodeIO root nid
    let TargetSystem{mDataFlowGraph} = nModel node
    unless (nIsComplete node) $ error "test bench not allow for non complete synthesis"
    view
        <$> writeAndRunTestbench
            Project
                { pName
                , pLibPath = joinPath ["..", "..", "hdl"]
                , pPath = joinPath ["gen", pName]
                , pUnit = mUnit $ nModel node
                , pTestCntx = simulateDataFlowGraph loopsNumber def receivedValues mDataFlowGraph
                }

------------------------------------------------------------

-- |Type for CAD debugging. Used for extracting internal information.
data Debug tag v t = Debug
    { dbgEndpointOptions :: [UnitEndpointView tag v]
    , dbgFunctionLocks :: [(String, [Lock v])]
    , dbgCurrentStateFunctionLocks :: [(String, [Lock v])]
    , dbgPULocks :: [(String, [Lock v])]
    }
    deriving (Generic)

instance (ToJSON tag, ToJSON t, Time t) => ToJSON (Debug tag String t)

instance ToSample (Debug String String Int)
instance ToSample Char where toSamples _ = noSamples

instance {-# OVERLAPS #-} ToSample [(String, [Lock String])] where
    toSamples _ = singleSample [("PU or function tag", [Lock{locked = "b", lockBy = "a"}])]

type DebugAPI tag v t =
    Description
        "Debuging interface to fast access to internal state \
        \(see NITTA.UIBackend.REST.Debug)"
        :> Get '[JSON] (Debug tag v t)

debug BackendCtx{root} nid = liftIO $ do
    node <- getNodeIO root nid
    let dbgFunctionLocks = map (\f -> (show f, locks f)) $ functions $ mDataFlowGraph $ nModel node
        already = transferred $ mUnit $ nModel node
    return
        Debug
            { dbgEndpointOptions = endpointOptions' $ mUnit $ nModel node
            , dbgFunctionLocks
            , dbgCurrentStateFunctionLocks =
                [ (tag, filter (\Lock{lockBy, locked} -> S.notMember lockBy already && S.notMember locked already) ls)
                | (tag, ls) <- dbgFunctionLocks
                ]
            , dbgPULocks = map (second locks) $ M.assocs $ bnPus $ mUnit $ nModel node
            }
    where
        endpointOptions' BusNetwork{bnPus} =
            let f (tag, pu) =
                    map (\(t, ep) -> UnitEndpointView t $ view ep) $
                        zip (repeat tag) $ endpointOptions pu
             in concatMap f $ M.assocs bnPus
