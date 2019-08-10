{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.UIBackend.REST
Description : REST API description for NITTA backend
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.UIBackend.REST
    ( SynthesisAPI
    , synthesisServer
    ) where

import           Control.Concurrent.STM
import           Control.Monad.Except
import           Data.Aeson
import           Data.Default
import qualified Data.Map                         as M
import           Data.Maybe
import qualified Data.Tree                        as T
import           GHC.Generics
import           NITTA.Intermediate.Simulation
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.Problems.Whole
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.TargetSystem
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Types
import           NITTA.Project.Utils              (writeAndRunTestbench)
import           NITTA.Synthesis.Method
import           NITTA.Synthesis.Types
import           NITTA.Synthesis.Utils
import           NITTA.UIBackend.Marshalling      ()
import           NITTA.UIBackend.Timeline
import           NITTA.UIBackend.VisJS            (VisJS, algToVizJS)
import           Servant
import           System.FilePath                  (joinPath)


-- *REST API

type SynthesisAPI tag v x t
    =    "synthesis" :> Get '[JSON] (T.Tree SynthesisNodeView)
    :<|> "synthesis" :> Capture "nId" NId :> WithSynthesis tag v x t

synthesisServer root
    =    liftIO ( synthesisNodeView root )
    :<|> \nId -> withSynthesis root nId



type WithSynthesis tag v x t
    =    Get '[JSON] (G Node tag v x t)
    :<|> "edge" :> Get '[JSON] (Maybe (G Edge tag v x t))
    :<|> "model" :> Get '[JSON] (ModelState (BusNetwork tag v x t) v x)
    :<|> "timelines" :> Get '[JSON] (ProcessTimelines t)
    :<|> "endpointOptions" :> Get '[JSON] [(tag, EndpointOption v t)]
    :<|> "model" :> "alg" :> Get '[JSON] VisJS
    :<|> "testBench" :> "output" :> QueryParam' '[Required] "name" String :> Get '[JSON] (TestbenchReport v x)
    :<|> SimpleCompilerAPI tag v x t

withSynthesis root nId
    =    liftIO ( getNodeIO root nId )
    :<|> liftIO ( nOrigin <$> getNodeIO root nId )
    :<|> liftIO ( nModel <$> getNodeIO root nId )
    :<|> liftIO ( processTimelines . process . mUnit . nModel <$> getNodeIO root nId )
    :<|> liftIO ( endpointOptions' . mUnit . nModel <$> getNodeIO root nId )
    :<|> liftIO ( algToVizJS . alg . nModel <$> getNodeIO root nId )
    :<|> (\name -> liftIO ( do
        node <- getNodeIO root nId
        let ModelState{ mDataFlowGraph } = nModel node
        unless (nIsComplete node) $ error "test bench not allow for non complete synthesis"
        writeAndRunTestbench Project
            { pName=name
            , pLibPath=joinPath ["..", "..", "hdl"]
            , pPath=joinPath ["gen", name]
            , pUnit=mUnit $ nModel node
            , pTestCntx=simulateDataFlowGraph def def mDataFlowGraph
            }
    ))
    :<|> simpleCompilerServer root nId
    where
        alg ModelState{ mDataFlowGraph=DFCluster nodes } = map (\(DFLeaf f) -> f) nodes
        alg _                                            = error "unsupported algorithm structure"
        endpointOptions' BusNetwork{ bnPus }
            = let f (tag, pu) = zip (repeat tag) $ endpointOptions pu
            in concatMap f $ M.assocs bnPus



type SimpleCompilerAPI tag v x t
    =    "edges" :> Get '[JSON] [ G Edge tag v x t ]
    :<|> "simpleSynthesis" :> Post '[JSON] NId
    :<|> "smartBindSynthesisIO" :> Post '[JSON] NId
    :<|> "obviousBindThread" :> Post '[JSON] NId
    :<|> "allBestThread" :> QueryParam' '[Required] "n" Int :> Post '[JSON] NId

simpleCompilerServer root n
    =    liftIO ( getEdgesIO =<< getNodeIO root n )
    :<|> liftIO ( nId <$> (simpleSynthesisIO =<< getNodeIO root n))
    :<|> liftIO ( nId <$> (smartBindSynthesisIO =<< getNodeIO root n))
    :<|> liftIO ( nId <$> (obviousBindThreadIO =<< getNodeIO root n))
    :<|> ( \deep -> liftIO ( nId <$> (allBestThreadIO deep =<< getNodeIO root n)) )



-- *Internal

data SynthesisNodeView
    = SynthesisNodeView
        { svNnid             :: NId
        , svCntx             :: [String]
        , svIsComplete       :: Bool
        , svIsEdgesProcessed :: Bool
        , svDuration         :: Int
        , svCharacteristic   :: Float -- FIXME:
        , svOptionType       :: String
        }
    deriving ( Generic )

instance ToJSON SynthesisNodeView

synthesisNodeView Node{ nId, nIsComplete, nModel, nEdges, nOrigin } = do
    nodesM <- readTVarIO nEdges
    nodes <- case nodesM of
        Just ns -> mapM (synthesisNodeView . eNode) ns
        Nothing -> return []
    return T.Node
        { T.rootLabel=SynthesisNodeView
            { svNnid=nId
            , svCntx=[]
            , svIsComplete=nIsComplete
            , svIsEdgesProcessed=isJust nodesM
            , svDuration=fromEnum $ targetProcessDuration nModel
            , svCharacteristic=maybe (read "NaN") eObjectiveFunctionValue nOrigin
            , svOptionType=case nOrigin of
                Just Edge{ eOption=BindingOption{} }  -> "Bind"
                Just Edge{ eOption=DataflowOption{} } -> "Transport"
                Just Edge{ eOption=RefactorOption{} } -> "Refactor"
                Nothing                               -> "-"
            }
        , T.subForest=nodes
        }
