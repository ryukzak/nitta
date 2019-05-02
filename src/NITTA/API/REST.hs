{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.API.REST
Description : REST API description for NITTA backend
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.API.REST
    ( SynthesisAPI
    , synthesisServer
    ) where

import           Control.Concurrent.STM
import           Control.Monad.Except
import           Data.Aeson
import qualified Data.Map               as M
import           Data.Maybe
import qualified Data.Tree              as T
import           GHC.Generics
import           NITTA.API.Marshalling  ()
import           NITTA.API.VisJS        (VisJS, algToVizJS)
import           NITTA.BusNetwork
import           NITTA.DataFlow
import           NITTA.Project          (writeAndRunTestBench)
import           NITTA.SynthesisMethod
import           NITTA.Types
import           NITTA.Types.Project
import           NITTA.Types.Synthesis
import           Servant
import           System.FilePath        (joinPath)



-- *REST API

type SynthesisAPI title v x t
    =    "synthesis" :> Get '[JSON] (T.Tree SynthesisNodeView)
    :<|> "synthesis" :> Capture "nId" NId :> WithSynthesis title v x t

synthesisServer root
    =    liftIO ( synthesisNodeView root )
    :<|> \nId -> withSynthesis root nId



type WithSynthesis title v x t
    =    Get '[JSON] (Node title v x t)
    :<|> "edge" :> Get '[JSON] (Maybe (Edge title v x t))
    :<|> "model" :> Get '[JSON] (ModelState title v x t)
    :<|> "endpointOptions" :> Get '[JSON] [(title, Option (EndpointDT v t))]
    :<|> "model" :> "alg" :> Get '[JSON] VisJS
    :<|> "testBench" :> "output" :> QueryParam' '[Required] "name" String :> Get '[JSON] TestBenchReport
    :<|> SimpleCompilerAPI title v x t

withSynthesis root nId
    =    liftIO ( getNodeIO root nId )
    :<|> liftIO ( nOrigin <$> getNodeIO root nId )
    :<|> liftIO ( nModel <$> getNodeIO root nId )
    :<|> liftIO ( endpointOptions . processor . nModel <$> getNodeIO root nId )
    :<|> liftIO ( algToVizJS . alg . nModel <$> getNodeIO root nId )
    :<|> (\name -> liftIO ( do
        node <- getNodeIO root nId
        unless (nIsComplete node) $ error "test bench not allow for non complete synthesis"
        writeAndRunTestBench Project
            { projectName=name
            , libraryPath="../.."
            , projectPath=joinPath ["hdl", "gen", name]
            , processorModel=processor $ nModel node
            , testCntx=Nothing
            }
    ))
    :<|> simpleCompilerServer root nId
    where
        alg Frame{ dfg=DFG nodes } = map (\(DFGNode f) -> f) nodes
        alg _                      = error "unsupported algorithm structure"
        endpointOptions BusNetwork{ bnPus }
            = let f (title, pu) = zip (repeat title) $ options endpointDT pu
            in concatMap f $ M.assocs bnPus



type SimpleCompilerAPI title v x t
    =    "edges" :> Get '[JSON] [ Edge title v x t ]
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
        , svCharacteristic   :: Float
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
            , svCharacteristic=maybe (read "NaN") eCharacteristic nOrigin
            , svOptionType=case nOrigin of
                Just Edge{ eOption=BindingOption{} }  -> "Bind"
                Just Edge{ eOption=DataFlowOption{} } -> "Transport"
                Just Edge{ eOption=RefactorOption{} } -> "Refactor"
                Nothing                               -> "-"
            }
        , T.subForest=nodes
        }
