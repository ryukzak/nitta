{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
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

import           Control.Monad.Except
import           Data.Aeson
import           Data.Default
import qualified Data.Map                         as M
import qualified Data.Set                         as S
import           GHC.Generics
import           NITTA.Intermediate.Simulation
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.TargetSystem
import           NITTA.Model.Types
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Types
import           NITTA.Project.Utils              (writeAndRunTestbench)
import           NITTA.Synthesis.Method
import           NITTA.Synthesis.Types
import           NITTA.UIBackend.Marshalling
import           NITTA.UIBackend.Timeline
import           NITTA.UIBackend.VisJS            (VisJS, algToVizJS)
import           NITTA.Utils
import           Numeric.Interval
import           Servant
import           System.FilePath                  (joinPath)



type SynthesisAPI tag v x t
    =    "synthesis" :> Get '[JSON] (TreeView SynthesisNodeView)
    :<|> "synthesis" :> Capture "nId" NId :> WithSynthesis tag v x t

synthesisServer root
    =    liftIO ( view root )
    :<|> \nId -> withSynthesis root nId



type WithSynthesis tag v x t
    =    Get '[JSON] (G Node tag v x t)
    :<|> "edge" :> Get '[JSON] (Maybe (G Edge tag v x t))
    :<|> "model" :> Get '[JSON] (ModelState (BusNetwork tag v x t) v x)
    :<|> "timelines" :> Get '[JSON] (ProcessTimelines t)
    :<|> "debug" :> Get '[JSON] (Debug tag v t)
    :<|> "history" :> Get '[JSON] [SynthesisDecisionView tag v x (Interval t)]
    :<|> "model" :> "alg" :> Get '[JSON] VisJS
    :<|> "testBench" :> "output" :> QueryParam' '[Required] "name" String :> Get '[JSON] (TestbenchReport v x)
    :<|> SimpleCompilerAPI tag v x t

withSynthesis root nId
    =    liftIO ( getNodeIO root nId )
    :<|> liftIO ( nOrigin <$> getNodeIO root nId )
    :<|> liftIO ( nModel <$> getNodeIO root nId )
    :<|> liftIO ( processTimelines . process . mUnit . nModel <$> getNodeIO root nId )
    :<|> liftIO ( debug root nId )
    :<|> liftIO ( map view <$> getSynthesisHistoryIO root nId )
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


-- |Type for CAD debugging. Used for extracting internal information.
data Debug tag v t = Debug
        { dbgEndpointOptions           :: [ ( tag, EndpointOption v t ) ]
        , dbgFunctionLocks             :: [ ( String, [Lock v] ) ]
        , dbgCurrentStateFunctionLocks :: [ ( String, [Lock v] ) ]
        , dbgPULocks                   :: [ ( String, [Lock v] ) ]
        }
    deriving ( Generic )

instance ( ToJSON tag, ToJSON t, Time t ) => ToJSON (Debug tag String t)

debug root nId = do
    node <- getNodeIO root nId
    let dbgFunctionLocks = map (\f -> (show f, locks f)) $ functions $ mDataFlowGraph $ nModel node
        already = transferred $ mUnit $ nModel node
    return Debug
        { dbgEndpointOptions=endpointOptions' $ mUnit $ nModel node
        , dbgFunctionLocks
        , dbgCurrentStateFunctionLocks=
            [ (tag, filter (\Lock{ lockBy, locked } -> S.notMember lockBy already && S.notMember locked already) ls)
            | (tag, ls) <- dbgFunctionLocks
            ]
        , dbgPULocks=map (\(tag, pu) -> (tag, locks pu)) $ M.assocs $ bnPus $ mUnit $ nModel node
        }
    where
        endpointOptions' BusNetwork{ bnPus }
            = let f (tag, pu) = zip (repeat tag) $ endpointOptions pu
            in concatMap f $ M.assocs bnPus


type SimpleCompilerAPI tag v x t
    =    "edges" :> Get '[JSON] [ EdgeView tag v x t ]
    :<|> "simpleSynthesis" :> Post '[JSON] NId
    :<|> "smartBindSynthesisIO" :> Post '[JSON] NId
    :<|> "obviousBindThread" :> Post '[JSON] NId
    :<|> "allBestThread" :> QueryParam' '[Required] "n" Int :> Post '[JSON] NId
    :<|> "allBindsAndRefsIO" :> Post '[JSON] NId

simpleCompilerServer root n
    =    liftIO ( return . map view =<< getEdgesIO =<< getNodeIO root n )
    :<|> liftIO ( nId <$> (simpleSynthesisIO =<< getNodeIO root n))
    :<|> liftIO ( nId <$> (smartBindSynthesisIO =<< getNodeIO root n))
    :<|> liftIO ( nId <$> (obviousBindThreadIO =<< getNodeIO root n))
    :<|> ( \deep -> liftIO ( nId <$> (allBestThreadIO deep =<< getNodeIO root n)) )
    :<|> liftIO ( nId <$> (allBindsAndRefsIO =<< getNodeIO root n))
