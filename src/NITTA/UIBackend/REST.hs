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
    , BackendCntx(..)
    ) where

import           Control.Monad.Except
import           Data.Aeson
import           Data.Default
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import           GHC.Generics
import           NITTA.Intermediate.Simulation
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Problems
import           NITTA.Model.ProcessorUnits.Time
import           NITTA.Model.TargetSystem
import           NITTA.Model.Types
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Types
import           NITTA.Project.Utils             (writeAndRunTestbench)
import           NITTA.Synthesis.Method
import           NITTA.Synthesis.Tree
import           NITTA.UIBackend.Marshalling
import           NITTA.UIBackend.Timeline
import           NITTA.UIBackend.VisJS           (VisJS, algToVizJS)
import           NITTA.Utils
import           Servant
import           System.FilePath                 (joinPath)



data BackendCntx tag v x t
    = BackendCntx
        { root           :: G Node tag v x t
          -- ^root synthesis node
        , receivedValues :: [ (v, [x]) ]
          -- ^lists of received by IO values
        }


type SynthesisAPI tag v x t
    =    "synthesisTree"                  :> Get '[JSON] (TreeView SynthesisNodeView)
    :<|> "synthesis" :> Capture "nId" NId :> GetSynthesis tag v x t
    :<|> "synthesis" :> Capture "nId" NId :> PostSynthesis tag v x t
    :<|> "testbench" :> Capture "nId" NId
                     :> QueryParam' '[Required] "name" String
                     :> Post '[JSON] (TestbenchReport v x)

synthesisServer cntx@BackendCntx{ root }
    =    liftIO ( viewNodeTree root )
    :<|> ( \nId -> getSynthesis cntx nId )
    :<|> ( \nId -> postSynthesis cntx nId )
    :<|> testBench cntx



type GetSynthesis tag v x t
    =                    Get '[JSON] (G Node tag v x t) -- FIXME: TypeScriptDeclaration
    :<|> "path"       :> Get '[JSON] [ NodeView tag v x t ]
    :<|> "originEdge" :> Get '[JSON] (Maybe (G Edge tag v x t)) -- FIXME: TypeScriptDeclaration
    :<|> "edges"      :> Get '[JSON] [ EdgeView tag v x t ]

    :<|> "intermediateView" :> Get '[JSON] VisJS
    :<|> "timelines"        :> Get '[JSON] (ProcessTimelines t) -- FIXME: TypeScriptDeclaration

    :<|> "debug"       :> Get '[JSON] (Debug tag v t)
    :<|> "puEndpoints" :> Get '[JSON] [ UnitEndpointView tag v ]

getSynthesis BackendCntx{ root } nId
    =    liftIO ( getNodeIO root nId )
    :<|> liftIO ( map view <$> getNodePathIO root nId )
    :<|> liftIO ( nOrigin <$> getNodeIO root nId )
    :<|> liftIO ( map view <$> ( getEdgesIO =<< getNodeIO root nId ) )

    :<|> liftIO ( algToVizJS . extractFuns . nModel <$> getNodeIO root nId )
    :<|> liftIO ( processTimelines . process . mUnit . nModel <$> getNodeIO root nId )

    :<|> liftIO ( debug root nId )
    :<|> liftIO ( dbgEndpointOptions <$> debug root nId )



type PostSynthesis tag v x t
    =    "stateOfTheArtSynthesisIO" :> Post '[JSON] NId
    :<|> "simpleSynthesis"          :> Post '[JSON] NId
    :<|> "smartBindSynthesisIO"     :> Post '[JSON] NId

    :<|> "bestStep"                 :> Post '[JSON] NId
    :<|> "obviousBindThread"        :> Post '[JSON] NId
    :<|> "allBindsAndRefsIO"        :> Post '[JSON] NId

    :<|> "allBestThread" :> QueryParam' '[Required] "n" Int :> Post '[JSON] NId

postSynthesis BackendCntx{ root } n
    =    liftIO ( nId <$> ( stateOfTheArtSynthesisIO =<< getNodeIO root n ) )
    :<|> liftIO ( nId <$> ( simpleSynthesisIO        =<< getNodeIO root n ) )
    :<|> liftIO ( nId <$> ( smartBindSynthesisIO     =<< getNodeIO root n ) )

    :<|> liftIO ( nId <$> ( bestStepIO               =<< getNodeIO root n ) )
    :<|> liftIO ( nId <$> ( obviousBindThreadIO      =<< getNodeIO root n ) )
    :<|> liftIO ( nId <$> ( allBindsAndRefsIO        =<< getNodeIO root n ) )

    :<|> ( \deep -> liftIO ( nId <$> ( allBestThreadIO deep =<< getNodeIO root n ) ) )



testBench BackendCntx{ root, receivedValues } nId pName = liftIO $ do
        node <- getNodeIO root nId
        let ModelState{ mDataFlowGraph } = nModel node
        unless (nIsComplete node) $ error "test bench not allow for non complete synthesis"
        writeAndRunTestbench Project
            { pName
            , pLibPath=joinPath ["..", "..", "hdl"]
            , pPath=joinPath ["gen", pName]
            , pUnit=mUnit $ nModel node
            , pTestCntx=simulateDataFlowGraph def receivedValues mDataFlowGraph
            }


------------------------------------------------------------

extractFuns ModelState{ mDataFlowGraph=DFCluster nodes }
    = map (\(DFLeaf f) -> f) nodes
extractFuns _ = error "unsupported algorithm structure"



-- |Type for CAD debugging. Used for extracting internal information.
data Debug tag v t = Debug
        { dbgEndpointOptions           :: [ UnitEndpointView tag v ]
        , dbgFunctionLocks             :: [ ( String, [ Lock v ] ) ]
        , dbgCurrentStateFunctionLocks :: [ ( String, [ Lock v ] ) ]
        , dbgPULocks                   :: [ ( String, [ Lock v ] ) ]
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
            = let f (tag, pu) = map (\(t, ep) -> UnitEndpointView t $ view ep) $ zip (repeat tag) $ endpointOptions pu
            in concatMap f $ M.assocs bnPus
