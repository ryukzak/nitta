{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

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
import qualified Data.Tree              as T
import           GHC.Generics
import           NITTA.API.Marshalling  ()
import           NITTA.SymthesisMethod
import           NITTA.DataFlow
import           NITTA.Project          (writeAndRunTestBench)
import           NITTA.Types            (F)
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
    :<|> "model" :> Get '[JSON] (ModelState title v x t)
    :<|> "model" :> "alg" :> Get '[JSON] [F v x]
    :<|> "testBench" :> "output" :> QueryParam' '[Required] "name" String :> Get '[JSON] TestBenchReport
    :<|> SimpleCompilerAPI title v x t

withSynthesis root nId
    =    liftIO ( getNodeIO root nId )
    :<|> liftIO ( nModel <$> getNodeIO root nId )
    :<|> liftIO ( alg . nModel <$> getNodeIO root nId )
    :<|> (\name -> liftIO ( do
        node <- getNodeIO root nId
        writeAndRunTestBench Project
            { projectName=name
            , libraryPath="../.."
            , projectPath=joinPath ["hdl", "gen", name]
            , processorModel=processor $ nModel node
            , testCntx=Nothing
            , targetPlatforms=[ Makefile ]
            }
    ))
    :<|> simpleCompilerServer root nId
    where
        alg Frame{ dfg=DFG nodes } = map (\(DFGNode f) -> f) nodes
        alg _                      = error "unsupported algorithm structure"



type SimpleCompilerAPI title v x t
    =    "simple" :> "options" :> Get '[JSON] [ Edge title v x t ]
    :<|> "simpleSynthesis" :> Post '[JSON] NId
    :<|> "obliousBindThread" :> Post '[JSON] NId
    :<|> "allBestThread" :> QueryParam' '[Required] "n" Int :> Post '[JSON] NId
    -- :<|> "simpleManual" :> QueryParam' '[Required] "manual" Int :> Post '[JSON] NId -- manualStep
    -- :<|> "simple" :> QueryParam' '[Required] "onlyOneStep" Bool :> Post '[JSON] NId

simpleCompilerServer root n
    =    liftIO ( getEdgesIO =<< getNodeIO root n )
    :<|> liftIO ( nId <$> (simpleSynthesisIO =<< getNodeIO root n))
    :<|> liftIO ( nId <$> (obliousBindThreadIO =<< getNodeIO root n))
    :<|> ( \deep -> liftIO ( nId <$> (allBestThreadIO deep =<< getNodeIO root n)) )
    -- :<|> ( \ix -> updateSynthesis (apply (simpleSynthesisStep "manual") SynthesisStep{ setup=simple, ix=Just ix }) st nId )
    -- :<|> \case
    --     True -> updateSynthesis (apply (simpleSynthesisStep "auto") SynthesisStep{ setup=simple, ix=Nothing }) st nId
    --     False -> updateSynthesis (Just . recApply (simpleSynthesisStep "auto") SynthesisStep{ setup=simple, ix=Nothing }) st nId



-- *Internal

data SynthesisNodeView
    = SynthesisNodeView
        { svNnid     :: NId
        , svCntx     :: [String]
        , svStatus   :: Bool
        , svDuration :: Int
        }
    deriving (Generic)

instance ToJSON SynthesisNodeView

synthesisNodeView Node{ nId, nIsComplete, nModel, nEdges } = do
    nodes <- readTVarIO nEdges >>= \case
        Just ns -> mapM (synthesisNodeView . eNode) ns
        Nothing -> return []
    return T.Node
        { T.rootLabel=SynthesisNodeView
            { svNnid=nId
            , svCntx=[]
            , svStatus=nIsComplete
            , svDuration=fromEnum $ targetProcessDuration nModel
            }
        , T.subForest=nodes
        }
