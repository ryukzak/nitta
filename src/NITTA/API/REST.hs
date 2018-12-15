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
import           Data.Tree
import           GHC.Generics
import           NITTA.API.Marshalling  ()
import           NITTA.Compiler
import           NITTA.DataFlow
import           NITTA.Project          (writeAndRunTestBench)
import           NITTA.Types            (F)
import           NITTA.Types.Project
import           NITTA.Types.Synthesis
import           Servant
import           System.FilePath        (joinPath)



-- *REST API

type SynthesisAPI title v x t
    =    "synthesis" :> Get '[JSON] (Tree SynthesisNodeView)
    :<|> "synthesis" :> Capture "nid" Nid :> WithSynthesis title v x t

synthesisServer root
    =    liftIO ( synthesisNodeView root )
    :<|> \nid -> withSynthesis root nid



type WithSynthesis title v x t
    =    Get '[JSON] (SynthesisNode title v x t)
    :<|> "model" :> Get '[JSON] (ModelState title v x t)
    :<|> "model" :> "alg" :> Get '[JSON] [F v x]
    :<|> "testBench" :> "output" :> QueryParam' '[Required] "name" String :> Get '[JSON] TestBenchReport
    :<|> SimpleCompilerAPI title v x t

withSynthesis root nid
    =    liftIO ( getSynthesisNodeIO root nid )
    :<|> liftIO ( sModel <$> getSynthesisNodeIO root nid )
    :<|> liftIO ( alg . sModel <$> getSynthesisNodeIO root nid )
    :<|> (\name -> liftIO ( do
        node <- getSynthesisNodeIO root nid
        writeAndRunTestBench Project
            { projectName=name
            , libraryPath="../.."
            , projectPath=joinPath ["hdl", "gen", name]
            , processorModel=processor $ sModel node
            , testCntx=Nothing
            , targetPlatforms=[ Makefile ]
            }
    ))
    :<|> simpleCompilerServer root nid
    where
        alg Frame{ dfg=DFG nodes } = map (\(DFGNode f) -> f) nodes
        alg _                      = error "unsupported algorithm structure"



type SimpleCompilerAPI title v x t
    =    "simple" :> "options" :> Get '[JSON] [ SynthesisSubNode title v x t ]
    -- :<|> "simpleSynthesis" :> Post '[JSON] Nid
    :<|> "obliousBindThread" :> Post '[JSON] Nid
    :<|> "allBestThread" :> QueryParam' '[Required] "n" Int :> Post '[JSON] Nid
    -- :<|> "simpleManual" :> QueryParam' '[Required] "manual" Int :> Post '[JSON] Nid -- manualStep
    -- :<|> "simple" :> QueryParam' '[Required] "onlyOneStep" Bool :> Post '[JSON] Nid

simpleCompilerServer root n
    =    liftIO ( getSynthesisSubNodesIO =<< getSynthesisNodeIO root n )
    -- :<|> liftIO ( nid <$> (simpleSynthesisIO =<< getSynthesisNodeIO root n))
    :<|> liftIO ( nid <$> (obliousBindThreadIO =<< getSynthesisNodeIO root n))
    :<|> ( \deep -> liftIO ( nid <$> (allBestThreadIO deep =<< getSynthesisNodeIO root n)) )
    -- :<|> ( \ix -> updateSynthesis (apply (simpleSynthesisStep "manual") SynthesisStep{ setup=simple, ix=Just ix }) st nid )
    -- :<|> \case
    --     True -> updateSynthesis (apply (simpleSynthesisStep "auto") SynthesisStep{ setup=simple, ix=Nothing }) st nid
    --     False -> updateSynthesis (Just . recApply (simpleSynthesisStep "auto") SynthesisStep{ setup=simple, ix=Nothing }) st nid



-- *Internal

data SynthesisNodeView
    = SynthesisNodeView
        { svNnid     :: Nid
        , svCntx     :: [String]
        , svStatus   :: Bool
        , svDuration :: Int
        }
    deriving (Generic)

instance ToJSON SynthesisNodeView

synthesisNodeView SynthesisNode{ nid, isComplete, sModel, sSubNodes } = do
    nodes <- readTVarIO sSubNodes >>= \case
        Just ns -> mapM (synthesisNodeView . subNode) ns
        Nothing -> return []
    return Node
        { rootLabel=SynthesisNodeView
            { svNnid=nid
            , svCntx=[]
            , svStatus=isComplete
            , svDuration=fromEnum $ targetProcessDuration sModel
            }
        , subForest=nodes
        }
