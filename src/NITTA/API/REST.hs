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
--     :<|> SimpleCompilerAPI x t

withSynthesis root nid
    =    liftIO ( getSynthesisNodeIO root nid )
    :<|> liftIO ( sModel <$> getSynthesisNodeIO root nid )
    :<|> liftIO ( alg . sModel <$> getSynthesisNodeIO root nid )
    :<|> \name -> liftIO ( do
        sModel <- sModel <$> getSynthesisNodeIO root nid
        node <- simpleSynthesisIO sModel
        writeAndRunTestBench Project
            { projectName=name
            , libraryPath="../.."
            , projectPath=joinPath ["hdl", "gen", name]
            , processorModel=processor node
            , testCntx=Nothing
            , targetPlatforms=[ Makefile ]
            }
    )
--     :<|> simpleCompilerServer st nid
    where
        alg Frame{ dfg=DFG nodes } = map (\(DFGNode f) -> f) nodes
        alg _                      = error "unsupported algorithm structure"



-- type SimpleCompilerAPI x t
--     =    "simple" :> "options" :> Get '[JSON] [ WithMetric (CompilerDT String String x t) ]
--     -- :<|> "simple" :> "obviousBind" :> Post '[JSON] Nid
--     -- :<|> "simple" :> "allThreads" :> QueryParam' '[Required] "deep" Int :> Post '[JSON] Nid
--     -- :<|> "simpleManual" :> QueryParam' '[Required] "manual" Int :> Post '[JSON] Nid -- manualStep
--     -- :<|> "simple" :> QueryParam' '[Required] "onlyOneStep" Bool :> Post '[JSON] Nid

-- simpleCompilerServer st nid
--     =    simpleCompilerOptions st nid
--     -- :<|> updateSynthesis (Just . synthesisObviousBind) st nid
--     -- :<|> ( \deep -> updateSynthesis (Just . simpleSynthesisAllThreads simple deep) st nid )
--     -- :<|> ( \ix -> updateSynthesis (apply (simpleSynthesisStep "manual") SynthesisStep{ setup=simple, ix=Just ix }) st nid )
--     -- :<|> \case
--     --         True -> updateSynthesis (apply (simpleSynthesisStep "auto") SynthesisStep{ setup=simple, ix=Nothing }) st nid
--     --         False -> updateSynthesis (Just . recApply (simpleSynthesisStep "auto") SynthesisStep{ setup=simple, ix=Nothing }) st nid



-- simpleCompilerOptions st nid = do
--     root <- liftSTM $ readTVar st
--     let Synthesis{ sModel } = getSynthesis nid root
--     return $ optionsWithMetrics simple sModel

-- updateSynthesis f st nid = liftSTM $ do
--     n <- readTVar st
--     case update f nid n of
--         Just (n', nid') -> do
--             writeTVar st n'
--             return nid'
--         Nothing -> return nid


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
    nodesM <- readTVarIO sSubNodes
    nodes <- case nodesM of
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
