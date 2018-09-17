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
    ( Synthesis(..)
    , SynthesisAPI
    , synthesisServer
    ) where

import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.Zip      (mzip)
import           Data.Aeson
import           Data.Tree
import           GHC.Generics
import           NITTA.API.Marshalling  ()
import           NITTA.Compiler
import           NITTA.DataFlow
import           NITTA.Project          (Project (..), TestBenchReport,
                                         writeAndRunTestBench)
import           NITTA.Types
import           NITTA.Types.Synthesis
import           Servant
import           System.FilePath        (joinPath)



type SYN = SynthesisTree String String String Int (TaggedTime String Int)


-- *REST API Projections.

data SynthesisView
    = SynthesisView
        { svNnid     :: Nid
        , svCntx     :: [String]
        , svStatus   :: SynthesisStatus
        , svDuration :: Int
        }
    deriving (Generic)

instance ToJSON SynthesisView

-- FIXME: Filter a synthesis tree to the fastest (or n fastet) process.

view n = f <$> mzip (nidsTree n) n
    where
        f ( nid, Synthesis sModel sCntx sStatus _sCache ) = SynthesisView
            { svNnid=nid
            , svCntx=map show sCntx
            , svStatus=sStatus
            , svDuration=fromEnum $ targetProcessDuration sModel
            }


-- FIXME: Convert to record.
type RESTOption =
    ( Int
    , GlobalMetrics
    , SpecialMetrics
    , Option (CompilerDT String String String (TaggedTime String Int))
    , Decision (CompilerDT String String String (TaggedTime String Int))
    )


-- *REST API

type SynthesisAPI
    =    "synthesis" :> Get '[JSON] (Tree SynthesisView)
    :<|> "synthesis" :> Capture "nid" Nid :> WithSynthesis

synthesisServer st
    =    ( view <$> liftSTM (readTVar st))
    :<|> \nid -> withSynthesis st nid



type WithSynthesis
    =    Get '[JSON] SYN
    :<|> "model" :> Get '[JSON] (ModelState String String String Int (TaggedTime String Int))
    :<|> "testBench" :> "output" :> QueryParam' '[Required] "name" String :> Get '[JSON] TestBenchReport
    :<|> SimpleCompilerAPI

withSynthesis st nid
    =    get st nid
    :<|> getModel st nid
    :<|> ( \name -> getTestBenchOutput st nid name )
    :<|> simpleCompilerServer st nid



type SimpleCompilerAPI
    =    "simple" :> "options" :> Get '[JSON] [ RESTOption ]
    :<|> "simple" :> "obviousBind" :> Post '[JSON] Nid
    :<|> "simple" :> "allThreads" :> QueryParam' '[Required] "deep" Int :> Post '[JSON] Nid
    :<|> "simpleManual" :> QueryParam' '[Required] "manual" Int :> Post '[JSON] Nid -- manualStep
    :<|> "simple" :> QueryParam' '[Required] "onlyOneStep" Bool :> Post '[JSON] Nid

simpleCompilerServer st nid
    =    simpleCompilerOptions st nid
    :<|> updateSynthesis (Just . synthesisObviousBind) st nid
    :<|> ( \deep -> updateSynthesis (Just . simpleSynthesisAllThreads simple deep) st nid )
    :<|> ( \ix -> updateSynthesis (apply (simpleSynthesisStep "manual") SynthesisStep{ setup=simple, ix }) st nid )
    :<|> \case
            True -> updateSynthesis (apply (simpleSynthesisStep "auto") SynthesisStep{ setup=simple, ix=0 }) st nid
            False -> updateSynthesis (Just . recApply (simpleSynthesisStep "auto") SynthesisStep{ setup=simple, ix=0 }) st nid



get st nid = do
    root <- liftSTM $ readTVar st
    return $ getSynthesisNode nid root

simpleCompilerOptions st nid = do
    root <- liftSTM $ readTVar st
    let Synthesis{ sModel } = getSynthesis nid root
    return $ optionsWithMetrics sModel

getModel st nid = do
    root <- liftSTM $ readTVar st
    let Synthesis{ sModel } = getSynthesis nid root
    return sModel

updateSynthesis f st nid = liftSTM $ do
    n <- readTVar st
    let Just (n', nid') = update f nid n
    writeTVar st n'
    return nid'

getTestBenchOutput st _nid name = do
    Node{ rootLabel=Synthesis{ sModel } } <- liftSTM $ readTVar st
    let prj = Project
            { projectName=name
            , libraryPath="../.."
            , projectPath=joinPath ["hdl", "gen", name]
            , model=simpleSynthesis sModel
            , testCntx=Nothing
            }
    liftIO $ writeAndRunTestBench prj


-- *Internal

liftSTM stm = do
    e <- liftIO (atomically $ catchSTM (Right <$> stm) (return . Left))
    either throwError return e
