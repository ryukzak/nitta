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
import           Data.Default
import           Data.Tree
import           GHC.Generics
import           NITTA.API.Marshalling  ()
import           NITTA.Compiler
import           NITTA.DataFlow
import           NITTA.Types
import           NITTA.Types.Synthesis
import           Servant



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
        f (nid, Synthesis sModel sCntx sStatus ) = SynthesisView
            { svNnid=nid
            , svCntx=map show sCntx
            , svStatus=sStatus
            , svDuration=fromEnum $ targetProcessDuration sModel
            }


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
    :<|> SimpleCompilerAPI

withSynthesis st nid
    =    get st nid
    :<|> getModel st nid
    :<|> simpleCompilerServer st nid



type SimpleCompilerAPI
    =    "simple" :> "options" :> Get '[JSON] [ RESTOption ]
    :<|> "simple" :> "obviousBind" :> Post '[JSON] Nid
    :<|> "simple" :> "allThreads" :> QueryParam' '[Required] "deep" Int :> Post '[JSON] Nid
    :<|> "simpleManual" :> QueryParam' '[Required] "manual" Int :> Post '[JSON] Nid -- manualStep
    :<|> "simple" :> QueryParam' '[Required] "onlyOneStep" Bool :> Post '[JSON] Nid

simpleCompilerServer st nid
    =    simpleCompilerOptions st nid
    :<|> updateSynthesis (Just . compilerObviousBind def) st nid
    :<|> ( \deep -> updateSynthesis (Just . compilerAllTheads def deep) st nid )
    :<|> ( \md -> updateSynthesis (apply (simpleSynthesisStep def md "manual")) st nid )
    :<|> \case
            True -> updateSynthesis (apply (simpleSynthesisStep def 0 "auto")) st nid
            False -> updateSynthesis (Just . recApply (simpleSynthesisStep def 0 "auto")) st nid





get st nid = do
    root <- liftSTM $ readTVar st
    return $ getSynthesis nid root

simpleCompilerOptions st nid = do
    root <- liftSTM $ readTVar st
    let Node{ rootLabel=Synthesis{ sModel } } = getSynthesis nid root
    let compilerState = def{ state=sModel }
    return $ optionsWithMetrics compilerState

getModel st nid = do
    root <- liftSTM $ readTVar st
    let Node{ rootLabel=Synthesis{ sModel } } = getSynthesis nid root
    return sModel

updateSynthesis f st nid = liftSTM $ do
    n <- readTVar st
    let Just (n', nid') = update f nid n
    writeTVar st n'
    return nid'



-- *Internal

liftSTM stm = do
    e <- liftIO (atomically $ catchSTM (Right <$> stm) (return . Left))
    either throwError return e
