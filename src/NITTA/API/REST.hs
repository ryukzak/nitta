{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
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
        { svNnid :: Nid
        , svCntx :: [String]
        }
    deriving (Generic)

instance ToJSON SynthesisView

view n = (\(nid, Synthesis{ sCntx } ) -> SynthesisView{ svNnid=nid, svCntx=map show sCntx }) <$> mzip (nids n) n


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
    :<|> "model" :> Get '[JSON] (SystemState String String String Int (TaggedTime String Int))
    :<|> "simple" :> "options" :> Get '[JSON] [ RESTOption ]
    :<|> "simpleManual" :> QueryParam' '[Required] "manual" Int :> Post '[JSON] Nid -- manualStep
    :<|> "simple" :> QueryParam' '[Required] "onlyOneStep" Bool :> Post '[JSON] Nid

withSynthesis st nid
    =    get st nid
    :<|> getModel st nid
    :<|> simpleCompilerOptions st nid
    :<|> ( \md -> simpleCompilerManual st nid md )
    :<|> \onlyOneStep -> simpleCompiler st nid onlyOneStep



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



simpleCompiler st nid onlyOneStep
    = liftSTM $ do
        n <- readTVar st
        let Just (n', nid') = if onlyOneStep
            then update (simpleSynthesisOneStep def) nid n
            else update (Just . simpleSynthesis def) nid n
        writeTVar st n'
        return nid'

simpleCompilerManual st nid md
    = liftSTM $ do
        n <- readTVar st
        let Just (n', nid') = update (simpleSynthesisManual def md) nid n
        writeTVar st n'
        return nid'



-- *Internal

liftSTM stm = do
    e <- liftIO (atomically $ catchSTM (Right <$> stm) (return . Left))
    either throwError return e
