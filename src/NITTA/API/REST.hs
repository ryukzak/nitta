{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
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
    -- , synthesis
    -- , rootNode
    , SynthesisAPI
    , synthesisServer
    ) where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad.Except
import           Control.Monad.Zip             (mzip)
import           Data.Aeson
import           Data.Default
import           Data.Hashable
import           Data.List.Split
import           Data.Map                      (Map, fromList)
import           Data.Maybe                    (fromMaybe, isJust)
import qualified Data.Text                     as T
import           Data.Tree
import           Debug.Trace
import           GHC.Generics
import           ListT                         (toList)
import           NITTA.Compiler
import           NITTA.DataFlow
import           NITTA.DataFlow                ()
import           NITTA.Types
import           NITTA.Types.Synthesis
import           NITTA.Utils.JSON              ()
import           Servant
import qualified STMContainers.Map             as M
import           Text.InterpolatedString.Perl6 (qq)
import           Text.Read.Lex                 (numberToInteger)



type CSt = CompilerStep String String String Int (TaggedTime String Int)
type SYN = SynthesisNode String String String Int (TaggedTime String Int)

instance ToJSON (Synthesis String String String Int (TaggedTime String Int))


-- *REST API Projections.

data SynthesisView
    = SynthesisView
        { svNnid :: Nid
        , svCntx :: [String]
        }
    deriving (Generic)

instance ToJSON SynthesisView

view n = fmap (\(nid, Synthesis{ sCntx } ) -> SynthesisView nid sCntx) $ mzip (nids n) n

instance ToJSON Nid where
    toJSON nid = toJSON $ show nid

instance FromJSON Nid where
    parseJSON v = read <$> parseJSON v

instance FromHttpApiData Nid where
    parseUrlPiece = Right . read . T.unpack

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
    :<|> "simple" :> QueryParam' '[Required] "onlyOneStep" Bool :> Post '[JSON] Nid

--     :<|> "decisions" :> Get '[JSON] [Decision (CompilerDT String String String (TaggedTime String Int))]


withSynthesis st nid
    =    get st nid
    :<|> getModel st nid
    :<|> simpleCompilerOptions st nid
    :<|> \onlyOneStep -> simpleCompiler st nid onlyOneStep
--     :<|> stepServer st node




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
            then simpleSynthesisOneStepAt def nid n
            else simpleSynthesisAt def nid n
        writeTVar st n'
        return $ trace (show nid') nid'




-- type WithStep
--     =    Get '[JSON] CSt
--     -- Дублирование auto в path - костыль. Проблема в следующем - параметры
--     -- и флаги не влияют на имя функции в автоматически генерируемом API
--     -- для JS, что приводит к утере одного из методов. Что бы решить эту
--     -- проблему - параметр был явно указан в path.
--     :<|> "config" :> Get '[JSON] NaiveOpt
--     :<|> "options" :> Get '[JSON] [ RESTOption ]
--     :<|> "decisions" :> Get '[JSON] [Decision (CompilerDT String String String (TaggedTime String Int))]
--     :<|> QueryParam' '[Required] "did" Int :> Post '[JSON] (SNode, Six) -- manualStep

-- withStep st node six
--     =    getStep st node six
--     :<|> ( config <$> getStep st node six )
--     :<|> ( optionsWithMetrics <$> getStep st node six )
--     :<|> ( map option2decision . options compiler <$> getStep st node six )
--     :<|> ( \did -> liftSTM $ manualStep st node six did )



-- *Synthesis inspect

-- getSynthesis stm node = liftIO $ atomically $ getSynthesis' stm node
-- getSynthesis' stm node = do
--     syn <- M.lookup node stm
--     maybe (throwSTM err404) return syn


-- getStep st node six = liftSTM $ getStep' st node six
-- getStep' st node six = do
--     Synthesis{ sSteps } <- getSynthesis' st node
--     unless ( length sSteps > six ) $ throwSTM err409{ errBody="Step not exists." }
--     return $ reverse sSteps !! six


-- -- synthesisMustNotExist st node = do
-- --     syn <- M.lookup node st
-- --     when ( isJust syn ) $
-- --         throwSTM err409{ errBody="Synthesis already exist." }



-- -- *Synthesis generation

-- manualStep st node six dix = do
--     syn@Synthesis{ sSteps } <- getSynthesis' st node
--     case length sSteps `compare` six of
--         LT -> throwSTM err409{ errBody="Step with that index not exists." }
--         EQ -> do -- make step
--             let step = head sSteps
--             let d = ((!! dix) . map option2decision . options compiler) step
--             let step' = decision compiler step d
--             let syn' = syn{ sSteps=step' : sSteps }
--             M.insert syn' node st
--             return (node, six + 1)
--         GT -> do -- fork and manualStep
--             (childUid, _) <- forkSynthesis st node (six - 1)
--             manualStep st childUid six dix

-- genChildNode st SNode{ sid, six } childSix = inner (0 :: Int)
--     where
--         inner acc = do
--             let childNode = SNode{ sid=[qq|$sid[$six:$childSix].$acc|], six=childSix }
--             syn <- M.lookup childNode st
--             case syn of
--                 Just _  -> inner (acc + 1)
--                 Nothing -> return childNode


-- -- *Internal

liftSTM stm = do
    e <- liftIO (atomically $ catchSTM (Right <$> stm) (return . Left))
    either throwError return e

instance (Show t, Show v, Show title) => Show (CompilerStep title tag v x t) where
    show CompilerStep{ lastDecision } = "CompilerStep.lastDecision: " ++ show lastDecision
