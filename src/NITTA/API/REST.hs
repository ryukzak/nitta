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
    , synthesis
    , rootNode
    , SynthesisAPI
    , synthesisServer
    ) where

import           Control.Concurrent.STM
import           Control.Monad.Except
import           Data.Aeson
import           Data.Default
import           Data.Hashable
import           Data.Map                      (Map, fromList)
import           Debug.Trace
import           GHC.Generics
import           ListT                         (toList)
import           NITTA.Compiler
import           NITTA.DataFlow                ()
import           NITTA.Types
import           NITTA.Utils.JSON              ()
import           Servant
import qualified STMContainers.Map             as M
import           Text.InterpolatedString.Perl6 (qq)



type CSt = CompilerStep String String String Int (TaggedTime String Int)
type SYN = Synthesis String String String Int (TaggedTime String Int)



type Sid = String -- ^synthesis ID
type Six = Int    -- ^step index

-- |Synthesis node.
data SNode = SNode
    { sid :: Sid -- ^Synthesis id.
    , six :: Six -- ^Synthesis step index from which forked.
    } deriving ( Generic, Show, Ord, Eq )

instance Hashable SNode where
    hashWithSalt salt SNode{ sid, six } = hashWithSalt salt (sid, six)

instance ToJSON SNode
instance ToJSONKey SNode
instance FromJSON SNode

rootNode = SNode { sid="root", six=0 }

-- |One of many synthesis states. All synthesis is a sequence of compiler steps. Synthesis can be
-- forked from another. The root synthesis is defined externally.
data Synthesis title tag v x t
    = Synthesis
        { sParent :: Maybe SNode
        , sChilds :: [SNode]
        , sSteps  :: [CompilerStep title tag v x t]
        }
    deriving ( Generic, Show )

instance ToJSON SYN

instance Default (Synthesis title tag v x t) where
    def = Synthesis
        { sParent=Nothing
        , sChilds=[]
        , sSteps=[]
        }

-- | Create initial synthesis.
synthesis initialStep = Synthesis
    { sParent=Nothing
    , sChilds=[]
    , sSteps=[initialStep]
    }



-- *REST API Projections.

-- |Projection of 'Synthesis' for REST API.
data SynthesisView
    = SynthesisView
        { siParent :: Maybe SNode
        , siChilds :: [SNode]
        , siSteps  :: [Int]
        }
    deriving ( Generic )

instance ToJSON SynthesisView

type RESTOption =
    ( Int
    , GlobalMetrics
    , SpecialMetrics
    , Option (CompilerDT String String String (TaggedTime String Int))
    , Decision (CompilerDT String String String (TaggedTime String Int))
    )



-- *REST API

type SynthesisAPI
    =    "synthesis" :> Get '[JSON] (Map SNode SynthesisView)
    :<|> "synthesis" :> Capture "rsid" Sid :> Capture "rsix" Six :> WithSynthesis

synthesisServer st
    =    allSynthesis st
    :<|> \sid six -> withSynthesis st SNode{ sid, six }

allSynthesis st = fromList . map (\(k, v) -> (k, view v)) <$> liftSTM ( toList $ M.stream st )
    where
        view Synthesis{ sParent, sChilds, sSteps }
            = SynthesisView
                { siParent=sParent
                , siChilds=sChilds
                , siSteps=zipWith (curry fst) [0 ..] sSteps
                }



type WithSynthesis
    =    Get '[JSON] SYN
    :<|> QueryParam' '[Required] "childSix" Six :> Post '[JSON] SNode
    :<|> StepAPI

withSynthesis st node
    =    getSynthesis st node
    :<|> ( \childSix -> liftSTM $ forkSynthesis st node childSix )
    :<|> stepServer st node



type StepAPI
    =    "sSteps" :> Get '[JSON] [CSt]
    :<|> "sSteps" :> QueryParam' '[Required] "oneStep" Bool :> Post '[JSON] (SNode, Six) -- compilerStep
    :<|> "sSteps" :> Capture "six" Int :> WithStep

stepServer st node
    =    ( sSteps <$> getSynthesis st node )
    :<|> ( \oneStep -> liftSTM $ compilerStep st node oneStep )
    :<|> \six -> withStep st node six



type WithStep
    =    Get '[JSON] CSt
    -- Дублирование auto в path - костыль. Проблема в следующем - параметры
    -- и флаги не влияют на имя функции в автоматически генерируемом API
    -- для JS, что приводит к утере одного из методов. Что бы решить эту
    -- проблему - параметр был явно указан в path.
    :<|> "config" :> Get '[JSON] NaiveOpt
    :<|> "options" :> Get '[JSON] [ RESTOption ]
    :<|> "decisions" :> Get '[JSON] [Decision (CompilerDT String String String (TaggedTime String Int))]
    :<|> QueryParam' '[Required] "did" Int :> Post '[JSON] (SNode, Six) -- manualStep

withStep st node six
    =    getStep st node six
    :<|> ( config <$> getStep st node six )
    :<|> ( optionsWithMetrics <$> getStep st node six )
    :<|> ( map option2decision . options compiler <$> getStep st node six )
    :<|> ( \did -> liftSTM $ manualStep st node six did )



-- *Synthesis inspect

getSynthesis stm node = liftIO $ atomically $ getSynthesis' stm node
getSynthesis' stm node = do
    syn <- M.lookup node stm
    maybe (throwSTM err404) return syn


getStep st node six = liftSTM $ getStep' st node six
getStep' st node six = do
    Synthesis{ sSteps } <- getSynthesis' st node
    unless ( length sSteps > six ) $ throwSTM err409{ errBody="Step not exists." }
    return $ reverse sSteps !! six


-- synthesisMustNotExist st node = do
--     syn <- M.lookup node st
--     when ( isJust syn ) $
--         throwSTM err409{ errBody="Synthesis already exist." }



-- *Synthesis generation

forkSynthesis st parentNode childSix = do
    childNode <- genChildNode st parentNode childSix
    parent@Synthesis{ sChilds, sSteps } <- getSynthesis' st parentNode
    M.insert parent{ sChilds=childNode : sChilds } parentNode st
    M.insert
        Synthesis
            { sParent=Just parentNode
            , sChilds=[]
            , sSteps=drop (length sSteps - childSix - 1) sSteps
            }
            childNode
        st
    return childNode

compilerStep st node oneStep = do
    syn@Synthesis{ sSteps } <- getSynthesis' st node
    sSteps' <- case oneStep of
        True
            | Just step <- naive' (head sSteps)
            -> return (step : sSteps)
        False -> return $ mkAllSteps sSteps
        _ -> throwSTM err409{ errBody="Synthesis is over." }
    M.insert syn{ sSteps=sSteps' } node st
    return (node, length sSteps' - 1)
    where
        mkAllSteps steps@(s:_)
            | Just s' <- naive' s = mkAllSteps (s':steps)
            | otherwise = steps
        mkAllSteps _ = error "Empty CompilerState."

manualStep st node six dix = do
    syn@Synthesis{ sSteps } <- getSynthesis' st node
    case length sSteps `compare` six of
        LT -> throwSTM err409{ errBody="Step with that index not exists." }
        EQ -> do -- make step
            let step = head sSteps
            let d = ((!! dix) . map option2decision . options compiler) step
            let step' = decision compiler step d
            let syn' = syn{ sSteps=step' : sSteps }
            M.insert syn' node st
            return (node, six + 1)
        GT -> do -- fork and manualStep
            childUid <- forkSynthesis st node (six - 1)
            manualStep st childUid six dix

genChildNode st SNode{ sid, six } childSix = inner (0 :: Int)
    where
        inner acc = do
            let childNode = SNode{ sid=[qq|$sid[$six:$childSix].$acc|], six=childSix }
            syn <- M.lookup childNode st
            case syn of
                Just _  -> inner (acc + 1)
                Nothing -> return childNode


-- *Internal

liftSTM stm = do
    e <- liftIO (atomically $ catchSTM (Right <$> stm) (return . Left))
    either throwError return e

instance (Show t, Show v, Show title) => Show (CompilerStep title tag v x t) where
    show CompilerStep{ lastDecision } = "CompilerStep.lastDecision: " ++ show lastDecision
