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
    , SynthesisAPI
    , synthesisServer
    ) where

import           Control.Concurrent.STM
import           Control.Monad                 (when)
import           Control.Monad.Except
import           Data.Aeson
import           Data.Default
import           Data.Map                      (Map, fromList)
import           Data.Maybe
import           Debug.Trace
import           GHC.Generics
import           ListT                         (toList)
import           NITTA.Compiler
import           NITTA.DataFlow
import           NITTA.Types
import           NITTA.Utils.JSON              ()
import           Servant
import qualified STMContainers.Map             as M
import           Text.InterpolatedString.Perl6 (qq)


type T = TaggedTime String Int
type CSt = CompilerStep String String String Int (TaggedTime String Int)
type SYN = Synthesis String String String Int (TaggedTime String Int)


-- |SynthesisUid with two field:
--
-- - synthesis id (sid);
-- - synthesis step index from which forked (six).
type SynthesisUid = (String, Int)

type RESTOption =
    ( Int
    , GlobalMetrics
    , SpecialMetrics
    , Option (CompilerDT String String String T)
    , Decision (CompilerDT String String String T)
    )

-- |One of many synthesis states. All synthesis is a sequence of compiler steps. Synthesis can be
-- forked from another. The root synthesis is defined externally.
data Synthesis title tag v x t
    = Synthesis
        { sParent :: Maybe SynthesisUid -- ^ (name, tick)
        , sChilds :: [SynthesisUid] -- ^ [(sId, stepId)]
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

synthesis initialStep = Synthesis
    { sParent=Nothing
    , sChilds=[]
    , sSteps=[initialStep]
    }




type SynthesisAPI
    =    "synthesis" :> Get '[JSON] (Map String SYN)
    :<|> "synthesis" :> Capture "sid" String :> WithSynthesis

synthesisServer st
    =    allSynthesis st
    :<|> \sid -> withSynthesis st sid

allSynthesis st = fmap fromList ( liftSTM $ toList $ M.stream st )



type WithSynthesis
    =    Get '[JSON] SYN
    :<|> QueryParam' '[Required] "parentSid" String :>
            QueryParam' '[Required] "six" Int :>
            PostNoContent '[JSON] ()
    :<|> StepAPI

withSynthesis st sid
    =    getSynthesis st sid
    :<|> ( \ parentSid six -> liftSTM $ forkSynthesis st sid parentSid six )
    :<|> stepServer st sid



type StepAPI
    =    "sSteps" :> Get '[JSON] [CSt]
    :<|> "sSteps" :> QueryParam' '[Required] "oneStep" Bool :> Post '[JSON] SynthesisUid -- compilerStep
    :<|> "sSteps" :> Capture "six" Int :> WithStep

stepServer st sid
    =    ( sSteps <$> getSynthesis st sid )
    :<|> ( \oneStep -> liftSTM $ compilerStep st sid oneStep )
    :<|> \six -> withStep st sid six



type WithStep
    =    Get '[JSON] CSt
    -- Дублирование auto в path - костыль. Проблема в следующем - параметры
    -- и флаги не влияют на имя функции в автоматически генерируемом API
    -- для JS, что приводит к утере одного из методов. Что бы решить эту
    -- проблему - параметр был явно указан в path.
    :<|> QueryParam' '[Required] "manual" Int :> Post '[JSON] SynthesisUid -- manualStep
    :<|> "config" :> Get '[JSON] NaiveOpt
    :<|> "decisions" :> Get '[JSON] [Decision (CompilerDT String String String T)]
    :<|> "options" :> Get '[JSON] [ RESTOption ]

withStep st sid six
    =    getStep st sid six
    :<|> ( \did -> liftSTM $ manualStep st sid six did )
    :<|> ( config <$> getStep st sid six )
    :<|> ( map option2decision . options compiler <$> getStep st sid six )
    :<|> ( optionsWithMetrics <$> getStep st sid six )



-- *Synthesis inspect

getSynthesis stm sId = liftIO $ atomically $ getSynthesis' stm sId
getSynthesis' stm sId = do
    syn <- M.lookup sId stm
    maybe (throwSTM err404) return syn


getStep st sid six = liftSTM $ getStep' st sid six
getStep' st sid stepId = do
    Synthesis{ sSteps } <- getSynthesis' st sid
    unless ( length sSteps > stepId ) $ throwSTM err409{ errBody="Step not exists." }
    return $ reverse sSteps !! stepId


synthesisMustNotExist st sid = do
    syn <- M.lookup sid st
    when ( isJust syn ) $
        throwSTM err409{ errBody="Synthesis already exist." }



-- *Synthesis generation

forkSynthesis st sid parentSid six = do
    synthesisMustNotExist st sid
    parent@Synthesis{ sChilds, sSteps } <- getSynthesis' st parentSid
    M.insert parent{ sChilds=(sid, six) : sChilds } parentSid st
    M.insert
        Synthesis
            { sParent=Just ( parentSid, six )
            , sChilds=[]
            , sSteps=drop (length sSteps - six - 1) sSteps
            }
        sid
        st

compilerStep st sid oneStep = do
    s@Synthesis{ sSteps } <- getSynthesis' st sid
    sSteps' <- case oneStep of
        True
            | Just step <- naive' (head sSteps)
            -> return (step : sSteps)
        False -> return $ mkAllSteps sSteps
        _ -> throwSTM err409{ errBody="Synthesis is over." }
    M.insert s{ sSteps=sSteps' } sid st
    return (sid, length sSteps' - 1)
    where
        mkAllSteps steps@(s:_)
            | Just s' <- naive' s = mkAllSteps (s':steps)
            | otherwise = steps
        mkAllSteps _ = error "Empty CompilerState."


manualStep st sid six dix = do
    syn@Synthesis{ sSteps } <- getSynthesis' st sid
    let a = length sSteps
    let b = six
    let c = a `compare` b
    case trace [qq|$a $c $b|] c of
        LT -> throwSTM err409{ errBody="Step with that index not exists." }
        EQ -> do -- make step
            let step = head sSteps
            let d = ((!! dix) . map option2decision . options compiler) step
            let step' = decision compiler step d
            let syn' = syn{ sSteps=step' : sSteps }
            M.insert syn' sid st
            return (sid, six + 1)
        GT -> do -- fork and make step
            let six' = six - 1
            let sid' = [qq|fork $sid[$six'] decision: $dix|]
            forkSynthesis st sid' sid six'
            manualStep st sid' six dix


-- *Internal

liftSTM stm = do
    e <- liftIO (atomically $ catchSTM (Right <$> stm) (return . Left))
    either throwError return $ trace (">" ++ show e) e

instance (Show t, Show v, Show title) => Show (CompilerStep title tag v x t) where
    show CompilerStep{ lastDecision } = "CompilerStep.lastDecision: " ++ show lastDecision
