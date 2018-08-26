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
    , SRoot(..)  -- FIXME: Remove?
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
import Data.Hashable
import           Servant
import qualified STMContainers.Map             as M
import Web.HttpApiData
import           Text.InterpolatedString.Perl6 (qq)


type T = TaggedTime String Int
type CSt = CompilerStep String String String Int (TaggedTime String Int)
type SYN = Synthesis String String String Int (TaggedTime String Int)

type Sid = String -- ^synthesis ID
type Six = Int    -- ^step index

-- |Synthesis ID.
data SRoot = SRoot
    { sid :: Sid -- ^synthesis id
    , six :: Six -- ^synthesis step index from which forked
    } deriving ( Generic, Show, Ord, Eq )

instance Hashable SRoot where
    hashWithSalt salt SRoot{ sid, six }
        = hashWithSalt salt (sid, six)
instance ToJSON SRoot
instance ToJSONKey SRoot
-- instance ToJSON SRoot where
--     toJSON SRoot{ sid, six } = object
--         [ "sid" .= sid
--         , "six" .= six
--         ]


-- instance FromHttpApiData SRoot where
--     parseUrlPiece text
--         = case parseUrlPiece text of 
--             Right (sid, six) -> Right SRoot{ sid, six }
--             Left text' -> Left text'

instance FromJSON SRoot where
    parseJSON = withObject "SRoot" $ \v -> SRoot
        <$> v .: "sid"
        <*> v .: "six"


-- |Projection of 'Synthesis' for REST API.
data SynthesisView
    = SynthesisView
        { siParent :: Maybe SRoot -- ^ (name, tick)
        , siChilds :: [SRoot] -- ^ [(sId, stepId)]
        , siSteps  :: [Int]
        }
    deriving ( Generic )

instance ToJSON SynthesisView


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
        { sParent :: Maybe SRoot
        , sChilds :: [SRoot]
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
    =    "synthesis" :> Get '[JSON] (Map SRoot SynthesisView)
    :<|> "synthesis" :> Capture "rsid" Sid :> Capture "rsix" Six :> WithSynthesis

synthesisServer st
    =    allSynthesis st
    :<|> \sid six -> withSynthesis st SRoot{ sid, six }

allSynthesis st = fromList . map (\(k, v) -> (k, view v)) <$> ( liftSTM $ toList $ M.stream st )
    where
        view Synthesis{ sParent, sChilds, sSteps } 
            = SynthesisView
                { siParent=sParent 
                , siChilds=sChilds
                , siSteps=map fst $ zip [0..] sSteps
                }

type WithSynthesis
    =    Get '[JSON] SYN
    :<|> QueryParam' '[Required] "childSix" Six :> Post '[JSON] SRoot
    :<|> StepAPI

withSynthesis st sRoot
    =    getSynthesis st sRoot
    :<|> ( \childSix -> liftSTM $ forkSynthesis st sRoot childSix )
    :<|> stepServer st sRoot



type StepAPI
    =    "sSteps" :> Get '[JSON] [CSt]
    :<|> "sSteps" :> QueryParam' '[Required] "oneStep" Bool :> Post '[JSON] (SRoot, Six) -- compilerStep
    :<|> "sSteps" :> Capture "six" Int :> WithStep

stepServer st sRoot
    =    ( sSteps <$> getSynthesis st sRoot )
    :<|> ( \oneStep -> liftSTM $ compilerStep st sRoot oneStep )
    :<|> \six -> withStep st sRoot six



type WithStep
    =    Get '[JSON] CSt
--     -- Дублирование auto в path - костыль. Проблема в следующем - параметры
--     -- и флаги не влияют на имя функции в автоматически генерируемом API
--     -- для JS, что приводит к утере одного из методов. Что бы решить эту
--     -- проблему - параметр был явно указан в path.
--     :<|> QueryParam' '[Required] "manual" Int :> Post '[JSON] SRoot -- manualStep
--     :<|> "config" :> Get '[JSON] NaiveOpt
--     :<|> "decisions" :> Get '[JSON] [Decision (CompilerDT String String String T)]
--     :<|> "options" :> Get '[JSON] [ RESTOption ]

withStep st sRoot six
    =    getStep st sRoot six
--     :<|> ( \did -> liftSTM $ manualStep st sRoot six did )
--     :<|> ( config <$> getStep st sRoot six )
--     :<|> ( map option2decision . options compiler <$> getStep st sRoot six )
--     :<|> ( optionsWithMetrics <$> getStep st sRoot six )



-- *Synthesis inspect

getSynthesis stm suid = liftIO $ atomically $ getSynthesis' stm suid
getSynthesis' stm suid = do
    syn <- M.lookup suid stm
    maybe (throwSTM err404) return syn


getStep st sRoot six = liftSTM $ getStep' st sRoot six
getStep' st sRoot six = do
    Synthesis{ sSteps } <- getSynthesis' st sRoot
    unless ( length sSteps > six ) $ throwSTM err409{ errBody="Step not exists." }
    return $ reverse sSteps !! six


synthesisMustNotExist st uid = do
    syn <- M.lookup uid st
    when ( isJust syn ) $
        throwSTM err409{ errBody="Synthesis already exist." }



-- *Synthesis generation

forkSynthesis st parentUid@SRoot{ sid, six } childSix = do
    childUid <- genChildUid st parentUid childSix
    parent@Synthesis{ sChilds, sSteps } <- getSynthesis' st parentUid
    M.insert parent{ sChilds=childUid : sChilds } parentUid st
    M.insert
        Synthesis
            { sParent=Just parentUid
            , sChilds=[]
            , sSteps=drop (length sSteps - childSix - 1) sSteps
            }
        childUid
        st
    return $ trace (show childUid) childUid

genChildUid st root@SRoot{ sid, six } childSix = inner (0 :: Int)
    where
        inner acc = do
            let child = SRoot{ sid=[qq|$sid[$six:$childSix].$acc|], six=childSix }
            syn <- M.lookup child st
            case syn of
                Just _ -> inner (acc + 1)
                Nothing -> return child
    
compilerStep st sRoot oneStep = do
    s@Synthesis{ sSteps } <- getSynthesis' st sRoot
    sSteps' <- case oneStep of
        True
            | Just step <- naive' (head sSteps)
            -> return (step : sSteps)
        False -> return $ mkAllSteps sSteps
        _ -> throwSTM err409{ errBody="Synthesis is over." }
    M.insert s{ sSteps=sSteps' } sRoot st
    return (sRoot, length sSteps' - 1)
    where
        mkAllSteps steps@(s:_)
            | Just s' <- naive' s = mkAllSteps (s':steps)
            | otherwise = steps
        mkAllSteps _ = error "Empty CompilerState."


-- manualStep st sid six dix = do
--     syn@Synthesis{ sSteps } <- getSynthesis' st sid
--     let a = length sSteps
--     let b = six
--     let c = a `compare` b
--     case trace [qq|$a $c $b|] c of
--         LT -> throwSTM err409{ errBody="Step with that index not exists." }
--         EQ -> do -- make step
--             let step = head sSteps
--             let d = ((!! dix) . map option2decision . options compiler) step
--             let step' = decision compiler step d
--             let syn' = syn{ sSteps=step' : sSteps }
--             M.insert syn' sid st
--             return (sid, six + 1)
--         GT -> do -- fork and make step
--             let six' = six - 1
--             let sid' = [qq|fork $sid[$six'] decision: $dix|]
--             forkSynthesis st sid' sid six'
--             manualStep st sid' six dix


-- *Internal

liftSTM stm = do
    e <- liftIO (atomically $ catchSTM (Right <$> stm) (return . Left))
    either throwError return $ trace (">" ++ show e) e

instance (Show t, Show v, Show title) => Show (CompilerStep title tag v x t) where
    show CompilerStep{ lastDecision } = "CompilerStep.lastDecision: " ++ show lastDecision
