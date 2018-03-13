{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- TODO: https://tools.ietf.org/id/draft-kelly-json-hal-03.txt
module NITTA.API where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson
import           Data.Default
import           Data.Map               (Map, fromList)
import           Data.Maybe
import           GHC.Generics
import           ListT                  (toList)
import           NITTA.Compiler
import           NITTA.Flows
import           NITTA.Types            hiding (Synthesis)
import           NITTA.Utils.JSON       ()
import           Servant
import qualified STMContainers.Map      as M


data Synthesis
  = Synthesis{ parent :: Maybe (String, Int) -- ^ (name, tick)
             , childs :: [String] -- TODO: привязка к шагу процесса синтеза, а не к названию синтеза.
             , states :: [ST]
             } deriving ( Generic )


instance ToJSON Synthesis


instance Default Synthesis where
  def = Synthesis{ parent=Nothing
                 , childs=[]
                 , states=[]
                 }

type T = TaggedTime String Int
type ST = CompilerStep String String String (TaggedTime String Int)
instance ToJSON ST



type SynthesisAPI = "synthesis" :>
     ( Get '[JSON] (Map String Synthesis)
  :<|> Capture "sid" String :> Get '[JSON] Synthesis
  -- TODO: servant 0.13, use Strict Param
  :<|> Capture "sid" String :> QueryParam "parent" String :> QueryParam "decision" Int :> PostNoContent '[JSON] ()
  :<|> Capture "sid" String :> StepsAPI
     )

synthesisServer state
     = fmap fromList ( liftIO $ atomically $ toList $ M.stream state )
  :<|> getSynthesis state
  :<|> postSynthesis
  :<|> stepsServer state
  where
    postSynthesis _ Nothing _ = throwError err400{ errBody = "Parameter `parent` not defined." }
    postSynthesis _ _ Nothing = throwError err400{ errBody = "Parameter `decision` not defined." }
    postSynthesis sid (Just pid) (Just did) = liftSTM $ postSynthesis' sid pid did

    postSynthesis' sid pid did = do
      s <- M.lookup sid state
      when ( isJust s ) $ throwSTM err409{ errBody = "Synthesis already exist." }
      parent <- M.lookup pid state
      when ( isNothing parent ) $ throwSTM err404{ errBody = "Parent not found." }
      let Just parent'@Synthesis{ childs=cs } = parent
      M.insert parent'{ childs=sid:cs } pid state
      M.insert parent'{ parent=Just (pid, did)
                      -- TODO: crop states by did
                      } sid state



type StepsAPI = "steps" :>
     ( Get '[JSON] [ST]
     -- step - необходимо обеспечить сохранность индексов для разных веток синтеза.
  :<|> Capture "step" Int :> Get '[JSON] ST
  :<|> Capture "step" Int :> QueryParam "manual" Int :> Post '[JSON] ST
  :<|> Capture "step" Int :> "config" :> Get '[JSON] NaiveOpt
  -- :<|> Capture "step" Int :> "options0" :> Get '[JSON] [Option (CompilerDT String String String T)]
  -- :<|> Capture "step" Int :> "options" :> Capture "oid" Int :> "metrics" :> Get '[JSON] [Option (CompilerDT String String String T)]
  -- :<|> Capture "step" Int :> "options" :> QueryParam "sort" Int :> Get '[JSON] [Option (CompilerDT String String String T)]
  :<|> Capture "step" Int :> "decisions" :> Get '[JSON] [Decision (CompilerDT String String String T)]
  :<|> Capture "step" Int :> "options" :> Get '[JSON] [ ( Int
                                                          , GlobalMetrics
                                                          , SpecialMetrics
                                                          , Option (CompilerDT String String String T)
                                                          , Decision (CompilerDT String String String T)
                                                          )
                                                        ]
     )

stepsServer state sid
     = ( reverse . states <$> getSynthesis state sid )
  :<|> getStep
  :<|> postStep
  :<|> ( fmap config . getStep )
  -- :<|> ( fmap (options compiler) . getStep )
  :<|> ( fmap (map option2decision . options compiler) <$> getStep )
  :<|> ( fmap optionsWithMetrics <$> getStep )
  where
    getStep = liftSTM . getStep'
    getStep' step = do
      steps <- states <$> getSynthesis' state sid
      unless (length steps > step) $ throwSTM err409{ errBody="Step not exists." }
      return $ steps !! step
    postStep step Nothing     = liftSTM $ autoPostStep step
    postStep step (Just (-1)) = liftSTM $ autoPostStep step
    postStep step (Just d)    = liftSTM $ manualPostStep step d
    autoPostStep step = do
      s@Synthesis{..} <- getSynthesis' state sid
      unless (length states <= step) $ throwSTM err409{ errBody="Steps already exist." }
      let states' = foldl (\(x:xs) _ -> mkStep x : x : xs) states [length states .. step]
      M.insert s{ states=states' } sid state
      return $ head states'
    manualPostStep step d = do
      s@Synthesis{ states=states@(st:_) } <- getSynthesis' state sid
      unless (length states == step) $ throwSTM err409{ errBody="Only one manual step at a time." }
      let d' = ((!! d) . map option2decision . options compiler) st
      let st' = decision compiler st d'
      let s' = s{ states=st' : states }
      M.insert s' sid state
      return st'
    mkStep st = fromMaybe (error "Synthesis is over.") $ naive' st


liftSTM stm = liftIO (atomically $ catchSTM (Right <$> stm) (return . Left)) >>= either throwError return

getSynthesis state sid = liftIO $ atomically $ getSynthesis' state sid
getSynthesis' state sid = do
  v <- M.lookup sid state
  maybe (throwSTM err404) return v


getDecision state sid did = do
  s <- getSynthesis state sid
  return $ states s !! did


app root = do
  state <- atomically $ do
    st <- M.new
    M.insert def{ states=[root] } "root" st
    return st
  return $ serve (Proxy :: Proxy SynthesisAPI) $ synthesisServer state
