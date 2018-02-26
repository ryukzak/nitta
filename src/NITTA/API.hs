
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
import           Prelude                hiding (last)
import           Servant
import qualified STMContainers.Map      as M


data Synthesis
  = Synthesis{ parent :: Maybe (String, Int) -- ^ (name, tick)
             , childs :: [String]
             , config :: NaiveOpt
             , states :: [BranchedProcess String String String (TaggedTime String Int)]
             } deriving ( Generic )


instance ToJSON Synthesis
instance ( ToJSON tag , ToJSON t ) => ToJSON (TaggedTime tag t)


instance Default Synthesis where
  def = Synthesis{ parent=Nothing
                 , childs=[]
                 , config=def
                 , states=[]
                 }

type ST = BranchedProcess String String String (TaggedTime String Int)



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
    postSynthesis sid (Just pid) (Just did)
      = liftIO $ atomically $ postSynthesis' sid pid did

    postSynthesis' sid pid did = do
      s <- M.lookup sid state
      when ( isJust s ) $ throwSTM err409{ errBody = "Synthesis already exist." }
      parent <- M.lookup pid state
      when ( isNothing parent ) $ throwSTM err404{ errBody = "Parent not found." }
      let Just parent'@Synthesis{ childs=cs } = parent
      M.insert parent'{ childs=sid:cs } pid state
      M.insert def{ parent=Just (pid, did) } sid state



type StepsAPI = "steps" :>
     ( Get '[JSON] [ST]
  :<|> "last" :> Get '[JSON] ST
  :<|> Capture "step" Int :> Get '[JSON] ST
  :<|> Capture "step" Int :> Post '[JSON] ST
  :<|> Capture "step" Int :> "config" :> Get '[JSON] NaiveOpt
     )

stepsServer state sid
     = ( reverse . states <$> getSynthesis state sid )
  :<|> ( head . states <$> getSynthesis state sid )
  :<|> ( \step -> (!! step) . states <$> getSynthesis state sid )
  :<|> postStep
  :<|> ( \_step -> config <$> getSynthesis state sid )
  where
    postStep step = liftIO $ atomically $ postStep' step
    postStep' step = do
      s@Synthesis{..} <- getSynthesis' state sid
      when (length states > step) $ throwSTM err409{ errBody = "Steps already exist." }
      let states' = foldl (\(x:xs) _ -> naive config x : x : xs) states [length states .. step]
      M.insert s{ states=states' } sid state
      return $ head states'



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
