
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

module NITTA.API where

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Default
import           Data.List
import           Data.Map                    (Map, fromList)
import           Data.Maybe
import           GHC.Generics
import           ListT
import           NITTA.Compiler
import           Servant
import qualified STMContainers.Map           as M


data Synthesis
  = Synthesis{ parent :: Maybe (String, Int) -- ^ (name, tick)
             , childs :: [String]
             , config :: NaiveOpt
             } deriving ( Generic )

instance ToJSON Synthesis
instance ToJSON NaiveOpt

instance Default Synthesis where
  def = Synthesis{ parent=Nothing
                 , childs=[]
                 , config=def
                 }


type SynthesisAPI
  =    "synthesis" :> Get '[JSON] (Map String Synthesis)
  :<|> "synthesis" :> Capture "sid" String :> Get '[JSON] Synthesis
  :<|> "synthesis" :> Capture "sid" String :> QueryParam "parent" String :> QueryParam "decision" Int :> PostNoContent '[JSON] ()
  :<|> "synthesis" :> Capture "sid" String :> "config" :> Get '[JSON] NaiveOpt


server state
  = fmap fromList ( liftIO $ atomically $ toList $ M.stream state )
  :<|> ( \sid -> do
    v <- liftIO $ atomically $ M.lookup sid state
    maybe (throwError err404) return v )
  :<|> postSynthesis state
  :<|> ( \sid -> do
    v <- liftIO $ atomically $ M.lookup sid state
    when ( isNothing v) $ throwError err404
    let Just Synthesis{..} = v
    return config )


postSynthesis _ _ Nothing _ = throwError err400{ errBody = "Parameter parent not defined." }
postSynthesis _ _ _ Nothing = throwError err400{ errBody = "Parameter parent not defined." }
postSynthesis state sid (Just pid) (Just did) = do
  s <- liftIO $ atomically $ M.lookup sid state
  when ( isJust s ) $ throwError err409{ errBody = "Synthesis already exist." }
  parent <- liftIO $ atomically $ M.lookup pid state
  when ( isNothing parent ) $ throwError err404{ errBody = "Parent not found." }
  liftIO $ atomically $ do
    let Just parent'@Synthesis{ childs=cs } = parent
    M.insert parent'{ childs=sid:cs } pid state
    M.insert def{ parent=Just (pid, did) } sid state



app = do
  state :: M.Map String Synthesis <- atomically $ do
    st <- M.new
    M.insert def "root" st
    return st
  return $ serve (Proxy :: Proxy SynthesisAPI) $ server state
