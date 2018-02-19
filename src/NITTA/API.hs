
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module NITTA.API where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Aeson.Parser
import           Data.Aeson.Types
import           Data.Default
import           Data.List
import           Data.Maybe
import           GHC.Generics
import           Servant


newtype Synthesis
  = Synthesis{ parent           :: Maybe String -- ^ Имя синтеза
             } deriving ( Generic )

instance ToJSON Synthesis

instance Default Synthesis where
  def = Synthesis{ parent=Nothing
                 }

type SynthesisAPI
  -- | Получить список всех присутствующих в системе синтезов.
  =    "synthesis" :> Get '[JSON] [Synthesis]
  -- | Получить информацию по конкретному синтезу.
  :<|> "synthesis" :> Capture "synthesisid" String :> Get '[JSON] Synthesis

a = Synthesis (Just "1")
b = Synthesis (Just "2")

server = return [a, b]
    :<|> \_ -> do
      withNamed
      return a

app = serveWithContext (Proxy :: Proxy SynthesisAPI) (Synthesis Nothing) server


--   :<|> synthesis :> Post (name, source)
--   :<|> Synthesis - один на весь цикл синтеза.
  -- :<|> Rollback = fork from one of a previous step.
