{- |
Module      : NITTA.UIBackend.Types
Description : Types for UI backend.
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.UIBackend.Types (
    BackendCtx (..),
) where

import Data.Default
import NITTA.Synthesis.MlBackend.ServerInstance
import NITTA.Synthesis.Types

data BackendCtx tag v x t = BackendCtx
    { -- | root synthesis node
      root :: DefTree tag v x t
    , -- | lists of received by IO values
      receivedValues :: [(v, [x])]
    , outputPath :: String
    , mlBackendGetter :: IO MlBackendServer
    , mlScoringModel :: Maybe String
    }

instance Default (BackendCtx tag v x t) where
    def = BackendCtx (error "root of a default (missing) BackendCtx was accessed") def def def def
