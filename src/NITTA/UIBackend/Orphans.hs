{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.UIBackend.Orphans
Description : Marshalling data for REST API
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

Marshaling data for JSON REST API.
-}
module NITTA.UIBackend.Orphans (
    VarValTimeJSON,
) where

import Data.Aeson
import qualified Data.String.Utils as S
import qualified Data.Text as T
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.Types
import NITTA.Synthesis.Binding
import NITTA.Synthesis.Dataflow
import NITTA.Synthesis.Refactor
import NITTA.Synthesis.Types
import Numeric.Interval
import Servant
import Servant.Docs

-- |Type class instance helper
type VarValTimeJSON v x t = (Var v, Val x, Time t, ToJSONKey v, ToJSON v, ToJSON x, ToJSON t)

-- *Problems

instance (ToJSON v, ToJSON tp) => ToJSON (EndpointSt v tp)

instance (ToJSON v) => ToJSON (EndpointRole v)

instance (Time t) => ToJSON (Interval t) where
    toJSON = String . T.pack . S.replace (show (maxBound :: t)) "∞" . show

-- *Model

instance (ToJSON v) => ToJSON (Lock v)

-- *Synthesis

instance ToJSON SID where
    toJSON sid = toJSON $ show sid

instance FromJSON SID where
    parseJSON v = read <$> parseJSON v

instance FromHttpApiData SID where
    parseUrlPiece = Right . read . T.unpack

instance ToSample SID where
    toSamples _ = [("The synthesis node path from the root by edge indexes.", SID [1, 1, 3])]

instance ToJSON BindMetrics
instance ToJSON RefactorMetrics
instance ToJSON RefactorType
instance ToJSON DataflowMetrics

-- *Values

instance ToJSONKey (IntX w) where
    toJSONKey =
        let ToJSONKeyText f g = toJSONKey
         in ToJSONKeyText (\(IntX x) -> f x) (\(IntX x) -> g x)

instance ToJSON (IntX w) where
    toJSON (IntX x) = toJSON x

instance ToJSONKey (FX m b) where
    toJSONKey =
        let ToJSONKeyText f g = toJSONKey
         in ToJSONKeyText (\(FX x) -> f $ show x) (\(FX x) -> g $ show x)

instance ToJSON (FX m b) where
    toJSON (FX x) = toJSON $ show x

instance (ToJSON x) => ToJSON (Attr x) where
    toJSON Attr{value} = toJSON value
