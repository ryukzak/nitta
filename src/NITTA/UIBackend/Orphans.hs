{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import qualified Data.Set as S
import qualified Data.String.Utils as S
import qualified Data.Text as T
import NITTA.Intermediate.DataFlow
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Model.Types
import NITTA.Synthesis.Estimate
import NITTA.Synthesis.Tree
import Numeric.Interval
import Servant
import Servant.Docs

-- |Type class instance helper
type VarValTimeJSON v x t = (Var v, Val x, Time t, ToJSONKey v, ToJSON v, ToJSON x, ToJSON t)

-- *Problems

instance (VarValTimeJSON v x t) => ToJSON (SynthesisStatement String v x (TimeConstrain t))
instance (VarValTimeJSON v x t) => ToJSON (SynthesisStatement String v x (Interval t))

instance (ToJSON v, Show v, Show x) => ToJSON (Refactor v x) where
    toJSON = toJSON . show

instance (ToJSON v, ToJSON tp) => ToJSON (EndpointSt v tp)

instance {-# OVERLAPS #-} (Time t) => ToJSON (EndpointSt String (TimeConstrain t)) where
    toJSON EndpointSt{epRole = Source vs, epAt} = toJSON ("Source: " ++ S.join ", " (S.elems vs) ++ " at " ++ show epAt)
    toJSON EndpointSt{epRole = Target v, epAt} = toJSON ("Target: " ++ v ++ " at " ++ show epAt)

instance (ToJSON v) => ToJSON (EndpointRole v)

instance (ToJSON t, Time t) => ToJSON (TimeConstrain t) where
    toJSON TimeConstrain{..} =
        object
            [ "available" .= tcAvailable
            , "duration" .= tcDuration
            ]

instance (Show a, Bounded a) => ToJSON (Interval a) where
    toJSON = String . T.pack . S.replace (show (maxBound :: a)) "∞" . show

-- *Model

instance (Show v) => ToJSON (F v x) where
    toJSON = String . T.pack . show

instance (Var v, ToJSON v, ToJSON x) => ToJSON (DataFlowGraph v x)

instance (ToJSON v) => ToJSON (Lock v)

instance ToJSON Relation where
    toJSON (Vertical a b) = toJSON [a, b]

-- *Synthesis

instance ToJSON NId where
    toJSON nId = toJSON $ show nId

instance FromJSON NId where
    parseJSON v = read <$> parseJSON v

instance FromHttpApiData NId where
    parseUrlPiece = Right . read . T.unpack

instance ToSample NId where
    toSamples _ = [("The synthesis node path from the root by edge indexes.", NId [1, 1, 3])]

instance ToJSON ObjectiveFunctionConf
instance ToJSON Parameters

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
