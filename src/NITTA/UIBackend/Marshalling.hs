{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.UIBackend.Marshalling
Description : Marshalling data for REST API
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

Marshaling data for JSON REST API.
-}
module NITTA.UIBackend.Marshalling () where

import           Data.Aeson
import qualified Data.Map                         as M
import qualified Data.Set                         as S
import qualified Data.String.Utils                as S
import qualified Data.Text                        as T
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.Problems.Refactor
import           NITTA.Model.Problems.Whole
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.TargetSystem
import           NITTA.Model.Types
import           NITTA.Project.Parts.TestBench
import           NITTA.Synthesis.Types
import           NITTA.UIBackend.Timeline
import           NITTA.Utils                      (transferred)
import           Numeric.Interval
import           Servant


type VarValTimeJSON v x t = ( Var v, Val x, Time t, ToJSONKey v, ToJSON v, ToJSON x, ToJSON t )


-- *Option/Decision
instance ( VarValTimeJSON v x t
        ) => ToJSON (Option (SynthesisDT (BusNetwork String v x t)))
instance ( VarValTimeJSON v x t
         ) => ToJSON (Decision (SynthesisDT (BusNetwork String v x t)))
instance ( ToJSON v, Show v, Show x ) => ToJSON (RefactorOption v x) where
    toJSON = toJSON . show
instance ( ToJSON v, Show v, Show x ) => ToJSON (RefactorDecision v x) where
    toJSON = toJSON . show
instance ( Time t ) => ToJSON (Option (EndpointDT String t)) where
    toJSON EndpointO{ epoRole=Source vs, epoAt } = toJSON ("Source: " ++ S.join ", " (S.elems vs) ++ " at " ++ show epoAt)
    toJSON EndpointO{ epoRole=Target v, epoAt } = toJSON ("Target: " ++ v ++ " at " ++ show epoAt)



-- *Process units
instance ( VarValTimeJSON v x t
        ) => ToJSON (BusNetwork String v x t) where
    toJSON n@BusNetwork{..} = object
        [ "width"              .= bnSignalBusWidth
        , "remain"             .= bnRemains
        , "forwardedVariables" .= map (String . T.pack . show) (S.elems $ transferred n)
        , "binds"              .= bnBinded
        , "processLength"      .= nextTick (process n)
        , "processUnits"       .= M.keys bnPus
        , "process"            .= process n
        ]



-- *Model
instance ( Var v, ToJSON v, ToJSON x ) => ToJSON (DataFlowGraph v x)

instance ToJSON Relation where
    toJSON (Vertical a b) = toJSON [ a, b ]

instance ( VarValTimeJSON v x t
        ) => ToJSON (ModelState (BusNetwork String v x t) v x)

instance ( VarValTimeJSON v x t
         ) => ToJSON (Process v x t) where
    toJSON Process{ steps, nextTick, relations } = object
        [ "steps"     .= steps
        , "nextTick"  .= nextTick
        , "relations" .= relations
        ]

instance ( VarValTimeJSON v x t
         ) => ToJSON (Step v x t) where
    toJSON Step{ sKey, sTime, sDesc } = object
        [ "sKey"   .= sKey
        , "sDesc"  .= show sDesc
        , "sTime"  .= sTime
        , "sLevel" .= levelName sDesc
        , "sPU"    .= showPU sDesc
        ]

levelName CADStep{}           = "CAD" :: String
levelName FStep{}             = "Function"
levelName EndpointRoleStep{}  = "Endpoint"
levelName InstructionStep{}   = "Instruction"
levelName (NestedStep _ step) = levelName $ sDesc step

instance ToJSON ViewPointID
instance ( Time t, ToJSON t ) => ToJSON ( TimelinePoint t )
instance ( Time t, ToJSON t ) => ToJSON ( TimelineWithViewPoint t )
instance ( Time t, ToJSON t ) => ToJSON ( ProcessTimelines t )

-- *Synthesis
instance ToJSON NId where
    toJSON nId = toJSON $ show nId

instance FromJSON NId where
    parseJSON v = read <$> parseJSON v

instance FromHttpApiData NId where
    parseUrlPiece = Right . read . T.unpack


instance ( VarValTimeJSON v x t
        ) => ToJSON (SG Node String v x t) where
    toJSON Node{ nId, nModel, nIsComplete } = object
        [ "nModel"      .= nModel
        , "nIsComplete" .= nIsComplete
        , "nId"         .= nId
        ]

instance ( ToJSONKey v, ToJSON v, ToJSON x ) => ToJSON (CycleCntx v x)
instance ( ToJSONKey v, ToJSON v, ToJSON x ) => ToJSON (TestbenchReport v x)


-- *Simple synthesis
instance ToJSON ObjectiveFunctionConf
instance ToJSON Parameters

instance ( VarValTimeJSON v x t
        ) => ToJSON (Edge m (SynthesisDT (BusNetwork String v x t))) where
    toJSON Edge{ eObjectiveFunctionValue, eParameters, eOption, eDecision } = object
        [ "eObjectiveFunctionValue" .= eObjectiveFunctionValue
        , "eParameters"             .= eParameters
        , "eOption"                 .= eOption
        , "eDecision"               .= eDecision
        ]



-- *Basic data
instance ( ToJSON tag, ToJSON t ) => ToJSON (TaggedTime tag t)

instance ( Show v ) => ToJSON (F v x) where
    toJSON = String . T.pack . show

instance ( ToJSON t, Time t ) => ToJSON (TimeConstrain t) where
    toJSON TimeConstrain{..} = object
        [ "available" .= tcAvailable
        , "duration"  .= tcDuration
        ]

instance ToJSONKey (IntX w) where
    toJSONKey = let
            ToJSONKeyText f g = toJSONKey
        in ToJSONKeyText (\(IntX x) -> f x) (\(IntX x) -> g x)


instance ToJSON (IntX w) where
    toJSON ( IntX x ) = toJSON x

instance ToJSONKey (FX m b) where
    toJSONKey = let
            ToJSONKeyText f g = toJSONKey
        in ToJSONKeyText (\( FX x ) -> f $ show x) (\( FX x ) -> g $ show x)

instance ToJSON (FX m b) where
    toJSON ( FX x ) = toJSON $ show x



-- *System
instance ( Show a, Bounded a ) => ToJSON (Interval a) where
    toJSON = String . T.pack . S.replace (show (maxBound :: a)) "∞" . show
