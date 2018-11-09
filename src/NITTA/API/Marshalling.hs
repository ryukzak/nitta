{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.API.Marshalling
Description : Marshalling data for REST API
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.API.Marshalling where

import           Data.Aeson
import qualified Data.Map              as M
import qualified Data.Text             as T
import           Data.Typeable
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.DataFlow
import           NITTA.Types
import           NITTA.Types.Project
import           NITTA.Types.Synthesis
import           NITTA.Utils           (transfered)
import           Numeric.Interval
import           Servant



-- *Option/Decision
instance ( ToJSON title
         , ToJSONKey v, ToJSON v, Var v
         , ToJSON (TimeConstrain t)
         ) => ToJSON (Option (CompilerDT title tag v t))
instance ( ToJSON title
         , ToJSONKey v, ToJSON v, Var v
         , ToJSON (TimeConstrain t), Time t
         ) => ToJSON (Decision (CompilerDT title tag v t))
instance ( ToJSON title
         , ToJSONKey v
         , ToJSON (TimeConstrain t)
         ) => ToJSON (Option (DataFlowDT title v t))
instance ( ToJSON title
         , ToJSONKey v
         , ToJSON (TimeConstrain t), Time t
         ) => ToJSON (Decision (DataFlowDT title v t))
instance ( Show title
         ) => ToJSON (Option (BindingDT title v x)) where
    toJSON (BindingO f title) = toJSON [ show f, show title ]
instance ( Show title
         ) => ToJSON (Decision (BindingDT title v x)) where
    toJSON (BindingD f title) = toJSON [ show f, show title ]
-- instance ( ToJSON v, Var v ) => ToJSON (Option (ControlDT v))
-- instance ( ToJSON v, Var v ) => ToJSON (Decision (ControlDT v))



-- *Process units
instance ( ToJSONKey title, ToJSON title, Typeable title, Ord title, Show title
         , Var v
         , Time t, ToJSON t
         , Typeable x, ToJSON x, ToJSONKey x
         ) => ToJSON (BusNetwork title v x t) where
    toJSON n@BusNetwork{..} = object
        [ "width" .= bnSignalBusWidth
        , "remain" .= bnRemains
        , "forwardedVariables" .= map (String . T.pack . show) (transfered n)
        , "binds" .= bnBinded
        , "processLength" .= nextTick (process n)
        , "processUnits" .= M.keys bnPus
        , "process" .= process n
        ]



-- *Model
instance ( ToJSON v, Var v ) => ToJSON (DataFlowGraph v)

instance ToJSON Relation where
    toJSON (Vertical a b) = toJSON [ a, b ]

instance ( ToJSONKey title, ToJSON title, Show title, Ord title, Typeable title
         , ToJSON tag
         , ToJSON v, Var v
         , ToJSON t, Time t
         , ToJSONKey v
         , Show x, Ord x, Typeable x, ToJSON x, ToJSONKey x
         ) => ToJSON (ModelState title tag x v t)

instance ( ToJSON t, Time t, Show v
         ) => ToJSON (Process v x t) where
    toJSON Process{ steps, nextTick, relations } = object
        [ "steps" .= steps
        , "nextTick" .= nextTick
        , "relations" .= relations
        ]

instance ( ToJSON t, Time t, Show v
         ) => ToJSON (Step v x t) where
    toJSON Step{ sKey, sTime, sDesc } = object
        [ "sKey" .= sKey
        , "sDesc" .= show sDesc
        , "sTime" .= sTime
        , "sLevel" .= level sDesc
        , "sPU" .= showPU sDesc
        ]



-- *Synthesis
instance ToJSON Nid where
    toJSON nid = toJSON $ show nid

instance FromJSON Nid where
    parseJSON v = read <$> parseJSON v

instance FromHttpApiData Nid where
    parseUrlPiece = Right . read . T.unpack


instance ToJSON (Synthesis String String String Int (TaggedTime String Int)) where
    toJSON Synthesis{ sModel, sCntx, sStatus } = object
        [ "sModel" .= sModel
        , "sCntx" .= map show sCntx
        , "sStatus" .= show sStatus
        ]

instance ToJSON SynthesisStatus
instance ToJSON TestBenchReport


-- *Simple compiler
instance ToJSON SynthesisSetup
instance ToJSON SpecialMetrics

instance ToJSON (WithMetric (CompilerDT String String String (TaggedTime String Int))) where
    toJSON WithMetric{ mIntegral, mSpecial, mOption, mDecision }
        = toJSON ( mIntegral, mSpecial, mOption, mDecision )


-- *Basic data
instance ( ToJSON tag, ToJSON t ) => ToJSON (TaggedTime tag t)

instance ( ToJSON t, Time t ) => ToJSON (PlaceInTime t) where
    toJSON (Event t)    = toJSON [ fromEnum t, fromEnum t ]
    toJSON (Activity i) = toJSON [ fromEnum $ inf i, fromEnum $ sup i ]

instance ( Show v ) => ToJSON (F v x) where
    toJSON = String . T.pack . show

instance ( ToJSON t, Time t ) => ToJSON (TimeConstrain t) where
    toJSON TimeConstrain{..} = object
        [ "available" .= tcAvailable
        , "duration" .= tcDuration
        ]



-- *System
instance ( Show a ) => ToJSON (Interval a) where
    toJSON = String . T.pack . show
