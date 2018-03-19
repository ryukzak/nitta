{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

module NITTA.Utils.JSON where

import           Data.Aeson
import qualified Data.Map         as M
import           Data.Text        (pack)
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.DataFlow
import           NITTA.Types
import           Numeric.Interval


instance ( Show a ) => ToJSON (Interval a) where
  toJSON = String . pack . show


instance ( ToJSON tag
         , ToJSON t
         ) => ToJSON (TaggedTime tag t)
instance ( Show v
         ) => ToJSON (FB (Parcel v) v) where
  toJSON = String . pack . show
instance ( ToJSON t, Time t
         ) => ToJSON (TimeConstrain t) where
  toJSON TimeConstrain{..} = object [ "available" .= tcAvailable
                                    , "duration" .= tcDuration
                                    ]


instance ToJSON NaiveOpt
instance ( ToJSONKey title, ToJSON title, Title title
         , ToJSON tag
         , ToJSON v, Var v
         , ToJSON t, Time t
         ) => ToJSON (SystemState title tag v t)
instance ( ToJSON v, Var v ) => ToJSON (DataFlowGraph v)


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
instance ( ToJSON title
         , ToJSON v, Var v
         ) => ToJSON (Option (BindingDT title v))
instance ( ToJSON title
         , ToJSON v
         , Var v
         ) => ToJSON (Decision (BindingDT title v))
instance ( ToJSON v, Var v ) => ToJSON (Option (ControlDT v))
instance ( ToJSON v, Var v ) => ToJSON (Decision (ControlDT v))


instance ( ToJSONKey title, ToJSON title, Title title
         , Var v
         , ToJSON t, Time t
         ) => ToJSON (BusNetwork title v t) where
  toJSON n@BusNetwork{..}
             -- , bnSignalBusWidth     :: Int
    = object [ "width" .= bnSignalBusWidth
             --   bnRemains            :: [FB (Parcel v) v]
             , "remain" .= bnRemains
             -- , bnForwardedVariables :: [v]
             , "forwardedVariables" .= map (String . pack . show) bnForwardedVariables
             -- , bnBinded             :: M.Map title [FB (Parcel v) v]
             , "binds" .= bnBinded
             -- , bnProcess            :: Process v t
             , "processLength" .= nextTick (process n)
             -- , bnPus                :: M.Map title spu
             , "processUnits" .= M.keys bnPus
             ]


instance ToJSON SpecialMetrics
instance ToJSON GlobalMetrics
