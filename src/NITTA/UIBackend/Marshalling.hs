{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
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
module NITTA.UIBackend.Marshalling
    ( Viewable(..)
    , SynthesisNodeView
    , SynthesisDecisionView
    , DataflowEndpointView
    , EdgeView
    , TreeView
    ) where

import           Control.Concurrent.STM
import           Data.Aeson
import           Data.Hashable
import qualified Data.HashMap.Strict             as HM
import qualified Data.Map                        as M
import           Data.Maybe
import qualified Data.Set                        as S
import qualified Data.String.Utils               as S
import qualified Data.Text                       as T
import           GHC.Generics
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.Problems.Refactor
import           NITTA.Model.Problems.Whole
import           NITTA.Model.ProcessorUnits.Time
import           NITTA.Model.TargetSystem
import           NITTA.Model.Types
import           NITTA.Project.Parts.TestBench
import           NITTA.Synthesis.Tree
import           NITTA.Synthesis.Utils
import           NITTA.UIBackend.Timeline
import           NITTA.Utils                     (transferred)
import           Numeric.Interval
import           Servant

-- *Intermediate representation between raw haskell representation and JSON

class Viewable t v | t -> v where
    view :: t -> v



data SynthesisNodeView
    = SynthesisNodeView
        { svNnid             :: NId
        , svCntx             :: [ String ]
        , svIsComplete       :: Bool
        , svIsEdgesProcessed :: Bool
        , svDuration         :: Int
        , svCharacteristic   :: Float -- FIXME:
        , svOptionType       :: String
        }
    deriving ( Generic )

instance ToJSON SynthesisNodeView

data TreeView a = NodeView
        { rootLabel :: a
        , subForest :: [ TreeView a ]
        }
    deriving ( Generic )

instance ( ToJSON a ) => ToJSON (TreeView a)


instance ( UnitTag tag, VarValTime v x t
        ) => Viewable (G Node tag v x t) (IO (TreeView SynthesisNodeView)) where
    view Node{ nId, nIsComplete, nModel, nEdges, nOrigin } = do
        nodesM <- readTVarIO nEdges
        nodes <- case nodesM of
            Just ns -> mapM (view . eTarget) ns
            Nothing -> return []
        return NodeView
            { rootLabel=SynthesisNodeView
                { svNnid=nId
                , svCntx=[]
                , svIsComplete=nIsComplete
                , svIsEdgesProcessed=isJust nodesM
                , svDuration=fromEnum $ targetProcessDuration nModel
                , svCharacteristic=maybe (read "NaN") eObjectiveFunctionValue nOrigin
                , svOptionType=case nOrigin of
                    Just Edge{ eOption=Binding{} }  -> "Bind"
                    Just Edge{ eOption=Dataflow{} } -> "Transport"
                    Just Edge{ eOption=Refactor{} } -> "Refactor"
                    Nothing                         -> "-"
                }
            , subForest=nodes
            }



instance Viewable (F v x) String where
    view = show



data DataflowEndpointView tag tp
    = DataflowEndpointView
        { pu   :: tag
        , time :: tp
        }
    deriving ( Generic )

instance ( ToJSON tp, ToJSON tag ) => ToJSON (DataflowEndpointView tag tp)



data SynthesisDecisionView tag v x tp
    = BindingView{ function :: String, pu :: tag }
    | DataflowView
        { source  :: DataflowEndpointView tag tp
        , targets :: HM.HashMap v (Maybe (DataflowEndpointView tag tp))
        }
    | RefactorView (Refactor v x)
    deriving ( Generic )

instance ( Show x, Show v, ToJSON v, ToJSONKey v, ToJSON tp, ToJSON tag
        ) => ToJSON (SynthesisDecisionView tag v x tp)

instance ( Eq v, Hashable v ) => Viewable (SynthesisStatement tag v x tp) (SynthesisDecisionView tag v x tp) where
    view (Binding f pu) = BindingView{ function=show f, pu }
    view Dataflow{ dfSource=(stag, st), dfTargets } = DataflowView
        { source=DataflowEndpointView stag st
        , targets=HM.map
            (fmap $ \(tag, t) -> DataflowEndpointView tag t)
            $ HM.fromList $ M.assocs dfTargets
        }
    view (Refactor r) = RefactorView r



data EdgeView tag v x t
    = EdgeView
        { nid :: String
        , option :: SynthesisDecisionView tag v x (TimeConstrain t)
        , decision :: SynthesisDecisionView tag v x (Interval t)
        , parameters :: Parameters
        , objectiveFunctionValue :: Float
        }
    deriving ( Generic )

instance ( VarValTimeJSON v x t, Hashable v, ToJSON tag ) => ToJSON (EdgeView tag v x t)

instance ( VarValTimeJSON v x t, Hashable v ) => Viewable (G Edge tag v x t) (EdgeView tag v x t) where
    view Edge{ eTarget, eOption, eDecision, eParameters, eObjectiveFunctionValue }
        = EdgeView
            { nid=show $ nId eTarget
            , option=view eOption
            , decision=view eDecision
            , parameters=eParameters
            , objectiveFunctionValue=eObjectiveFunctionValue
            }


-- JSON

type VarValTimeJSON v x t = ( Var v, Val x, Time t, ToJSONKey v, ToJSON v, ToJSON x, ToJSON t )


-- *Option/Decision
instance ( VarValTimeJSON v x t ) => ToJSON (SynthesisStatement String v x (TimeConstrain t))
instance ( VarValTimeJSON v x t ) => ToJSON (SynthesisStatement String v x (Interval t))
instance ( ToJSON v, Show v, Show x ) => ToJSON (Refactor v x) where
    toJSON = toJSON . show
instance ( Time t ) => ToJSON (EndpointSt String (TimeConstrain t)) where
    toJSON EndpointSt{ epRole=Source vs, epAt } = toJSON ("Source: " ++ S.join ", " (S.elems vs) ++ " at " ++ show epAt)
    toJSON EndpointSt{ epRole=Target v, epAt } = toJSON ("Target: " ++ v ++ " at " ++ show epAt)



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
instance ( ToJSON v ) => ToJSON (Lock v)

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
        ) => ToJSON (G Node String v x t) where
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
        ) => ToJSON (G Edge String v x t) where
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
