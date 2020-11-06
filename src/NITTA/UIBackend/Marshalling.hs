{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-orphans #-}

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
    , RefactorView
    , ParametersView
    , DataflowEndpointView
    , NodeView, EdgeView, FView
    , TreeView, viewNodeTree
    , IntervalView, TimeConstrainView, UnitEndpointView(..)
    , TestbenchReportView(..)
    ) where

import           Control.Concurrent.STM
import           Data.Aeson
import           Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.String.Utils as S
import qualified Data.Text as T
import           GHC.Generics
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Problems
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.TargetSystem
import           NITTA.Model.Types
import           NITTA.Project
import           NITTA.Synthesis.Estimate
import           NITTA.Synthesis.Tree
import           NITTA.Synthesis.Utils
import           NITTA.UIBackend.Timeline
import           NITTA.Utils ( transferred )
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

data TreeView a = TreeNodeView
        { rootLabel :: a
        , subForest :: [ TreeView a ]
        }
    deriving ( Generic )

instance ( ToJSON a ) => ToJSON (TreeView a)


viewNodeTree Node{ nId, nIsComplete, nModel, nEdges, nOrigin } = do
    nodesM <- readTVarIO nEdges
    nodes <- case nodesM of
        Just ns -> mapM (viewNodeTree . eTarget) ns
        Nothing -> return []
    return TreeNodeView
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



data DataflowEndpointView tag tp
    = DataflowEndpointView
        { pu   :: tag
        , time :: tp
        }
    deriving ( Generic )

instance ( ToJSON tp, ToJSON tag ) => ToJSON (DataflowEndpointView tag tp)



data SynthesisDecisionView tag v x tp
    = BindingView
        { function :: FView
        , pu       :: tag
        , vars     :: [ String ]
        }
    | DataflowView
        { source  :: DataflowEndpointView tag tp
        , targets :: HM.HashMap v (Maybe (DataflowEndpointView tag tp))
        }
    | RefactorView RefactorView
    deriving ( Generic )

instance ( Show x, Show v, ToJSON v, ToJSONKey v, ToJSON tp, ToJSON tag
        ) => ToJSON (SynthesisDecisionView tag v x tp)

instance ( Var v, Hashable v, Show x
         ) => Viewable
             ( NId, SynthesisStatement tag v x tp )
             ( NId, SynthesisDecisionView tag v x tp ) where
    view ( nid, st ) = ( nid, view st )



instance Viewable (Refactor () ()) RefactorView where
    view (ResolveDeadlock set) = ResolveDeadlockView $ map show $ S.toList set
    view BreakLoop {loopX, loopO, loopI} =
        BreakLoopView
            { loopX = show loopX
            , loopO = map show ( S.toList loopO )
            , loopI = show loopI
            }
    view (OptimizeAccum refOld refNew) = OptimizeAccumView (map view refOld) (map view refNew)


instance ( Var v, Show x, Hashable v
         ) => Viewable (SynthesisStatement tag v x tp) (SynthesisDecisionView tag v x tp) where
    view (Binding f pu) = BindingView
        { function=view f
        , pu
        , vars=map (S.replace "\"" "" . show) $ S.elems $ variables f
        }
    view Dataflow{ dfSource=(stag, st), dfTargets } = DataflowView
        { source=DataflowEndpointView stag st
        , targets=HM.map
            (fmap $ uncurry DataflowEndpointView)
            $ HM.fromList $ M.assocs dfTargets
        }
    view (Refactor (ResolveDeadlock set )) = RefactorView $ ResolveDeadlockView $ map show $ S.toList set
    view (Refactor (BreakLoop {loopX, loopO, loopI})) = RefactorView $
        BreakLoopView
            { loopX = show loopX
            , loopO = map show ( S.toList loopO )
            , loopI = show loopI
            }
    view (Refactor (OptimizeAccum refOld refNew)) = RefactorView $ OptimizeAccumView (map view refOld) (map view refNew)


data RefactorView
    = ResolveDeadlockView [String]
    | BreakLoopView
        { loopX :: String -- ^initial looped value
        , loopO :: [String] -- ^output variables
        , loopI :: String -- ^input variable
        }
    | OptimizeAccumView [FView] [FView]
    deriving ( Generic , Show)

instance ToJSON RefactorView

data NodeView tag v x t
    = NodeView
        { nvId         :: NId
        --, nvModel      :: ModelState (BusNetwork tag v x t) v x
        , nvIsComplete :: Bool
        , nvOrigin     :: Maybe (EdgeView tag v x t)
        }
    deriving ( Generic )

instance ( VarValTimeJSON v x t, Hashable v, ToJSON tag ) => ToJSON (NodeView tag v x t)

instance ( VarValTimeJSON v x t, Hashable v
         ) => Viewable (G Node tag v x t) (NodeView tag v x t) where
    view Node{ nId, nOrigin, nIsComplete }
        = NodeView
            { nvId=nId
            --, nvModel=nModel
            , nvIsComplete=nIsComplete
            , nvOrigin=fmap view nOrigin
            }



data EdgeView tag v x t
    = EdgeView
        { nid                    :: String
        , option                 :: SynthesisDecisionView tag v x (TimeConstrain t)
        , decision               :: SynthesisDecisionView tag v x (Interval t)
        , parameters             :: ParametersView
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
            , parameters=view eParameters
            , objectiveFunctionValue=eObjectiveFunctionValue
            }

data ParametersView
    = BindEdgeParameterView
        { pCritical                :: Bool
        , pAlternative             :: Float
        , pRestless                :: Float
        , pOutputNumber            :: Float
        , pAllowDataFlow           :: Float
        , pPossibleDeadlock        :: Bool
        , pNumberOfBindedFunctions :: Float
        , pPercentOfBindedInputs   :: Float
        , pWave                    :: Maybe Float
        }
    | DataFlowEdgeParameterView
        { pWaitTime              :: Float
        , pRestrictedTime        :: Bool
        , pNotTransferableInputs :: [Float]
        }
    | RefactorEdgeParameterView
        { pRefactorType                  :: RefactorView
        , pNumberOfLockedVariables       :: Float
        , pBufferCount                   :: Float
        , pNStepBackRepeated             :: Maybe Int
        , pNumberOfTransferableVariables :: Float
        }
    deriving ( Show, Generic )

instance ToJSON ParametersView

instance Viewable Parameters ParametersView where
    view BindEdgeParameter
        { pCritical
        , pAlternative
        , pRestless
        , pOutputNumber
        , pAllowDataFlow
        , pPossibleDeadlock
        , pNumberOfBindedFunctions
        , pPercentOfBindedInputs
        , pWave
        } = BindEdgeParameterView
            { pCritical = pCritical
            , pAlternative = pAlternative
            , pRestless = pRestless
            , pOutputNumber = pOutputNumber
            , pAllowDataFlow = pAllowDataFlow
            , pPossibleDeadlock = pPossibleDeadlock
            , pNumberOfBindedFunctions = pNumberOfBindedFunctions
            , pPercentOfBindedInputs = pPercentOfBindedInputs
            , pWave = pWave
            }

    view DataFlowEdgeParameter { pWaitTime, pRestrictedTime, pNotTransferableInputs } = DataFlowEdgeParameterView
        { pWaitTime = pWaitTime
        , pRestrictedTime = pRestrictedTime
        , pNotTransferableInputs
        }

    view RefactorEdgeParameter
        { pRefactorType
        , pNumberOfLockedVariables
        , pBufferCount
        , pNStepBackRepeated
        , pNumberOfTransferableVariables } = RefactorEdgeParameterView
            { pRefactorType  = view pRefactorType
            , pNumberOfLockedVariables = pNumberOfLockedVariables
            , pBufferCount = pBufferCount
            , pNStepBackRepeated = pNStepBackRepeated
            , pNumberOfTransferableVariables = pNumberOfTransferableVariables
            }


type VarValTimeJSON v x t = ( Var v, Val x, Time t, ToJSONKey v, ToJSON v, ToJSON x, ToJSON t )


-- *Option/Decision
instance ( VarValTimeJSON v x t ) => ToJSON (SynthesisStatement String v x (TimeConstrain t))
instance ( VarValTimeJSON v x t ) => ToJSON (SynthesisStatement String v x (Interval t))
instance ( ToJSON v, Show v, Show x ) => ToJSON (Refactor v x) where
    toJSON = toJSON . show
instance ( Time t ) => ToJSON (EndpointSt String (TimeConstrain t)) where
    toJSON EndpointSt{ epRole=Source vs, epAt } = toJSON ("Source: " ++ S.join ", " (S.elems vs) ++ " at " ++ show epAt)
    toJSON EndpointSt{ epRole=Target v, epAt } = toJSON ("Target: " ++ v ++ " at " ++ show epAt)
instance ( ToJSON v ) => ToJSON (EndpointRole v)


data TimeConstrainView = TimeConstrainView
        { vAvailable :: IntervalView
        , vDuration  :: IntervalView
        }
    deriving ( Generic )

instance ToJSON TimeConstrainView
instance ( Show t, Bounded t ) => Viewable (TimeConstrain t) TimeConstrainView where
    view TimeConstrain{ tcAvailable, tcDuration } = TimeConstrainView
            { vAvailable=view tcAvailable
            , vDuration=view tcDuration
            }

instance ( Show t, Bounded t
        ) => Viewable (EndpointSt v (TimeConstrain t)) (EndpointSt v TimeConstrainView) where
    view EndpointSt{ epRole, epAt } = EndpointSt{ epRole, epAt=view epAt }

instance ( ToJSON v ) => ToJSON (EndpointSt v TimeConstrainView)


data UnitEndpointView tag v = UnitEndpointView
        { unitTag   :: tag
        , endpoints :: EndpointSt v TimeConstrainView
        }
    deriving ( Generic )

instance ( ToJSON tag, ToJSON v ) => ToJSON ( UnitEndpointView tag v )



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


showPU si = S.replace "\"" "" $ S.join "." $ showPU' si
    where
        showPU' (NestedStep tag Step{ sDesc }) = show tag : showPU' sDesc
        showPU' _                              = []


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

data TestbenchReportView v x
    = TestbenchReportView
        { tbStatus                   :: Bool
        , tbPath                     :: String
        , tbFiles                    :: [ String ]
        , tbFunctions                :: [ String ]
        , tbSynthesisSteps           :: [ String ]
        , tbCompilerDump             :: [ String ]
        , tbSimulationDump           :: [ String ]
        , tbFunctionalSimulationCntx :: [ HM.HashMap v x ]
        , tbLogicalSimulationCntx    :: [ HM.HashMap v x ]
        }
    deriving ( Generic )

instance ( ToJSONKey v, ToJSON x ) => ToJSON (TestbenchReportView v x)

instance ( Eq v, Hashable v ) => Viewable (TestbenchReport v x) (TestbenchReportView v x) where
    view TestbenchReport{ tbLogicalSimulationCntx, .. } = TestbenchReportView
        { tbLogicalSimulationCntx=map (HM.fromList . M.assocs . cycleCntx) $ cntxProcess tbLogicalSimulationCntx
        , ..
        }



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

data FView = FView
           { fvFun     :: String
           , fvHistory :: [ String ]
           }
    deriving ( Generic, Show )

instance ToJSON FView
instance Viewable (F v x) FView where
    view F{ fun, funHistory } = FView
        { fvFun=S.replace "\"" "" $ show fun
        , fvHistory=map (S.replace "\"" "" . show) funHistory
        }

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
newtype IntervalView = IntervalView String
    deriving ( Generic )

instance ( Show a, Bounded a ) => Viewable (Interval a) IntervalView where
    view = IntervalView . S.replace (show (maxBound :: a)) "∞" . show

instance ToJSON IntervalView

instance ( Show a, Bounded a ) => ToJSON (Interval a) where
    toJSON = String . T.pack . S.replace (show (maxBound :: a)) "∞" . show
