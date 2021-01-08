{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.UIBackend.ViewHelper
Description : Types for marshaling data for REST API
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

We can not autogenerate ToJSON implementation for some types, so we add helper
types for doing that automatically. Why do we need to generate `ToJSON`
automatically? We don't want to achieve consistency between client and server
manually.
-}
module NITTA.UIBackend.ViewHelper (
    Viewable (..),
    viewNodeTree,
    TreeView,
    NodeView,
    EdgeView,
    SynthesisNodeView,
    SynthesisStatementView,
    RefactorView,
    ParametersView,
    EndpointStView (..),
    DataflowEndpointView,
    TestbenchReportView (..),
    FView,
    TimeConstrainView,
    IntervalView,
) where

import Control.Concurrent.STM
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.String.Utils as S
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.TargetSystem
import NITTA.Model.Types
import NITTA.Project (TestbenchReport (..))
import NITTA.Synthesis.Estimate
import NITTA.Synthesis.Tree
import NITTA.UIBackend.Orphans
import Numeric.Interval
import Servant
import Servant.Docs

-- |Type class of view helper
class Viewable t v | t -> v where
    view :: t -> v

-- Synthesis tree

data TreeView a = TreeNodeView
    { rootLabel :: a
    , subForest :: [TreeView a]
    }
    deriving (Generic)

instance (ToJSON a) => ToJSON (TreeView a)

instance ToSample (TreeView SynthesisNodeView) where
    toSamples _ =
        singleSample $
            TreeNodeView
                { rootLabel =
                    SynthesisNodeView
                        { svNnid = NId []
                        , svIsComplete = False
                        , svIsEdgesProcessed = True
                        , svDuration = 0
                        , svCharacteristic = 0 / 0
                        , svOptionType = "-"
                        }
                , subForest =
                    [ TreeNodeView
                        { rootLabel =
                            SynthesisNodeView
                                { svNnid = NId [0]
                                , svIsComplete = False
                                , svIsEdgesProcessed = False
                                , svDuration = 0
                                , svCharacteristic = 4052
                                , svOptionType = "Bind"
                                }
                        , subForest = []
                        }
                    , TreeNodeView
                        { rootLabel =
                            SynthesisNodeView
                                { svNnid = NId [1]
                                , svIsComplete = False
                                , svIsEdgesProcessed = False
                                , svDuration = 0
                                , svCharacteristic = 3021
                                , svOptionType = "Bind"
                                }
                        , subForest = []
                        }
                    ]
                }

data SynthesisNodeView = SynthesisNodeView
    { svNnid :: NId
    , svIsComplete :: Bool
    , svIsEdgesProcessed :: Bool
    , svDuration :: Int
    , svCharacteristic :: Float -- FIXME: Maybe?
    , svOptionType :: String
    }
    deriving (Generic)

instance ToJSON SynthesisNodeView

viewNodeTree Node{nId, nIsComplete, nModel, nEdges, nOrigin} = do
    nodesM <- readTVarIO nEdges
    nodes <- case nodesM of
        Just ns -> mapM (viewNodeTree . eTarget) ns
        Nothing -> return []
    return
        TreeNodeView
            { rootLabel =
                SynthesisNodeView
                    { svNnid = nId
                    , svIsComplete = nIsComplete
                    , svIsEdgesProcessed = isJust nodesM
                    , svDuration = fromEnum $ processDuration nModel
                    , svCharacteristic = maybe (read "NaN") eObjectiveFunctionValue nOrigin
                    , svOptionType = case nOrigin of
                        Just Edge{eOption = Binding{}} -> "Bind"
                        Just Edge{eOption = Dataflow{}} -> "Transport"
                        Just Edge{eOption = Refactor{}} -> "Refactor"
                        Nothing -> "-"
                    }
            , subForest = nodes
            }

data NodeView tag v x t = NodeView
    { nvId :: NId
    , nvIsComplete :: Bool
    , nvOrigin :: Maybe (EdgeView tag v x t)
    }
    deriving (Generic)

instance
    ( VarValTimeJSON v x t
    , Hashable v
    ) =>
    Viewable (G Node tag v x t) (NodeView tag v x t)
    where
    view Node{nId, nOrigin, nIsComplete} =
        NodeView
            { nvId = nId
            , nvIsComplete = nIsComplete
            , nvOrigin = fmap view nOrigin
            }

instance (VarValTimeJSON v x t, Hashable v, ToJSON tag) => ToJSON (NodeView tag v x t)

instance {-# OVERLAPS #-} ToSample [NodeView String String Int Int] where
    toSamples _ =
        samples
            [ []
            , [NodeView{nvId = NId [], nvIsComplete = False, nvOrigin = Nothing}]
            ]

instance ToSample (NodeView String String Int Int) where
    toSamples _ =
        [ ("root", NodeView{nvId = NId [], nvIsComplete = False, nvOrigin = Nothing})
        ,
            ( "final"
            , NodeView
                { nvId = NId [1, 1, 2, 3, 1, 2, 4, 5, 6]
                , nvIsComplete = True
                , nvOrigin = fmap (\e -> e{nid = show $ NId [1, 1, 2, 3, 1, 2, 4, 5]}) $ toSample Proxy
                }
            )
        ]

data EdgeView tag v x t = EdgeView
    { nid :: String
    , option :: SynthesisStatementView tag v x (TimeConstrain t)
    , decision :: SynthesisStatementView tag v x (Interval t)
    , parameters :: ParametersView
    , objectiveFunctionValue :: Float
    }
    deriving (Generic)

instance (VarValTimeJSON v x t, Hashable v) => Viewable (G Edge tag v x t) (EdgeView tag v x t) where
    view Edge{eTarget, eOption, eDecision, eParameters, eObjectiveFunctionValue} =
        EdgeView
            { nid = show $ nId eTarget
            , option = view eOption
            , decision = view eDecision
            , parameters = view eParameters
            , objectiveFunctionValue = eObjectiveFunctionValue
            }

instance (VarValTimeJSON v x t, Hashable v, ToJSON tag) => ToJSON (EdgeView tag v x t)

dataflowViewSample =
    EdgeView
        { nid = show $ NId [3, 4, 3, 2, 0, 1]
        , option =
            DataflowView
                { source = DataflowEndpointView "PU1" $ TimeConstrain (1 ... maxBound) (1 ... maxBound)
                , targets = HM.fromList [("a1", Nothing), ("a2", Just $ DataflowEndpointView "PU2" $ TimeConstrain (1 ... maxBound) (1 ... 1))]
                }
        , decision =
            DataflowView
                { source = DataflowEndpointView "PU1" (1 ... 1)
                , targets = HM.fromList [("a1", Nothing), ("a2", Just $ DataflowEndpointView "PU2" (1 ... 1))]
                }
        , parameters =
            DataFlowEdgeParameterView
                { pWaitTime = 1
                , pRestrictedTime = False
                , pNotTransferableInputs = [0, 0]
                }
        , objectiveFunctionValue = 1999
        }

bindingViewSample =
    EdgeView
        { nid = show $ NId [3, 4, 3, 2, 0, 1]
        , option =
            BindingView
                { function = FView "reg(a) = b" []
                , pu = "PU1"
                , vars = ["a", "b"]
                }
        , decision =
            BindingView
                { function = FView "reg(a) = b" []
                , pu = "PU1"
                , vars = ["a", "b"]
                }
        , parameters =
            BindEdgeParameterView
                { pCritical = True
                , pAlternative = 1
                , pRestless = 1
                , pOutputNumber = 1
                , pAllowDataFlow = 1
                , pPossibleDeadlock = False
                , pNumberOfBindedFunctions = 1
                , pPercentOfBindedInputs = 1
                , pWave = Just 1
                }
        , objectiveFunctionValue = 19
        }

refactorViewSample =
    EdgeView
        { nid = show $ NId [3, 4, 3, 2, 0, 1]
        , option =
            BindingView
                { function = FView "reg(a) = b" []
                , pu = "PU1"
                , vars = ["a", "b"]
                }
        , decision =
            BindingView
                { function = FView "reg(a) = b" []
                , pu = "PU1"
                , vars = ["a", "b"]
                }
        , parameters =
            RefactorEdgeParameterView
                { pRefactorType = ResolveDeadlockView ["c"]
                , pNumberOfLockedVariables = 1
                , pBufferCount = 1
                , pNStepBackRepeated = Just 1
                , pNumberOfTransferableVariables = 1
                }
        , objectiveFunctionValue = 10000
        }

instance {-# OVERLAPS #-} ToSample [EdgeView String String Int Int] where
    toSamples _ = singleSample [dataflowViewSample, bindingViewSample, refactorViewSample]

instance ToSample (EdgeView String String Int Int) where
    toSamples _ =
        [ ("dataflow edge", dataflowViewSample)
        , ("bind edge", bindingViewSample)
        , ("refactor edge", refactorViewSample)
        ]

-- Problems

data SynthesisStatementView tag v x tp
    = BindingView
        { function :: FView
        , pu :: tag
        , vars :: [String]
        }
    | DataflowView
        { source :: DataflowEndpointView tag tp
        , targets :: HM.HashMap v (Maybe (DataflowEndpointView tag tp))
        }
    | RefactorView RefactorView
    deriving (Generic)

instance
    ( Var v
    , Hashable v
    , Show x
    ) =>
    Viewable
        (NId, SynthesisStatement tag v x tp)
        (NId, SynthesisStatementView tag v x tp)
    where
    view (nid, st) = (nid, view st)

instance
    ( Var v
    , Show x
    , Hashable v
    ) =>
    Viewable (SynthesisStatement tag v x tp) (SynthesisStatementView tag v x tp)
    where
    view (Binding f pu) =
        BindingView
            { function = view f
            , pu
            , vars = map (S.replace "\"" "" . show) $ S.elems $ variables f
            }
    view Dataflow{dfSource = (stag, st), dfTargets} =
        DataflowView
            { source = DataflowEndpointView stag st
            , targets =
                HM.map
                    (fmap $ uncurry DataflowEndpointView)
                    $ HM.fromList $ M.assocs dfTargets
            }
    view (Refactor ref) = RefactorView $ view ref

instance
    ( Show x
    , Show v
    , ToJSON v
    , ToJSONKey v
    , ToJSON tp
    , ToJSON tag
    ) =>
    ToJSON (SynthesisStatementView tag v x tp)

data DataflowEndpointView tag tp = DataflowEndpointView
    { pu :: tag
    , time :: tp
    }
    deriving (Generic)

instance (ToJSON tp, ToJSON tag) => ToJSON (DataflowEndpointView tag tp)

data RefactorView
    = ResolveDeadlockView [String]
    | BreakLoopView
        { -- |initial looped value
          loopX :: String
        , -- |output variables
          loopO :: [String]
        , -- |input variable
          loopI :: String
        }
    | OptimizeAccumView
        { oldSubGraph :: [FView]
        , newSubGraph :: [FView]
        }
    deriving (Generic, Show)

instance (Show v, Show x) => Viewable (Refactor v x) RefactorView where
    view (ResolveDeadlock set) = ResolveDeadlockView $ map show $ S.toList set
    view BreakLoop{loopX, loopO, loopI} =
        BreakLoopView
            { loopX = show loopX
            , loopO = map show (S.toList loopO)
            , loopI = show loopI
            }
    view OptimizeAccum{refOld, refNew} =
        OptimizeAccumView (map view refOld) (map view refNew)

instance ToJSON RefactorView

data ParametersView
    = BindEdgeParameterView
        { pCritical :: Bool
        , pAlternative :: Float
        , pRestless :: Float
        , pOutputNumber :: Float
        , pAllowDataFlow :: Float
        , pPossibleDeadlock :: Bool
        , pNumberOfBindedFunctions :: Float
        , pPercentOfBindedInputs :: Float
        , pWave :: Maybe Float
        }
    | DataFlowEdgeParameterView
        { pWaitTime :: Float
        , pRestrictedTime :: Bool
        , pNotTransferableInputs :: [Float]
        }
    | RefactorEdgeParameterView
        { pRefactorType :: RefactorView
        , pNumberOfLockedVariables :: Float
        , pBufferCount :: Float
        , pNStepBackRepeated :: Maybe Int
        , pNumberOfTransferableVariables :: Float
        }
    deriving (Show, Generic)

instance Viewable Parameters ParametersView where
    view
        BindEdgeParameter
            { pCritical
            , pAlternative
            , pRestless
            , pOutputNumber
            , pAllowDataFlow
            , pPossibleDeadlock
            , pNumberOfBindedFunctions
            , pPercentOfBindedInputs
            , pWave
            } =
            BindEdgeParameterView
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
    view DataFlowEdgeParameter{pWaitTime, pRestrictedTime, pNotTransferableInputs} =
        DataFlowEdgeParameterView
            { pWaitTime = pWaitTime
            , pRestrictedTime = pRestrictedTime
            , pNotTransferableInputs
            }
    view
        RefactorEdgeParameter
            { pRefactorType
            , pNumberOfLockedVariables
            , pBufferCount
            , pNStepBackRepeated
            , pNumberOfTransferableVariables
            } =
            RefactorEdgeParameterView
                { pRefactorType = view pRefactorType
                , pNumberOfLockedVariables = pNumberOfLockedVariables
                , pBufferCount = pBufferCount
                , pNStepBackRepeated = pNStepBackRepeated
                , pNumberOfTransferableVariables = pNumberOfTransferableVariables
                }

instance ToJSON ParametersView

-- Endpoint

data EndpointStView tag v = EndpointStView
    { unitTag :: tag
    , endpoint :: EndpointSt v TimeConstrainView
    }
    deriving (Generic)

instance (Viewable tp tp') => Viewable (EndpointSt v tp) (EndpointSt v tp') where
    view EndpointSt{epRole, epAt} = EndpointSt{epRole, epAt = view epAt}

instance (ToJSON tag, ToJSON v) => ToJSON (EndpointStView tag v)

instance ToSample (EndpointStView String String) where
    toSamples _ =
        [ ("target", EndpointStView "PU1" $ view $ EndpointSt{epRole = Target "a", epAt = TimeConstrain{tcAvailable = (0 :: Int) ... maxBound, tcDuration = 1 ... maxBound}})
        , ("source", EndpointStView "PU2" $ view $ EndpointSt{epRole = Source $ S.singleton "a", epAt = TimeConstrain{tcAvailable = (0 :: Int) ... maxBound, tcDuration = 1 ... 1}})
        ]

-- Testbench

data TestbenchReportView v x = TestbenchReportView
    { tbStatus :: Bool
    , tbPath :: String
    , tbFiles :: [String]
    , tbFunctions :: [String]
    , tbSynthesisSteps :: [String]
    , tbCompilerDump :: [String]
    , tbSimulationDump :: [String]
    , tbFunctionalSimulationCntx :: [HM.HashMap v x]
    , tbLogicalSimulationCntx :: [HM.HashMap v x]
    }
    deriving (Generic)

instance (Eq v, Hashable v) => Viewable (TestbenchReport v x) (TestbenchReportView v x) where
    view TestbenchReport{tbLogicalSimulationCntx, ..} =
        TestbenchReportView
            { tbLogicalSimulationCntx = map (HM.fromList . M.assocs . cycleCntx) $ cntxProcess tbLogicalSimulationCntx
            , ..
            }

instance (ToJSONKey v, ToJSON x) => ToJSON (TestbenchReportView v x)

instance ToSample (TestbenchReportView String Int) where
    toSamples _ =
        singleSample
            TestbenchReportView
                { tbStatus = True
                , tbCompilerDump = ["stdout:", "stderr:"]
                , tbSimulationDump =
                    [ "stdout:"
                    , "VCD info: dumpfile web_ui_net_tb.vcd opened for output."
                    , "0:0\tactual: 0.000  0\t"
                    , "0:1\tactual: 0.000  0 \texpect: 0.000  0 \tvar: x#0\t"
                    , "0:2\tactual: 0.000  0\t"
                    , "0:3\tactual: 0.000  0\t"
                    , "0:4\tactual: 0.000  0 \texpect: 0.000  0 \tvar: tmp_0#0\t"
                    , "0:5\tactual: 0.000  0\t"
                    , "1:0\tactual: 0.000  0\t"
                    , "1:1\tactual: 0.000  0 \texpect: 0.000  0 \tvar: x#0\t"
                    , "1:2\tactual: 0.000  0\t"
                    , "1:3\tactual: 0.000  0\t"
                    , "1:4\tactual: 0.000  0 \texpect: 0.000  0 \tvar: tmp_0#0\t"
                    , "1:5\tactual: 0.000  0\t"
                    , "stderr:"
                    ]
                , tbPath = "/Users/penskoi/Documents/nitta-corp/nitta/gen/web_ui"
                , tbFiles =
                    [ "web_ui_net/web_ui_net.v"
                    , "lib/pu_simple_control.v"
                    , "lib/pu_accum.v"
                    , "lib/div_mock.v"
                    , "lib/pu_div.v"
                    , "lib/pu_fram.v"
                    , "lib/mult_mock.v"
                    , "lib/pu_multiplier.v"
                    , "lib/pu_shift.v"
                    , "lib/pu_slave_spi_driver.v"
                    , "lib/spi_slave_driver.v"
                    , "lib/i2n_splitter.v"
                    , "lib/buffer.v"
                    , "lib/bounce_filter.v"
                    , "lib/spi_master_driver.v"
                    , "lib/n2i_splitter.v"
                    , "lib/pu_slave_spi.v"
                    , "lib/pu_master_spi.v"
                    , "web_ui_net_tb.v"
                    ]
                , tbFunctions =
                    [ "reg(x#0) = tmp_0#0"
                    , "LoopIn (Loop (X 0.000000) (O [x#0]) (I tmp_0#0)) (I tmp_0#0)"
                    , "LoopOut (Loop (X 0.000000) (O [x#0]) (I tmp_0#0)) (O [x#0])"
                    ]
                , tbSynthesisSteps =
                    [ "Step {sKey = 19, sTime = 0 ... 0, sDesc = Nested fram2: Step {sKey = 0, sTime = 0 ... 0, sDesc = bind Loop (X 0.000000) (O [x#0]) (I tmp_0#0)}}"
                    , "Step {sKey = 18, sTime = 0 ... 0, sDesc = Nested fram2: Step {sKey = 1, sTime = 0 ... 0, sDesc = revoke Loop (X 0.000000) (O [x#0]) (I tmp_0#0)}}"
                    , "Step {sKey = 17, sTime = 0 ... 0, sDesc = Nested fram2: Step {sKey = 2, sTime = 0 ... 0, sDesc = bind LoopOut (Loop (X 0.000000) (O [x#0]) (I tmp_0#0)) (O [x#0])}}"
                    , "Step {sKey = 16, sTime = 0 ... 0, sDesc = Nested fram2: Step {sKey = 3, sTime = 0 ... 0, sDesc = bind LoopIn (Loop (X 0.000000) (O [x#0]) (I tmp_0#0)) (I tmp_0#0)}}"
                    , "Step {sKey = 15, sTime = 1 ... 1, sDesc = Nested fram2: Step {sKey = 4, sTime = 1 ... 1, sDesc = Source x#0}}"
                    , "Step {sKey = 14, sTime = 0 ... 0, sDesc = Nested fram2: Step {sKey = 5, sTime = 0 ... 0, sDesc = PrepareRead 0}}"
                    , "Step {sKey = 13, sTime = 0 ... 1, sDesc = Nested fram2: Step {sKey = 6, sTime = 0 ... 1, sDesc = LoopOut (Loop (X 0.000000) (O [x#0]) (I tmp_0#0)) (O [x#0])}}"
                    , "Step {sKey = 12, sTime = 4 ... 4, sDesc = Nested fram2: Step {sKey = 7, sTime = 4 ... 4, sDesc = Target tmp_0#0}}"
                    , "Step {sKey = 11, sTime = 4 ... 4, sDesc = Nested fram2: Step {sKey = 8, sTime = 4 ... 4, sDesc = Write 0}}"
                    , "Step {sKey = 10, sTime = 4 ... 4, sDesc = Nested fram2: Step {sKey = 9, sTime = 4 ... 4, sDesc = LoopIn (Loop (X 0.000000) (O [x#0]) (I tmp_0#0)) (I tmp_0#0)}}"
                    , "Step {sKey = 9, sTime = 0 ... 0, sDesc = Nested fram1: Step {sKey = 0, sTime = 0 ... 0, sDesc = bind reg(x#0) = tmp_0#0}}"
                    , "Step {sKey = 8, sTime = 1 ... 1, sDesc = Nested fram1: Step {sKey = 1, sTime = 1 ... 1, sDesc = Target x#0}}"
                    , "Step {sKey = 7, sTime = 1 ... 1, sDesc = Nested fram1: Step {sKey = 2, sTime = 1 ... 1, sDesc = Write 0}}"
                    , "Step {sKey = 6, sTime = 4 ... 4, sDesc = Nested fram1: Step {sKey = 3, sTime = 4 ... 4, sDesc = Source tmp_0#0}}"
                    , "Step {sKey = 5, sTime = 3 ... 3, sDesc = Nested fram1: Step {sKey = 4, sTime = 3 ... 3, sDesc = PrepareRead 0}}"
                    , "Step {sKey = 4, sTime = 1 ... 4, sDesc = Nested fram1: Step {sKey = 5, sTime = 1 ... 4, sDesc = reg(x#0) = tmp_0#0}}"
                    , "Step {sKey = 3, sTime = 4 ... 4, sDesc = Transport \"tmp_0#0\" \"fram1\" \"fram2\"}"
                    , "Step {sKey = 2, sTime = 1 ... 1, sDesc = Transport \"x#0\" \"fram2\" \"fram1\"}"
                    , "Step {sKey = 1, sTime = 0 ... 0, sDesc = bind reg(x#0) = tmp_0#0}"
                    , "Step {sKey = 0, sTime = 0 ... 0, sDesc = bind Loop (X 0.000000) (O [x#0]) (I tmp_0#0)}"
                    ]
                , tbFunctionalSimulationCntx =
                    replicate 2 $
                        HM.fromList
                            [ ("tmp_0#0", 0)
                            , ("u#0", 0)
                            , ("x#0", 0)
                            ]
                , tbLogicalSimulationCntx =
                    replicate 2 $
                        HM.fromList
                            [ ("tmp_0#0", 0)
                            , ("u#0", 0)
                            , ("x#0", 0)
                            ]
                }

-- other

data FView = FView
    { fvFun :: String
    , fvHistory :: [String]
    }
    deriving (Generic, Show)

instance Viewable (F v x) FView where
    view F{fun, funHistory} =
        FView
            { fvFun = S.replace "\"" "" $ show fun
            , fvHistory = map (S.replace "\"" "" . show) funHistory
            }

instance ToJSON FView

data TimeConstrainView = TimeConstrainView
    { vAvailable :: IntervalView
    , vDuration :: IntervalView
    }
    deriving (Generic)

instance (Show t, Bounded t) => Viewable (TimeConstrain t) TimeConstrainView where
    view TimeConstrain{tcAvailable, tcDuration} =
        TimeConstrainView
            { vAvailable = view tcAvailable
            , vDuration = view tcDuration
            }

instance ToJSON TimeConstrainView

newtype IntervalView = IntervalView String
    deriving (Generic)

instance (Show a, Bounded a) => Viewable (Interval a) IntervalView where
    view = IntervalView . S.replace (show (maxBound :: a)) "∞" . show

instance ToJSON IntervalView
