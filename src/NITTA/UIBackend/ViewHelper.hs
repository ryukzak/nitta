{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -fno-warn-orphans #-}

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
    SynthesisNodeView,
    NodeView,
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
import Data.Typeable
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.Problems.ViewHelper
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Model.Types
import NITTA.Project (TestbenchReport (..))
import NITTA.Synthesis.Binding
import NITTA.Synthesis.Dataflow
import NITTA.Synthesis.Explore
import NITTA.Synthesis.Refactor
import NITTA.Synthesis.Types
import NITTA.UIBackend.ViewHelperCls
import Numeric.Interval
import Servant.Docs

type VarValTimeJSON v x t = (Var v, Val x, Time t, ToJSONKey v, ToJSON v, ToJSON x, ToJSON t)

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
                        { svNnid = show $ SID []
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
                                { svNnid = show $ SID [0]
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
                                { svNnid = show $ SID [1]
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
    { svNnid :: String
    , svIsComplete :: Bool
    , svIsEdgesProcessed :: Bool
    , svDuration :: Int
    , svCharacteristic :: Float -- FIXME: Maybe?
    , svOptionType :: String
    }
    deriving (Generic)

instance ToJSON SynthesisNodeView

viewNodeTree tree@Tree{sID = sid, sState = SynthesisState{sTarget}, sDecision, sSubForestVar} = do
    subForestM <- atomically $ tryReadTMVar sSubForestVar
    subForest <- maybe (return []) (mapM viewNodeTree) subForestM
    return
        TreeNodeView
            { rootLabel =
                SynthesisNodeView
                    { svNnid = show sid
                    , svIsComplete = isComplete tree
                    , svIsEdgesProcessed = isJust subForestM
                    , svDuration = fromEnum $ processDuration sTarget
                    , svCharacteristic = read "NaN" -- maybe (read "NaN") eObjectiveFunctionValue nOrigin
                    , svOptionType = case sDecision of
                        Root{} -> "root"
                        SynthesisDecision{metrics}
                            | Just BindMetrics{} <- cast metrics -> "Bind"
                            | Just DataflowMetrics{} <- cast metrics -> "Transport"
                            | Just RefactorMetrics{} <- cast metrics -> "Refactor"
                        _ -> "?"
                    }
            , subForest = subForest
            }

data NodeView tag v x t = NodeView
    { sid :: String
    , -- TODO: naming
      complete :: Bool
    , parameters :: Value
    , decision :: DecisionView
    , objectiveFunctionValue :: Float
    }
    deriving (Generic)

instance (UnitTag tag, VarValTimeJSON v x t) => Viewable (DefTree tag v x t) (NodeView tag v x t) where
    view tree@Tree{sID, sDecision} =
        NodeView
            { sid = show sID
            , complete = isComplete tree
            , decision =
                ( \case
                    SynthesisDecision{decision} -> view decision
                    _ -> RootView
                )
                    sDecision
            , parameters =
                ( \case
                    SynthesisDecision{metrics} -> toJSON metrics
                    _ -> String "root"
                )
                    sDecision
            , objectiveFunctionValue = score sDecision
            }

instance (VarValTimeJSON v x t, ToJSON tag) => ToJSON (NodeView tag v x t)

instance ToSample (NodeView tag v x t) where
    toSamples _ = noSamples

-- Problems

data DataflowEndpointView tag tp = DataflowEndpointView
    { pu :: tag
    , time :: tp
    }
    deriving (Generic)

instance (ToJSON tp, ToJSON tag) => ToJSON (DataflowEndpointView tag tp)

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

data TimeConstrainView = TimeConstrainView
    { vAvailable :: IntervalView
    , vDuration :: IntervalView
    }
    deriving (Generic)

instance (Time t) => Viewable (TimeConstrain t) TimeConstrainView where
    view TimeConstrain{tcAvailable, tcDuration} =
        TimeConstrainView
            { vAvailable = view tcAvailable
            , vDuration = view tcDuration
            }

instance ToJSON TimeConstrainView
