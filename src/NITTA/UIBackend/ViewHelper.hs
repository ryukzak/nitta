{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
    ShortNodeView,
    NodeView,
    TestbenchReportView (..),
    FView,
) where

import Control.Concurrent.STM
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Typeable
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Problems.ViewHelper
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Model.Types
import NITTA.Project (TestbenchReport (..))
import NITTA.Synthesis
import NITTA.UIBackend.ViewHelperCls
import Numeric.Interval.NonEmpty
import Servant.Docs

type VarValTimeJSON v x t = (Var v, Val x, Time t, ToJSONKey v, ToJSON v, ToJSON x, ToJSON t)

-- Synthesis tree

data TreeView a = TreeNodeView
    { rootLabel :: a
    , subForest :: [TreeView a]
    }
    deriving (Generic)

instance (ToJSON a) => ToJSON (TreeView a)

instance ToSample (TreeView ShortNodeView) where
    toSamples _ =
        singleSample $
            TreeNodeView
                { rootLabel =
                    ShortNodeView
                        { sid = show $ SID []
                        , isLeaf = False
                        , isProcessed = True
                        , duration = 0
                        , score = 0 / 0
                        , decsionType = "-"
                        }
                , subForest =
                    [ TreeNodeView
                        { rootLabel =
                            ShortNodeView
                                { sid = show $ SID [0]
                                , isLeaf = False
                                , isProcessed = False
                                , duration = 0
                                , score = 4052
                                , decsionType = "Bind"
                                }
                        , subForest = []
                        }
                    , TreeNodeView
                        { rootLabel =
                            ShortNodeView
                                { sid = show $ SID [1]
                                , isLeaf = False
                                , isProcessed = False
                                , duration = 0
                                , score = 3021
                                , decsionType = "Bind"
                                }
                        , subForest = []
                        }
                    ]
                }

data ShortNodeView = ShortNodeView
    { sid :: String
    , isLeaf :: Bool
    , isProcessed :: Bool
    , duration :: Int
    , score :: Float
    , decsionType :: String
    }
    deriving (Generic)

instance ToJSON ShortNodeView

viewNodeTree tree@Tree{sID = sid, sState = SynthesisState{sTarget}, sDecision, sSubForestVar} = do
    subForestM <- atomically $ tryReadTMVar sSubForestVar
    subForest <- maybe (return []) (mapM viewNodeTree) subForestM
    return
        TreeNodeView
            { rootLabel =
                ShortNodeView
                    { sid = show sid
                    , isLeaf = isComplete tree
                    , isProcessed = isJust subForestM
                    , duration = fromEnum $ processDuration sTarget
                    , score = read "NaN" -- maybe (read "NaN") eObjectiveFunctionValue nOrigin
                    , decsionType = case sDecision of
                        Root{} -> "root"
                        SynthesisDecision{metrics}
                            | Just BindMetrics{} <- cast metrics -> "Bind"
                            | Just DataflowMetrics{} <- cast metrics -> "Transport"
                            | Just BreakLoopMetrics{} <- cast metrics -> "Refactor"
                            | Just OptimizeAccumMetrics{} <- cast metrics -> "Refactor"
                            | Just ResolveDeadlockMetrics{} <- cast metrics -> "Refactor"
                        _ -> "?"
                    }
            , subForest = subForest
            }

data NodeView tag v x t = NodeView
    { sid :: String
    , isLeaf :: Bool
    , parameters :: Value
    , decision :: DecisionView
    , score :: Float
    }
    deriving (Generic)

instance (UnitTag tag, VarValTimeJSON v x t) => Viewable (DefTree tag v x t) (NodeView tag v x t) where
    view tree@Tree{sID, sDecision} =
        NodeView
            { sid = show sID
            , isLeaf = isComplete tree
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
            , score =
                ( \case
                    SynthesisDecision{score} -> score
                    _ -> 0
                )
                    sDecision
            }

instance (VarValTimeJSON v x t, ToJSON tag) => ToJSON (NodeView tag v x t)

instance ToSample (NodeView tag v x t) where
    toSamples _ =
        samples
            [ NodeView
                { sid = show $ SID [0, 1, 3, 1]
                , isLeaf = False
                , parameters =
                    toJSON $
                        BindMetrics
                            { pCritical = False
                            , pAlternative = 1
                            , pRestless = 0
                            , pOutputNumber = 2
                            , pAllowDataFlow = 1
                            , pPossibleDeadlock = False
                            , pNumberOfBindedFunctions = 1
                            , pPercentOfBindedInputs = 0.2
                            , pWave = Just 2
                            }
                , decision = BindDecisionView (FView "reg(a) = b = c" []) "pu"
                , score = 1032
                }
            , NodeView
                { sid = show $ SID [0, 1, 3, 1, 5]
                , isLeaf = False
                , parameters =
                    toJSON $
                        DataflowMetrics
                            { pWaitTime = 1
                            , pRestrictedTime = False
                            , pNotTransferableInputs = [0, 0]
                            }
                , decision =
                    DataflowDecisionView
                        { source = "PU1"
                        , targets =
                            HM.fromList
                                [ ("a1", Nothing)
                                , ("a2", Just ("PU2", 1 ... 1))
                                ]
                        }
                , score = 1999
                }
            , NodeView
                { sid = show $ SID [0, 1, 3, 1, 6]
                , isLeaf = False
                , parameters = toJSON BreakLoopMetrics
                , decision = BreakLoopView{value = "12.5", outputs = ["a", "b"], input = "c"}
                , score = 5000
                }
            , NodeView
                { sid = show $ SID [0, 1, 3, 1, 5]
                , isLeaf = False
                , parameters = toJSON OptimizeAccumMetrics
                , decision =
                    OptimizeAccumView
                        { old = [FView "a + b = c" [], FView "c + d = e" []]
                        , new = [FView "a + b + d = e" []]
                        }
                , score = 1999
                }
            , NodeView
                { sid = show $ SID [0, 1, 3, 1, 5]
                , isLeaf = False
                , parameters =
                    toJSON $
                        ResolveDeadlockMetrics
                            { pNumberOfLockedVariables = 1
                            , pBufferCount = 0
                            , pNumberOfTransferableVariables = 0
                            }
                , decision =
                    ResolveDeadlockView
                        { buffer = "reg(x#0@buf) = x#0"
                        , changeset = "Changeset {changeI = fromList [], changeO = fromList [(\"x#0\",fromList [\"x#0@buf\"])]}"
                        }
                , score = 1999
                }
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
                    , "lib/div/div_mock.v"
                    , "lib/div/pu_div.v"
                    , "lib/i2c/bounce_filter.v"
                    , "lib/i2c/buffer.v"
                    , "lib/multiplier/mult_mock.v"
                    , "lib/multiplier/pu_multiplier.v"
                    , "lib/spi/pu_slave_spi_driver.v"
                    , "lib/spi/spi_slave_driver.v"
                    , "lib/spi/i2n_splitter.v"
                    , "lib/spi/spi_master_driver.v"
                    , "lib/spi/n2i_splitter.v"
                    , "lib/spi/pu_slave_spi.v"
                    , "lib/spi/pu_master_spi.v"
                    , "lib/pu_accum.v"
                    , "lib/pu_fram.v"
                    , "lib/pu_shift.v"
                    , "lib/pu_simple_control.v"
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
