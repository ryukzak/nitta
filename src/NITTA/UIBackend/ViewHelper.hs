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
    module NITTA.UIBackend.ViewHelperCls,
    module NITTA.Model.Problems.ViewHelper,
    FView (..),
    Viewable (..),
    viewNodeTree,
    viewNodeTreeShow,
    TreeView,
    ShortNodeView,
    NodeView,
    StepInfoView (..),
    TestbenchReportView (..),
) where

import Control.Concurrent.STM
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.Problems.ViewHelper
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Model.Types
import NITTA.Project (TestbenchReport (..))
import NITTA.Synthesis
import NITTA.UIBackend.ViewHelperCls
import Numeric.Interval.NonEmpty
import Servant.Docs
import Debug.Trace
import Data.Maybe (catMaybes)

type VarValTimeJSON v x t = (Var v, Val x, Time t, ToJSONKey v, ToJSON v, ToJSON x, ToJSON t)

-- Synthesis tree

data TreeView a = TreeNodeView
    { rootLabel :: a
    , subForest :: [TreeView a]
    }
    deriving (Generic, Show)

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
    deriving (Generic, Show)

data NodeInfo = NodeInfo
    { sid :: String
    , isLeaf :: Bool
    , isProcessed :: Bool
    , duration :: Int
    , score :: Float
    , decsionType :: String
    }
    deriving (Generic, Show)

instance ToJSON ShortNodeView

filterTreeView TreeNodeView {rootLabel=ShortNodeView{isProcessed=False}} = Nothing
filterTreeView tnv@TreeNodeView {rootLabel=ShortNodeView{isProcessed=True}, subForest=[]} = Just tnv
filterTreeView tnv@TreeNodeView {rootLabel=ShortNodeView{isProcessed=True}, subForest} = Just tnv {subForest = catMaybes $ map filterTreeView subForest}

-- synthesisSteps TreeNodeView {rootLabel=ShortNodeView{isProcessed=False}}
-- viewNodeTreeShow tree = do
--     treeRes <- viewNodeTree tree
--     -- let filteredIsProcessed =
--     print $ filterTreeView treeRes
--     return treeRes

viewNodeTreeShow tree@Tree{sID = sid, sState = SynthesisState{sTarget}, sDecision, sSubForestVar}
    | traceShow tree False = undefined
    | otherwise =  do
    subForestM <- atomically $ tryReadTMVar sSubForestVar
    subForest <- maybe (return []) (mapM viewNodeTreeShow) subForestM
    let treeRes = TreeNodeView
            { rootLabel =
                ShortNodeView
                    { sid = show sid
                    , isLeaf = isComplete tree
                    , isProcessed = isJust subForestM
                    , duration = fromEnum $ processDuration sTarget
                    , score = read "NaN" -- maybe (read "NaN") eObjectiveFunctionValue nOrigin
                    , decsionType = case sDecision of
                        Root{} -> "root"
                        SynthesisDecision{metrics, decision}
                            | Just BindMetrics{} <- cast metrics -> ("Bind" <> " " <> show decision)
                            | Just BreakLoopMetrics{} <- cast metrics -> "Refactor"
                            | Just ConstantFoldingMetrics{} <- cast metrics -> "Refactor"
                            | Just DataflowMetrics{} <- cast metrics -> "Transport"
                            | Just OptimizeAccumMetrics{} <- cast metrics -> "Refactor"
                            | Just ResolveDeadlockMetrics{} <- cast metrics -> "Refactor"
                        _ -> "?"
                    }
            , subForest = subForest
            }

    print $ filterTreeView treeRes
    return treeRes

viewNodeTree tree@Tree{sID = sid, sState = SynthesisState{sTarget}, sDecision, sSubForestVar} = do
    subForestM <- atomically $ tryReadTMVar sSubForestVar
    subForest <- maybe (return []) (mapM viewNodeTree) subForestM
    return TreeNodeView
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
                            | Just BreakLoopMetrics{} <- cast metrics -> "Refactor"
                            | Just ConstantFoldingMetrics{} <- cast metrics -> "Refactor"
                            | Just DataflowMetrics{} <- cast metrics -> "Transport"
                            | Just OptimizeAccumMetrics{} <- cast metrics -> "Refactor"
                            | Just ResolveDeadlockMetrics{} <- cast metrics -> "Refactor"
                        _ -> "?"
                    }
            , subForest = subForest
            }


data NodeView tag v x t = NodeView
    { sid :: String
    , isLeaf :: Bool
    , duration :: Int
    , parameters :: Value
    , decision :: DecisionView
    , score :: Float
    }
    deriving (Generic)

instance (UnitTag tag, VarValTimeJSON v x t) => Viewable (DefTree tag v x t) (NodeView tag v x t) where
    view tree@Tree{sID, sDecision, sState = SynthesisState{sTarget}} =
        NodeView
            { sid = show sID
            , isLeaf = isComplete tree
            , duration = fromEnum $ processDuration sTarget
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
                , duration = 0
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
                , decision = BindDecisionView (FView "buffer(a) = b = c" []) "pu"
                , score = 1032
                }
            , NodeView
                { sid = show $ SID [0, 1, 3, 1, 5]
                , isLeaf = False
                , duration = 0
                , parameters =
                    toJSON $
                        DataflowMetrics
                            { pWaitTime = 1
                            , pRestrictedTime = False
                            , pNotTransferableInputs = [0, 0]
                            }
                , decision =
                    DataflowDecisionView
                        { source = ("PU1", EndpointSt{epRole = Source $ S.fromList ["a1", "a2"], epAt = 1 ... 1})
                        , targets =
                            [("PU2", EndpointSt{epRole = Target "a2", epAt = 1 ... 1})]
                        }
                , score = 1999
                }
            , NodeView
                { sid = show $ SID [0, 1, 3, 1, 6]
                , isLeaf = False
                , duration = 0
                , parameters = toJSON BreakLoopMetrics
                , decision = BreakLoopView{value = "12.5", outputs = ["a", "b"], input = "c"}
                , score = 5000
                }
            , NodeView
                { sid = show $ SID [0, 1, 3, 1, 5]
                , isLeaf = False
                , duration = 0
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
                , duration = 0
                , parameters = toJSON ConstantFoldingMetrics
                , decision =
                    ConstantFoldingView
                        { cRefOld = [FView "a = 1" [], FView "b = 2" [], FView "a + b = r" []]
                        , cRefNew = [FView "r = 3" []]
                        }
                , score = 1999
                }
            , NodeView
                { sid = show $ SID [0, 1, 3, 1, 5]
                , isLeaf = False
                , duration = 0
                , parameters =
                    toJSON $
                        ResolveDeadlockMetrics
                            { pNumberOfLockedVariables = 1
                            , pBufferCount = 0
                            , pNumberOfTransferableVariables = 0
                            }
                , decision =
                    ResolveDeadlockView
                        { newBuffer = "buffer(x#0@buf) = x#0"
                        , changeset = "Changeset {changeI = fromList [], changeO = fromList [(\"x#0\",fromList [\"x#0@buf\"])]}"
                        }
                , score = 1999
                }
            ]

newtype StepInfoView = StepInfoView String
    deriving (Generic)

instance (Show t, Show v) => Viewable (StepInfo v x t) StepInfoView where
    view = StepInfoView . show

instance ToJSON StepInfoView

instance (Show t, Show v) => Viewable (Process t (StepInfo v x t)) (Process t StepInfoView) where
    view p@Process{steps} = p{steps = map (\s@Step{pDesc} -> s{pDesc = view pDesc}) steps}

-- Testbench

data TestbenchReportView v x = TestbenchReportView
    { tbStatus :: Bool
    , tbPath :: String
    , tbFiles :: [String]
    , tbFunctions :: [T.Text]
    , tbSynthesisSteps :: [T.Text]
    , tbCompilerDump :: T.Text
    , tbSimulationDump :: T.Text
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
                , tbCompilerDump = "stdout:\n" <> "stderr:\n"
                , tbSimulationDump =
                    T.unlines
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
                    [ "buffer(x#0) = tmp_0#0"
                    , "LoopEnd (Loop (X 0.000000) (O [x#0]) (I tmp_0#0)) (I tmp_0#0)"
                    , "LoopBegin (Loop (X 0.000000) (O [x#0]) (I tmp_0#0)) (O [x#0])"
                    ]
                , tbSynthesisSteps =
                    [ "Step {pID = 19, pInterval = 0 ... 0, pDesc = Nested fram2: Step {pID = 0, pInterval = 0 ... 0, pDesc = bind Loop (X 0.000000) (O [x#0]) (I tmp_0#0)}}"
                    , "Step {pID = 18, pInterval = 0 ... 0, pDesc = Nested fram2: Step {pID = 1, pInterval = 0 ... 0, pDesc = revoke Loop (X 0.000000) (O [x#0]) (I tmp_0#0)}}"
                    , "Step {pID = 17, pInterval = 0 ... 0, pDesc = Nested fram2: Step {pID = 2, pInterval = 0 ... 0, pDesc = bind LoopBegin (Loop (X 0.000000) (O [x#0]) (I tmp_0#0)) (O [x#0])}}"
                    , "Step {pID = 16, pInterval = 0 ... 0, pDesc = Nested fram2: Step {pID = 3, pInterval = 0 ... 0, pDesc = bind LoopEnd (Loop (X 0.000000) (O [x#0]) (I tmp_0#0)) (I tmp_0#0)}}"
                    , "Step {pID = 15, pInterval = 1 ... 1, pDesc = Nested fram2: Step {pID = 4, pInterval = 1 ... 1, pDesc = Source x#0}}"
                    , "Step {pID = 14, pInterval = 0 ... 0, pDesc = Nested fram2: Step {pID = 5, pInterval = 0 ... 0, pDesc = PrepareRead 0}}"
                    , "Step {pID = 13, pInterval = 0 ... 1, pDesc = Nested fram2: Step {pID = 6, pInterval = 0 ... 1, pDesc = LoopBegin (Loop (X 0.000000) (O [x#0]) (I tmp_0#0)) (O [x#0])}}"
                    , "Step {pID = 12, pInterval = 4 ... 4, pDesc = Nested fram2: Step {pID = 7, pInterval = 4 ... 4, pDesc = Target tmp_0#0}}"
                    , "Step {pID = 11, pInterval = 4 ... 4, pDesc = Nested fram2: Step {pID = 8, pInterval = 4 ... 4, pDesc = Write 0}}"
                    , "Step {pID = 10, pInterval = 4 ... 4, pDesc = Nested fram2: Step {pID = 9, pInterval = 4 ... 4, pDesc = LoopEnd (Loop (X 0.000000) (O [x#0]) (I tmp_0#0)) (I tmp_0#0)}}"
                    , "Step {pID = 9, pInterval = 0 ... 0, pDesc = Nested fram1: Step {pID = 0, pInterval = 0 ... 0, pDesc = bind buffer(x#0) = tmp_0#0}}"
                    , "Step {pID = 8, pInterval = 1 ... 1, pDesc = Nested fram1: Step {pID = 1, pInterval = 1 ... 1, pDesc = Target x#0}}"
                    , "Step {pID = 7, pInterval = 1 ... 1, pDesc = Nested fram1: Step {pID = 2, pInterval = 1 ... 1, pDesc = Write 0}}"
                    , "Step {pID = 6, pInterval = 4 ... 4, pDesc = Nested fram1: Step {pID = 3, pInterval = 4 ... 4, pDesc = Source tmp_0#0}}"
                    , "Step {pID = 5, pInterval = 3 ... 3, pDesc = Nested fram1: Step {pID = 4, pInterval = 3 ... 3, pDesc = PrepareRead 0}}"
                    , "Step {pID = 4, pInterval = 1 ... 4, pDesc = Nested fram1: Step {pID = 5, pInterval = 1 ... 4, pDesc = buffer(x#0) = tmp_0#0}}"
                    , "Step {pID = 3, pInterval = 4 ... 4, pDesc = Transport \"tmp_0#0\" \"fram1\" \"fram2\"}"
                    , "Step {pID = 2, pInterval = 1 ... 1, pDesc = Transport \"x#0\" \"fram2\" \"fram1\"}"
                    , "Step {pID = 1, pInterval = 0 ... 0, pDesc = bind reg(x#0) = tmp_0#0}"
                    , "Step {pID = 0, pInterval = 0 ... 0, pDesc = bind Loop (X 0.000000) (O [x#0]) (I tmp_0#0)}"
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
