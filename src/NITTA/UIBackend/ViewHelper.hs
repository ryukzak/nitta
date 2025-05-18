{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

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
    TreeView,
    ShortNodeView,
    NodeView,
    StepInfoView (..),
    VarValTimeJSON,
) where

import Control.Concurrent.STM
import Data.Aeson
import Data.HashMap.Strict qualified as HM
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Typeable
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.Problems.ViewHelper
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Project.TestBench
import NITTA.Synthesis.Analysis
import NITTA.Synthesis.Steps
import NITTA.Synthesis.Types
import NITTA.UIBackend.ViewHelperCls
import NITTA.Utils.Base
import Numeric.Interval.NonEmpty
import Servant.Docs

-- Synthesis tree

data TreeView a = TreeNodeView
    { rootLabel :: a
    , subForest :: [TreeView a]
    }
    deriving (Generic, Show)

instance ToJSON a => ToJSON (TreeView a)

instance ToSample (TreeView ShortNodeView) where
    toSamples _ =
        singleSample $
            TreeNodeView
                { rootLabel =
                    ShortNodeView
                        { sid = showText $ Sid []
                        , isTerminal = False
                        , isFinish = False
                        , isProcessed = True
                        , duration = 0
                        , score = 0 / 0
                        , decsionType = "-"
                        }
                , subForest =
                    [ TreeNodeView
                        { rootLabel =
                            ShortNodeView
                                { sid = showText $ Sid [0]
                                , isTerminal = False
                                , isFinish = False
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
                                { sid = showText $ Sid [1]
                                , isTerminal = False
                                , isFinish = False
                                , isProcessed = False
                                , duration = 0
                                , score = 3021
                                , decsionType = "Bind"
                                }
                        , subForest = []
                        }
                    ]
                }

instance ToSample Integer where
    toSamples _ =
        singleSample 0

data ShortNodeView = ShortNodeView
    { sid :: T.Text
    , isTerminal :: Bool
    , isFinish :: Bool
    , isProcessed :: Bool
    , duration :: Int
    , score :: Float
    , decsionType :: T.Text
    }
    deriving (Generic, Show)

data NodeInfo = NodeInfo
    { sid :: String
    , isTerminal :: Bool
    , isProcessed :: Bool
    , duration :: Int
    , score :: Float
    , decsionType :: String
    }
    deriving (Generic, Show)

instance ToJSON ShortNodeView
instance ToJSON TreeInfo

instance ToSample TreeInfo where
    toSamples _ =
        singleSample mempty

viewNodeTree tree@Tree{sID = sid, sDecision, sSubForestVar} = do
    subForestM <- atomically $ tryReadTMVar sSubForestVar
    subForest <- maybe (return []) (mapM viewNodeTree) subForestM
    return
        TreeNodeView
            { rootLabel =
                ShortNodeView
                    { sid = showText sid
                    , isTerminal = isLeaf tree
                    , isFinish = isComplete tree
                    , isProcessed = isJust subForestM
                    , duration = (fromEnum . processDuration . sTarget . sState) tree
                    , score = read "NaN" -- maybe (read "NaN") eObjectiveFunctionValue nOrigin
                    , decsionType = case sDecision of
                        Root{} -> "root"
                        SynthesisDecision{metrics}
                            | Just AllocationMetrics{} <- cast metrics -> "Allocation"
                            | Just SingleBindMetrics{} <- cast metrics -> "SingleBind"
                            | Just GroupBindMetrics{} <- cast metrics -> "GroupBind"
                            | Just BreakLoopMetrics{} <- cast metrics -> "Refactor"
                            | Just ConstantFoldingMetrics{} <- cast metrics -> "Refactor"
                            | Just DataflowMetrics{} <- cast metrics -> "Transport"
                            | Just OptimizeAccumMetrics{} <- cast metrics -> "Refactor"
                            | Just OptimizeLutMetrics{} <- cast metrics -> "Refactor"
                            | Just ResolveDeadlockMetrics{} <- cast metrics -> "Refactor"
                        _ -> "?"
                    }
            , subForest = subForest
            }

data NodeView tag v x t = NodeView
    { sid :: T.Text
    , isTerminal :: Bool
    , isFinish :: Bool
    , duration :: Int
    , parameters :: Value
    , decision :: DecisionView
    , score :: Float
    , scores :: Value
    }
    deriving (Generic)

instance (UnitTag tag, VarValTimeJSON v x t) => Viewable (DefTree tag v x t) (NodeView tag v x t) where
    view tree@Tree{sID, sDecision} =
        NodeView
            { sid = showText sID
            , isTerminal = isLeaf tree
            , isFinish = isComplete tree
            , duration = fromEnum $ processDuration $ sTarget $ sState tree
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
                    -- TODO: add support for "scores" field in UI, remove that field (or rename to default_score/effective_score?)
                    sd@SynthesisDecision{} -> defScore sd
                    _ -> 0
                )
                    sDecision
            , scores =
                ( \case
                    SynthesisDecision{scores} -> toJSON scores
                    _ -> object ["default" .= (0 :: Float)]
                )
                    sDecision
            }

instance (VarValTimeJSON v x t, ToJSON tag) => ToJSON (NodeView tag v x t)

instance ToSample (NodeView tag v x t) where
    toSamples _ =
        samples
            [ NodeView
                { sid = showText $ Sid [0, 1, 3, 1]
                , isTerminal = False
                , isFinish = False
                , duration = 0
                , parameters =
                    toJSON $
                        SingleBindMetrics
                            { pCritical = False
                            , pAlternative = 1
                            , pRestless = 0
                            , pOutputNumber = 2
                            , pAllowDataFlow = 1
                            , pPossibleDeadlock = False
                            , pNumberOfBoundFunctions = 1
                            , pPercentOfBoundInputs = 0.2
                            , pWave = Just 2
                            }
                , decision = SingleBindView (FView "buffer(a) = b = c" []) "pu"
                , score = 1032
                , scores = object ["default" .= (1032 :: Float)]
                }
            , NodeView
                { sid = showText $ Sid [0, 1, 3, 1, 5]
                , isTerminal = False
                , isFinish = False
                , duration = 0
                , parameters =
                    toJSON $
                        DataflowMetrics
                            { pWaitTime = 1
                            , pRestrictedTime = False
                            , pNotTransferableInputs = [0, 0]
                            , pFirstWaveOfTargetUse = 0
                            }
                , decision =
                    DataflowDecisionView
                        { source = ("PU1", EndpointSt{epRole = Source $ S.fromList ["a1", "a2"], epAt = 1 ... 1})
                        , targets =
                            [("PU2", EndpointSt{epRole = Target "a2", epAt = 1 ... 1})]
                        }
                , score = 1999
                , scores = object ["default" .= (1999 :: Float)]
                }
            , NodeView
                { sid = showText $ Sid [0, 1, 3, 1, 6]
                , isTerminal = False
                , isFinish = False
                , duration = 0
                , parameters = toJSON BreakLoopMetrics
                , decision = BreakLoopView{value = "12.5", outputs = ["a", "b"], input = "c"}
                , score = 5000
                , scores = object ["default" .= (5000 :: Float)]
                }
            , NodeView
                { sid = showText $ Sid [0, 1, 3, 1, 5]
                , isTerminal = False
                , isFinish = False
                , duration = 0
                , parameters = toJSON OptimizeAccumMetrics
                , decision =
                    OptimizeAccumView
                        { old = [FView "a + b = c" [], FView "c + d = e" []]
                        , new = [FView "a + b + d = e" []]
                        }
                , score = 1999
                , scores = object ["default" .= (1999 :: Float)]
                }
            , NodeView
                { sid = showText $ Sid [0, 1, 3, 1, 5]
                , isTerminal = False
                , isFinish = False
                , duration = 0
                , parameters = toJSON OptimizeLutMetrics
                , decision =
                    OptimizeLutView
                        { lOld = [FView "a and b = c" [], FView "d = not c" []]
                        , lNew = [FView "LUT" []]
                        }
                , score = 1999
                , scores = object ["default" .= (1999 :: Float)]
                }
            , NodeView
                { sid = showText $ Sid [0, 1, 3, 1, 5]
                , isTerminal = False
                , isFinish = False
                , duration = 0
                , parameters = toJSON ConstantFoldingMetrics
                , decision =
                    ConstantFoldingView
                        { cRefOld = [FView "a = 1" [], FView "b = 2" [], FView "a + b = r" []]
                        , cRefNew = [FView "r = 3" []]
                        }
                , score = 1999
                , scores = object ["default" .= (1999 :: Float)]
                }
            , NodeView
                { sid = showText $ Sid [0, 1, 3, 1, 5]
                , isTerminal = False
                , isFinish = False
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
                , scores = object ["default" .= (1999 :: Float)]
                }
            ]

newtype StepInfoView = StepInfoView T.Text
    deriving (Generic)

instance (Var v, Time t) => Viewable (StepInfo v x t) StepInfoView where
    view = StepInfoView . showText

instance ToJSON StepInfoView

instance (Var v, Time t) => Viewable (Process t (StepInfo v x t)) (Process t StepInfoView) where
    view p@Process{steps} = p{steps = map (\s@Step{pDesc} -> s{pDesc = view pDesc}) steps}

-- Testbench

instance (ToJSONKey v, ToJSON v, ToJSON x) => ToJSON (TestbenchReport v x)

instance ToSample (TestbenchReport String Int) where
    toSamples _ =
        singleSample
            TestbenchReport
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
                , tbFunctionalSimulationLog =
                    replicate 2 $
                        HM.fromList
                            [ ("tmp_0#0", 0)
                            , ("u#0", 0)
                            , ("x#0", 0)
                            ]
                , tbLogicalSimulationLog =
                    replicate 2 $
                        HM.fromList
                            [ ("tmp_0#0", 0)
                            , ("u#0", 0)
                            , ("x#0", 0)
                            ]
                }
