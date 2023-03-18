{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.UIBackend.REST
Description : REST API description for NITTA backend
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.UIBackend.REST (
    SynthesisAPI,
    synthesisServer,
    BackendCtx (..),
    UnitEndpoints (..),
) where

import Control.Monad.Except
import Data.Aeson
import Data.Bifunctor
import Data.Default
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Generics
import NITTA.Intermediate.Simulation
import NITTA.Intermediate.Types
import NITTA.Model.Microarchitecture.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Networks.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Project (Project (..), collectNittaPath, defProjectTemplates, runTestbench, writeProject)
import NITTA.Project.TestBench
import NITTA.Synthesis
import NITTA.Synthesis.Analysis
import NITTA.UIBackend.Timeline
import NITTA.UIBackend.ViewHelper
import NITTA.UIBackend.VisJS (VisJS, algToVizJS)
import NITTA.Utils
import Numeric.Interval.NonEmpty ((...))
import Servant
import Servant.Docs
import System.Directory
import System.FilePath

data BackendCtx tag v x t = BackendCtx
    { root :: DefTree tag v x t
    -- ^ root synthesis node
    , receivedValues :: [(v, [x])]
    -- ^ lists of received by IO values
    , outputPath :: String
    }

type SynthesisAPI tag v x t =
    ( Description "Get whole synthesis tree"
        :> "synthesisTree"
        :> Get '[JSON] (TreeView ShortNodeView)
    )
        :<|> ( Description "Get synthesis tree info"
                :> "treeInfo"
                :> Get '[JSON] TreeInfo
             )
        :<|> ( "node"
                :> Capture "sid" Sid
                :> ( SynthesisTreeNavigationAPI tag v x t
                        :<|> NodeInspectionAPI tag v x t
                        :<|> TestBenchAPI v x
                        :<|> SynthesisMethodsAPI tag v x t
                        :<|> SynthesisPracticesAPI tag v x t
                   )
             )

synthesisServer ctx@BackendCtx{root} =
    liftIO (viewNodeTree root)
        :<|> liftIO (getTreeInfo root)
        :<|> \sid ->
            synthesisTreeNavigation ctx sid
                :<|> nodeInspection ctx sid
                :<|> testBench ctx sid
                :<|> synthesisMethods ctx sid
                :<|> synthesisPractices ctx sid

type SynthesisTreeNavigationAPI tag v x t =
    Summary "Synthesis tree navigation"
        :> ( ( Description "Get list of synthesis nodes from the root to the specific node"
                :> "history"
                :> Get '[JSON] [NodeView tag v x t]
             )
                :<|> ( Description "Get edge to the parent"
                        :> "parentEdge"
                        :> Get '[JSON] (Maybe (NodeView tag v x t))
                     )
                :<|> ( Description "Get sub forest"
                        :> "subForest"
                        :> Get '[JSON] [NodeView tag v x t]
                     )
           )

synthesisTreeNavigation BackendCtx{root} sid =
    liftIO (map view <$> getTreePathIO root sid)
        :<|> liftIO (fmap view . sParent . sState <$> getTreeIO root sid)
        :<|> liftIO (map view <$> (subForestIO =<< getTreeIO root sid))

type NodeInspectionAPI tag v x t =
    Summary "Synthesis node inspection"
        :> ( ( Description "Get node info\n(see: NITTA.Synthesis.Tree.Node)"
                :> Get '[JSON] (NodeView tag v x t)
             )
                :<|> ( Description "Intermidiate reperesentation of the current version of the algorithm"
                        :> "intermediateView"
                        :> Get '[JSON] VisJS
                     )
                -- TODO: Replace by raw process fetching or add typescript types.
                :<|> ( Description "Computational process representation (deprecated)"
                        :> "processTimelines"
                        :> Get '[JSON] (ProcessTimelines t)
                     )
                :<|> ( Description "Process Description for specific process unit"
                        :> "process"
                        :> Get '[JSON] (Process t StepInfoView)
                     )
                :<|> ( Description "Process Description for specific process unit"
                        :> "microarchitecture"
                        :> Capture "tag" tag
                        :> "process"
                        :> Get '[JSON] (Process t StepInfoView)
                     )
                :<|> ( Description "Enpoint options for all process units"
                        :> "endpoints"
                        :> Get '[JSON] [UnitEndpoints tag v t]
                     )
                :<|> ( Description "Microarchitecture description"
                        :> "microarchitecture"
                        :> Get '[JSON] (MicroarchitectureDesc tag)
                     )
                :<|> ("debug" :> DebugAPI tag v t)
           )

nodeInspection ctx@BackendCtx{root} sid =
    liftIO (view <$> getTreeIO root sid)
        :<|> liftIO (algToVizJS . functions . targetDFG <$> getTreeIO root sid)
        :<|> liftIO (processTimelines . process . targetUnit <$> getTreeIO root sid)
        :<|> liftIO (view . process . targetUnit <$> getTreeIO root sid)
        :<|> (\tag -> liftIO (view . process . (M.! tag) . bnPus . targetUnit <$> getTreeIO root sid))
        :<|> liftIO (dbgEndpointOptions <$> debug ctx sid)
        :<|> liftIO (microarchitectureDesc . targetUnit <$> getTreeIO root sid)
        :<|> debug ctx sid

type SynthesisMethodsAPI tag v x t =
    Summary
        "Synthesis methods is a method for full synthesis tree exploration. \
        \Usually, it is more complicated than synthesis practice, but it is \
        \not an essential difference."
        :> ( ( Description "Composition of all available synthesis methods"
                :> "stateOfTheArtSynthesisIO"
                :> Post '[JSON] Sid
             )
                :<|> "simpleSynthesis" :> Post '[JSON] Sid
                :<|> "smartBindSynthesisIO" :> Post '[JSON] Sid
           )

synthesisMethods BackendCtx{root} sid =
    liftIO (sID <$> (stateOfTheArtSynthesisIO =<< getTreeIO root sid))
        :<|> liftIO (sID <$> (simpleSynthesisIO =<< getTreeIO root sid))
        :<|> liftIO (sID <$> (smartBindSynthesisIO =<< getTreeIO root sid))

type SynthesisPracticesAPI tag v x t =
    Summary "SynthesisPractice is a set of small elements of the synthesis process."
        :> ( ( Description "Make the best synthesis step by the objective function"
                :> "bestStep"
                :> Post '[JSON] Sid
             )
                :<|> ( Description "Make all possible oblivious binds"
                        :> "obviousBindThread"
                        :> Post '[JSON] Sid
                     )
                :<|> ( Description "Make all possible binds and refactorings"
                        :> "allBindsAndRefsIO"
                        :> Post '[JSON] Sid
                     )
                :<|> ( Description
                        "Explore all best synthesis threads from current \
                        \and `deep` nested levels."
                        :> "allBestThreads"
                        :> QueryParam' '[Required] "deep" Int
                        :> Post '[JSON] Sid
                     )
           )

synthesisPractices BackendCtx{root} sid =
    liftIO (sID <$> (bestStepIO =<< getTreeIO root sid))
        :<|> liftIO (sID <$> (obviousBindThreadIO =<< getTreeIO root sid))
        :<|> liftIO (sID <$> (allBindsAndRefsIO =<< getTreeIO root sid))
        :<|> (\deep -> liftIO (sID <$> (allBestThreadIO deep =<< getTreeIO root sid)))

type TestBenchAPI v x =
    Summary "Get the report of testbench execution for the current node."
        :> "testbench"
        :> QueryParam' '[Required] "pName" String
        :> QueryParam' '[Required] "loopsNumber" Int
        :> Post '[JSON] (TestbenchReport v x)

testBench BackendCtx{root, receivedValues, outputPath} sid pName loopsNumber = liftIO $ do
    tree <- getTreeIO root sid
    pInProjectNittaPath <- either (error . T.unpack) id <$> collectNittaPath defProjectTemplates
    unless (isComplete tree) $ error "test bench not allow for non complete synthesis"
    pwd <- getCurrentDirectory
    let prj =
            Project
                { pName = T.pack pName
                , pLibPath = "hdl"
                , pTargetProjectPath = outputPath </> pName
                , pAbsTargetProjectPath = pwd </> outputPath </> pName
                , pInProjectNittaPath
                , pAbsNittaPath = pwd </> outputPath </> pInProjectNittaPath
                , pUnit = targetUnit tree
                , pUnitEnv = bnEnv $ targetUnit tree
                , pTestCntx = simulateDataFlowGraph loopsNumber def receivedValues $ targetDFG tree
                , pTemplates = defProjectTemplates
                }
    writeProject prj
    runTestbench prj

-- Helpers

data UnitEndpoints tag v t = UnitEndpoints
    { unitTag :: tag
    , unitEndpoints :: [EndpointSt v (TimeConstraint t)]
    }
    deriving (Generic)
instance (ToJSON tag, ToJSON t, ToJSON v, Time t) => ToJSON (UnitEndpoints tag v t)
instance ToSample (UnitEndpoints String String Int) where
    toSamples _ =
        singleSample
            UnitEndpoints
                { unitTag = "PU1"
                , unitEndpoints =
                    [ EndpointSt{epRole = Target "x", epAt = TimeConstraint (1 ... 10) (1 ... 1)}
                    ]
                }

-- Debug

-- | Type for CAD debugging. Used for extracting internal information.
data Debug tag v t = Debug
    { dbgEndpointOptions :: [UnitEndpoints tag v t]
    , dbgFunctionLocks :: [(T.Text, [Lock v])]
    , dbgCurrentStateFunctionLocks :: [(T.Text, [Lock v])]
    , dbgPULocks :: [(tag, [Lock v])]
    }
    deriving (Generic)

instance (ToJSON tag, ToJSON v, ToJSON t, Time t) => ToJSON (Debug tag v t)

instance ToSample (Debug String String Int) -- where toSamples _ = noSamples
instance ToSample (EndpointSt String (TimeConstraint Int)) where toSamples _ = noSamples

instance ToSample Char where toSamples _ = noSamples

instance (UnitTag tag) => ToSample (Lock tag) where
    toSamples _ = singleSample Lock{locked = "b", lockBy = "a"}

instance {-# OVERLAPS #-} (UnitTag tag) => ToSample [(T.Text, [Lock tag])] where
    toSamples _ = singleSample [("PU or function tag", [Lock{locked = "b", lockBy = "a"}])]

type DebugAPI tag v t =
    Description
        "Debuging interface to fast access to internal state \
        \(see NITTA.UIBackend.REST.Debug)"
        :> Get '[JSON] (Debug tag v t)

debug BackendCtx{root} sid = liftIO $ do
    tree <- getTreeIO root sid
    let dbgFunctionLocks = map (\f -> (f, locks f)) $ functions $ targetUnit tree
        already = transferred $ targetUnit tree
    return
        Debug
            { dbgEndpointOptions = endpointOptions' $ targetUnit tree
            , dbgFunctionLocks = map (first showText) dbgFunctionLocks
            , dbgCurrentStateFunctionLocks =
                [ (showText f, filter (\Lock{lockBy, locked} -> S.notMember lockBy already && S.notMember locked already) ls)
                | (f, ls) <- dbgFunctionLocks
                ]
            , dbgPULocks = map (second locks) $ M.assocs $ bnPus $ targetUnit tree
            }
    where
        endpointOptions' BusNetwork{bnPus} = map (uncurry UnitEndpoints . second endpointOptions) $ M.assocs bnPus

-- API Description

instance ToCapture (Capture "sid" Sid) where
    toCapture _ = DocCapture "nId" "Synthesis node ID (see NITTA.Synthesis.Tree.NId)"

instance ToCapture (Capture "tag" tag) where
    toCapture _ = DocCapture "tag" "Only process unit with specific tag"

instance ToParam (QueryParam' mods "deep" Int) where
    toParam _ =
        DocQueryParam
            "deep"
            ["number"]
            "How many levels need to be explore."
            Normal

instance ToParam (QueryParam' mods "pName" String) where
    toParam _ =
        DocQueryParam
            "pName"
            ["string"]
            "Project name"
            Normal

instance ToParam (QueryParam' mods "loopsNumber" Int) where
    toParam _ = DocQueryParam "loopsNumber" ["number"] "How many computation cycles need to simulate." Normal

instance ToSample Sid where
    toSamples _ = [("The synthesis node path from the root by edge indexes.", Sid [1, 1, 3])]

instance (Time t) => ToSample (Process t StepInfoView) where
    toSamples _ =
        [
            ( "for process unit"
            , Process
                { steps =
                    [ Step{pID = 6, pInterval = 0 ... 5, pDesc = StepInfoView "Intermediate+x_0#0 +1@const#0 = x#0;"}
                    , Step{pID = 5, pInterval = 4 ... 4, pDesc = StepInfoView "InstructionOut"}
                    , Step{pID = 4, pInterval = 5 ... 5, pDesc = StepInfoView "EndpointSource x#0"}
                    , Step{pID = 3, pInterval = 2 ... 2, pDesc = StepInfoView "InstructionLoad False"}
                    , Step{pID = 2, pInterval = 2 ... 2, pDesc = StepInfoView "EndpointTarget 1@const#0"}
                    , Step{pID = 1, pInterval = 1 ... 1, pDesc = StepInfoView "InstructionResetAndLoad False"}
                    , Step{pID = 0, pInterval = 1 ... 1, pDesc = StepInfoView "EndpointTarget x_0#0"}
                    ]
                , relations =
                    [ Vertical 6 4
                    , Vertical 6 2
                    , Vertical 6 0
                    , Vertical 4 5
                    , Vertical 2 3
                    , Vertical 0 1
                    ]
                , nextTick_ = 5
                , nextUid = 7
                }
            )
        ]

instance (UnitTag tag) => ToSample (MicroarchitectureDesc tag) where
    toSamples _ =
        let bn :: BusNetwork tag String (IntX 32) Int = defineNetwork "net1" Sync $ do
                addCustom "fram1" (framWithSize 16) FramIO
                addCustom "fram2" (framWithSize 32) FramIO
                add "shift" ShiftIO
         in singleSample $ microarchitectureDesc bn
