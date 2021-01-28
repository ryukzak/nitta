{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
) where

import Control.Monad.Except
import Data.Aeson
import Data.Bifunctor
import Data.Default
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import GHC.Generics
import NITTA.Intermediate.Simulation
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Model.Types
import NITTA.Project (Project (..), writeAndRunTestbench)
import NITTA.Synthesis
import NITTA.UIBackend.Timeline
import NITTA.UIBackend.ViewHelper
import NITTA.UIBackend.VisJS (VisJS, algToVizJS)
import NITTA.Utils
import Numeric.Interval.NonEmpty ((...))
import Servant
import Servant.Docs
import System.FilePath (joinPath)

data BackendCtx tag v x t = BackendCtx
    { -- |root synthesis node
      root :: DefTree tag v x t
    , -- |lists of received by IO values
      receivedValues :: [(v, [x])]
    , outputPath :: String
    }

type SynthesisAPI tag v x t =
    ( Description "Get whole synthesis tree"
        :> "synthesisTree"
        :> Get '[JSON] (TreeView ShortNodeView)
    )
        :<|> ( "node" :> Capture "sid" SID
                :> ( SynthesisTreeNavigationAPI tag v x t
                        :<|> NodeInspectionAPI tag v x t
                        :<|> TestBenchAPI v x
                        :<|> SynthesisMethodsAPI tag v x t
                        :<|> SynthesisPracticesAPI tag v x t
                   )
             )

synthesisServer ctx@BackendCtx{root} =
    liftIO (viewNodeTree root)
        :<|> \sid ->
            synthesisTreeNavigation ctx sid
                :<|> nodeInspection ctx sid
                :<|> testBench ctx sid
                :<|> synthesisMethods ctx sid
                :<|> synthesisPractices ctx sid

type SynthesisTreeNavigationAPI tag v x t =
    Summary "Synthesis tree navigation"
        :> ( ( Description "Get list of synthesis nodes from the root to the specific node"
                :> "rootPath"
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
                :<|> ( Description "Computational process representation (depricated)"
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
                        :> Get '[JSON] [(tag, [EndpointSt v (TimeConstrain t)])]
                     )
                :<|> ("debug" :> DebugAPI tag v t)
           )

nodeInspection ctx@BackendCtx{root} sid =
    liftIO (view <$> getTreeIO root sid)
        :<|> liftIO (algToVizJS . functions . targetDFG <$> getTreeIO root sid)
        :<|> liftIO (processTimelines . process . targetModel <$> getTreeIO root sid)
        :<|> liftIO (view . process . targetModel <$> getTreeIO root sid)
        :<|> (\tag -> liftIO (view . process . (M.! tag) . bnPus . targetModel <$> getTreeIO root sid))
        :<|> liftIO (dbgEndpointOptions <$> debug ctx sid)
        :<|> debug ctx sid

type SynthesisMethodsAPI tag v x t =
    Summary
        "Synthesis methods is a method for full synthesis tree exploration. \
        \Usually, it is more complicated than synthesis practice, but it is \
        \not an essential difference."
        :> ( ( Description "Composition of all available synthesis methods"
                :> "stateOfTheArtSynthesisIO"
                :> Post '[JSON] SID
             )
                :<|> "simpleSynthesis" :> Post '[JSON] SID
                :<|> "smartBindSynthesisIO" :> Post '[JSON] SID
           )

synthesisMethods BackendCtx{root} sid =
    liftIO (sID <$> (stateOfTheArtSynthesisIO =<< getTreeIO root sid))
        :<|> liftIO (sID <$> (simpleSynthesisIO =<< getTreeIO root sid))
        :<|> liftIO (sID <$> (smartBindSynthesisIO =<< getTreeIO root sid))

type SynthesisPracticesAPI tag v x t =
    Summary "SynthesisPractice is a set of small elements of the synthesis process."
        :> ( ( Description "Make the best synthesis step by the objective function"
                :> "bestStep"
                :> Post '[JSON] SID
             )
                :<|> ( Description "Make all possible oblivious binds"
                        :> "obviousBindThread"
                        :> Post '[JSON] SID
                     )
                :<|> ( Description "Make all possible binds and refactorings"
                        :> "allBindsAndRefsIO"
                        :> Post '[JSON] SID
                     )
                :<|> ( Description
                        "Explore all best synthesis threads from current \
                        \and `deep` nested levels."
                        :> "allBestThreads"
                        :> QueryParam' '[Required] "deep" Int
                        :> Post '[JSON] SID
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
        :> Post '[JSON] (TestbenchReportView v x)

testBench BackendCtx{root, receivedValues, outputPath} sid pName loopsNumber = liftIO $ do
    tree <- getTreeIO root sid
    unless (isComplete tree) $ error "test bench not allow for non complete synthesis"
    view
        <$> writeAndRunTestbench
            Project
                { pName
                , pLibPath = "hdl"
                , pPath = joinPath [outputPath, pName]
                , pUnit = mUnit $ sTarget $ sState tree
                , pTestCntx = simulateDataFlowGraph loopsNumber def receivedValues $ targetDFG tree
                }

-- Debug

-- |Type for CAD debugging. Used for extracting internal information.
data Debug tag v t = Debug
    { dbgEndpointOptions :: [(tag, [EndpointSt v (TimeConstrain t)])]
    , dbgFunctionLocks :: [(String, [Lock v])]
    , dbgCurrentStateFunctionLocks :: [(String, [Lock v])]
    , dbgPULocks :: [(String, [Lock v])]
    }
    deriving (Generic)

instance (ToJSON tag, ToJSON t, Time t) => ToJSON (Debug tag String t)

instance ToSample (Debug String String Int)
instance ToSample (EndpointSt [Char] (TimeConstrain Int)) where toSamples _ = noSamples

instance ToSample Char where toSamples _ = noSamples

instance {-# OVERLAPS #-} ToSample [(String, [Lock String])] where
    toSamples _ = singleSample [("PU or function tag", [Lock{locked = "b", lockBy = "a"}])]

type DebugAPI tag v t =
    Description
        "Debuging interface to fast access to internal state \
        \(see NITTA.UIBackend.REST.Debug)"
        :> Get '[JSON] (Debug tag v t)

debug BackendCtx{root} sid = liftIO $ do
    tree <- getTreeIO root sid
    let dbgFunctionLocks = map (\f -> (show f, locks f)) $ functions $ targetModel tree
        already = transferred $ targetModel tree
    return
        Debug
            { dbgEndpointOptions = endpointOptions' $ targetModel tree
            , dbgFunctionLocks
            , dbgCurrentStateFunctionLocks =
                [ (tag, filter (\Lock{lockBy, locked} -> S.notMember lockBy already && S.notMember locked already) ls)
                | (tag, ls) <- dbgFunctionLocks
                ]
            , dbgPULocks = map (second locks) $ M.assocs $ bnPus $ targetModel tree
            }
    where
        endpointOptions' BusNetwork{bnPus} = map (second endpointOptions) $ M.assocs bnPus

-- API Description

instance ToCapture (Capture "sid" SID) where
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

instance ToSample SID where
    toSamples _ = [("The synthesis node path from the root by edge indexes.", SID [1, 1, 3])]

instance (Time t) => ToSample (Process t StepInfoView) where
    toSamples _ =
        [
            ( "for process unit"
            , Process
                { steps =
                    [ Step{sKey = 6, sTime = 0 ... 5, sDesc = StepInfoView "Intermediate+x_0#0 +1@const#0 = x#0;"}
                    , Step{sKey = 5, sTime = 4 ... 4, sDesc = StepInfoView "InstructionOut"}
                    , Step{sKey = 4, sTime = 5 ... 5, sDesc = StepInfoView "EndpointSource x#0"}
                    , Step{sKey = 3, sTime = 2 ... 2, sDesc = StepInfoView "InstructionLoad False"}
                    , Step{sKey = 2, sTime = 2 ... 2, sDesc = StepInfoView "EndpointTarget 1@const#0"}
                    , Step{sKey = 1, sTime = 1 ... 1, sDesc = StepInfoView "InstructionResetAndLoad False"}
                    , Step{sKey = 0, sTime = 1 ... 1, sDesc = StepInfoView "EndpointTarget x_0#0"}
                    ]
                , relations =
                    [ Vertical 6 4
                    , Vertical 6 2
                    , Vertical 6 0
                    , Vertical 4 5
                    , Vertical 2 3
                    , Vertical 0 1
                    ]
                , nextTick = 5
                , nextUid = 7
                }
            )
        ]
