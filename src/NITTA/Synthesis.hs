{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      : NITTA.Synthesis
Description : Entry point for synthesis process and target system generation.
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

TargetSynthesis is an entry point for synthesis process. TargetSynthesis flow shown on fig.

@
====================================================================================================================
                                                                                                             Prepare
NITTA.Synthesis:TargetSynthesis                                                                     NITTA.Project...
    # tName                                                                                        NITTA.LuaFrontend
    # tMicroArch --------------------------\
    # tSourceCode ----+                    |     /--+-- mkModelWithOneNetwork
                      |                    |     |  |
                      *<--lua2functions    |     |  |
                      |                    |     |  v         NITTA.Model:TargetSystem----------\
    # tDFG <----------+                    +--------*--------> # mUnit            |             |    NITTA.Model...
        |                                        |                                |             |     /-----------\
        |                                        v                                |             |     |  Target   |
        +----------------------------------------*-----------> # mDataFlowGraph   |             \-----+  System   |
                                                                                  |                   | Imitation |
    # tReceivedValues                                                             |                   |   Model   |
    # tVerbose                                                                    |                   \-----------/
    # tSynthesisMethod ----------------------------------\                        |
                                                         |                        |
                                                         |                        |
===================================================================================================================
                                                         |                        |               Synthesis process
        NITTA.Synthesis.Types:Tree                       |                        |           NITTA.Synthesis.Types
            # sID                                        |                        |          NITTA.Synthesis.Method
            # sState <------------------------------------------------------------+
            # sDecision                                  |
                # option                                 |
                # decision                               *<-- search for best synthesis path
                # metrics                                |
                # score                                  v
            # sSubForestVar ------------------------->// * // -----\
                                                                   |
                                                                   v
                                                            NITTA.Synthesis.Types:Tree
                                                                # sState
                                                                    # sTarget
                                       /------------------------------- # mUnit
                                       |        /---------------------- # mDataFlowGraph
                                       |        |                   # ...
                                       |        |               # ...
                                       |        |
===================================================================================================================
                                       |        |                                         Target project generation
NITTA.Project.Types:Project            |        |                                                 NITTA.Project....
 |      # pName <--------- $tName      |        |
 |      # pLibPath                     |        +<----- $tReceivedValues
 |      # pTargetProjectPath           |        |
 |      # pModel<----------------------/        *<----- functional simulation (FIXME)
 |                                              |
 |      # pTestCntx <---------------------------/
 |
 |
 *<---------- $writeProject
 |                # TargetSystem
 |                    # hardware
 |                    # software
 |                # TestBench
 |                # Templates
 |
 \---> filesystem
@
-}
module NITTA.Synthesis (
    module NITTA.Synthesis.Bind,
    module NITTA.Synthesis.Dataflow,
    module NITTA.Synthesis.Explore,
    module NITTA.Synthesis.Method,
    module NITTA.Synthesis.Refactor,
    module NITTA.Synthesis.Types,
    mkModelWithOneNetwork,
    TargetSynthesis (..),
    runTargetSynthesis,
    synthesizeTargetSystem,
) where

import Control.Monad (when)
import Data.Default as D
import Data.Text (Text)
import qualified Data.Text as T
import NITTA.Intermediate.DataFlow
import NITTA.Intermediate.Simulation
import NITTA.Intermediate.Types
import NITTA.LuaFrontend
import NITTA.Model.Networks.Bus
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.TargetSystem
import NITTA.Model.Types
import NITTA.Project (Project (..), collectNittaPath, defProjectTemplates, runTestbench, writeProject)
import NITTA.Synthesis.Bind
import NITTA.Synthesis.Dataflow
import NITTA.Synthesis.Explore
import NITTA.Synthesis.Method
import NITTA.Synthesis.Refactor
import NITTA.Synthesis.Types
import System.Directory
import System.FilePath
import System.Log.Logger

{- |Description of synthesis task. Applicable for target system synthesis and
testing purpose.
-}
data TargetSynthesis tag v x t = TargetSynthesis
    { -- |target name, used for top level module name and project path
      tName :: String
    , -- |composition of processor units, IO ports and its interconnect
      tMicroArch :: BusNetwork tag v x t
    , -- |optional application source code (lua)
      tSourceCode :: Maybe Text
    , -- |algorithm in intermediate data flow graph representation (if
      -- tSourceCode present will be overwritten)
      tDFG :: DataFlowGraph v x
    , -- |values from input interface for testing purpose
      tReceivedValues :: [(v, [x])]
    , -- |synthesis method
      tSynthesisMethod :: SynthesisMethod tag v x t
    , -- |IP-core library directory
      tLibPath :: String
    , -- |output directory, where CAD create project directory with 'tName' name
      tPath :: String
    , tTemplates :: [FilePath]
    , -- |number of simulation and testbench cycles
      tSimulationCycleN :: Int
    }

instance (VarValTime v x t) => Default (TargetSynthesis String v x t) where
    def =
        TargetSynthesis
            { tName = undefined
            , tMicroArch = undefined
            , tSourceCode = Nothing
            , tDFG = undefined
            , tReceivedValues = def
            , tSynthesisMethod = stateOfTheArtSynthesisIO
            , tLibPath = "hdl"
            , tTemplates = defProjectTemplates
            , tPath = joinPath ["gen"]
            , tSimulationCycleN = 5
            }

runTargetSynthesis leaf = do
    prj <- synthesizeTargetSystem leaf
    traverse runTestbench prj

synthesizeTargetSystem
    TargetSynthesis
        { tName
        , tMicroArch
        , tSourceCode
        , tDFG
        , tReceivedValues
        , tSynthesisMethod
        , tLibPath
        , tPath
        , tTemplates
        , tSimulationCycleN
        } = do
        -- TODO: check that tName is a valid verilog module name
        when (' ' `elem` tName) $ error "TargetSynthesis name contain wrong symbols"
        tDFG' <- maybe (return tDFG) translateToIntermediate tSourceCode
        root <- synthesisTreeRootIO (mkModelWithOneNetwork tMicroArch tDFG')
        synthesise root >>= \case
            Left err -> return $ Left err
            Right leafNode -> do
                Right <$> writeProject' leafNode
        where
            translateToIntermediate src = do
                infoM "NITTA" "Lua transpiler..."
                let tmp = frDataFlow $ lua2functions src
                noticeM "NITTA" "Lua transpiler...ok"
                return tmp

            synthesise root = do
                infoM "NITTA" "synthesis process..."
                node <- tSynthesisMethod root
                case (isComplete node, isLeaf node) of
                    (True, True) -> do
                        noticeM "NITTA" "synthesis process...ok"
                        return $ Right node
                    (False, True) -> do
                        let msg = "synthesis process...fail; is not complete"
                        noticeM "NITTA" msg
                        return $ Left msg
                    (True, False) -> do
                        let msg = "synthesis process...fail; is not leaf"
                        noticeM "NITTA" msg
                        return $ Left msg
                    (False, False) -> do
                        let msg = "synthesis process...fail; is not complete; is not leaf"
                        noticeM "NITTA" msg
                        return $ Left msg

            writeProject' leaf = do
                pInProjectNittaPath <- either (error . T.unpack) id <$> collectNittaPath tTemplates
                pwd <- getCurrentDirectory
                let prj =
                        Project
                            { pName = T.pack tName
                            , pLibPath = tLibPath
                            , pTargetProjectPath = tPath </> tName
                            , pAbsTargetProjectPath = pwd </> tPath </> tName
                            , pInProjectNittaPath
                            , pAbsNittaPath = pwd </> tPath </> tName </> pInProjectNittaPath
                            , pUnit = targetUnit leaf
                            , pUnitEnv = bnEnv $ targetUnit leaf
                            , -- because application algorithm can be refactored we need to use
                              -- synthesised version
                              pTestCntx = simulateDataFlowGraph tSimulationCycleN def tReceivedValues $ targetDFG leaf
                            , pTemplates = tTemplates
                            }
                writeProject prj
                return prj

{- |Make a model of NITTA process with one network and a specific algorithm. All
functions are already bound to the network.
-}
mkModelWithOneNetwork arch dfg =
    TargetSystem
        { mUnit = foldl (flip bind) arch $ functions dfg
        , mDataFlowGraph = dfg
        }
