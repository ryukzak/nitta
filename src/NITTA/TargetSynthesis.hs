{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      : NITTA.TargetSynthesis
Description : Entry point for synthesis process and target system generation.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

TargetSynthesis is an entry point for synthesis process. TargetSynthesis flow shown on fig.

@
====================================================================================================================
                                                                                                             Prepare
NITTA.Project:TargetSynthesis                                                                       NITTA.Project...
    # tName                                                                                        NITTA.LuaFrontend
    # tMicroArch --------------------------\
    # tSourceCode ----+                    |     /--+-- mkModelWithOneNetwork
                      |                    |     |  |
                      *<--lua2functions    |     |  |
                      |                    |     |  v    NITTA.Model.TargetSystem:TargetSystem--\
    # tDFG <----------+                    +--------*------------> # mUnit        |             |    NITTA.Model...
        |                                        |                                |             |     /-----------\
        |                                        v                                |             |     |  Target   |
        +----------------------------------------*-----------> # mDataFlowGraph   |             \-----+  System   |
                                                                                  |                   | Imitation |
    # tReceivedValues                                                             |                   |   Model   |
    # tVerbose                                                                    |                   \-----------/
    # tSynthesisMethod ----------------------------------\                        |
    # tWriteProject                                      |                        |
                                                         |                        |
===================================================================================================================
                                                         |                        |               Synthesis process
        NITTA.Synthesis.Tree:Node                        |                        |            NITTA.Synthesis.Tree
            # nModel <------------------------------------------------------------+          NITTA.Synthesis.Method
            # ...                                        |
            # nEdges                                     *<-- search for best synthesis path
                |                                        |
                +-----> NITTA.Synthesis.Tree:Edge        v
                            # eNode ----------------- // * // -----\
                            # ...                                  |
                                                                   v
                                                            NITTA.Synthesis.Tree:Node
                                                                # nModel
                                       /--------------------------- # mUnit
                                       |        /------------------ # mDataFlowGraph
                                       |        |               # ...
                                       |        |
===================================================================================================================
                                       |        |                                         Target project generation
NITTA.Project.Types:Project            |        |                                                 NITTA.Project....
 |      # pName <--------- $tName      |        |
 |      # pLibPath                     |        +<----- $tReceivedValues
 |      # pPath                        |        |
 |      # pModel<----------------------/        *<----- functional simulation (FIXME)
 |                                              |
 |      # pTestCntx <---------------------------/
 |
 |
 *<---------- $tWriteProject by ProjectPart pt m
 |                # TargetSystem
 |                    # hardware
 |                    # software
 |                # QuartusProject
 |                # TestBench
 |                # IcarusMakefile
 |
 \---> filesystem
@
-}
module NITTA.TargetSynthesis (
    mkModelWithOneNetwork,
    TargetSynthesis (..),
    runTargetSynthesis,
    simpleRefactor,
) where

import Control.Monad (when)
import Data.Default as D
import Data.Text (Text)
import NITTA.Intermediate.DataFlow
import NITTA.Intermediate.Simulation
import NITTA.Intermediate.Types
import NITTA.LuaFrontend
import NITTA.Model.Networks.Bus (BusNetwork)
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.TargetSystem
import NITTA.Model.Types
import NITTA.Project (Project (..), TestbenchReport (..), runTestbench, writeWholeProject)
import NITTA.Synthesis.Explore
import NITTA.Synthesis.Method
import NITTA.Synthesis.Types
import System.FilePath (joinPath)

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
    , -- |verbose standard output (dumps, progress info, etc).
      tVerbose :: Bool
    , -- |synthesis method
      tSynthesisMethod :: SynthesisMethod tag v x t
    , -- |project writer, which defines necessary project part
      tWriteProject :: Project (BusNetwork tag v x t) v x -> IO ()
    , -- |IP-core library directory
      tLibPath :: String
    , -- |output directory, where CAD create project directory with 'tName' name
      tPath :: String
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
            , tVerbose = False
            , tSynthesisMethod = stateOfTheArtSynthesisIO
            , tWriteProject = writeWholeProject
            , tLibPath = "../../hdl"
            , tPath = joinPath ["gen"]
            , tSimulationCycleN = 5
            }

runTargetSynthesis
    TargetSynthesis
        { tName
        , tMicroArch
        , tSourceCode
        , tDFG
        , tReceivedValues
        , tSynthesisMethod
        , tVerbose
        , tWriteProject
        , tLibPath
        , tPath
        , tSimulationCycleN
        } = do
        -- TODO: check that tName is a valid verilog module name
        when (' ' `elem` tName) $ error "TargetSynthesis name contain wrong symbols"
        tDFG' <- maybe (return tDFG) translateToIntermediate tSourceCode
        root <- synthesisTreeRootIO (mkModelWithOneNetwork tMicroArch tDFG')
        synthesise root >>= \case
            Left err -> return $ Left err
            Right leafNode -> do
                let prj = project leafNode
                write prj
                report <- testbench prj
                return $ Right report
        where
            translateToIntermediate src = do
                when tVerbose $ putStrLn "> lua transpiler..."
                let tmp = frDataFlow $ lua2functions src
                when tVerbose $ putStrLn "> lua transpiler...ok"
                return tmp

            synthesise root = do
                when tVerbose $ putStrLn "> synthesis process..."
                leaf <- tSynthesisMethod root
                let complete = isComplete leaf
                when (tVerbose && complete) $ putStrLn "> synthesis process...ok"
                when (tVerbose && not complete) $ putStrLn "> synthesis process...fail"
                return $
                    if complete
                        then Right leaf
                        else Left "synthesis process...fail"

            project Tree{sState = SynthesisState{sTarget = TargetSystem{mUnit, mDataFlowGraph}}} =
                Project
                    { pName = tName
                    , pLibPath = tLibPath
                    , pPath = joinPath [tPath, tName]
                    , pUnit = mUnit
                    , -- because application algorithm can be refactored we need to use
                      -- synthesised version
                      pTestCntx = simulateDataFlowGraph tSimulationCycleN def tReceivedValues mDataFlowGraph
                    }

            write prj@Project{pPath} = do
                when tVerbose $ putStrLn $ "> write target project to: \"" ++ pPath ++ "\"..."
                tWriteProject prj
                when tVerbose $ putStrLn $ "> write target project to: \"" ++ pPath ++ "\"...ok"

            testbench prj = do
                when tVerbose $ putStrLn "> run logical synthesis..."
                report@TestbenchReport{tbStatus, tbCompilerDump, tbSimulationDump} <- runTestbench prj
                when tVerbose $
                    if tbStatus
                        then putStrLn "> run logical simulation...ok"
                        else do
                            putStrLn "> run logical simulation...fail"
                            putStrLn "-----------------------------------------------------------"
                            putStrLn "testbench compiler dump:"
                            putStrLn $ unlines tbCompilerDump
                            putStrLn "-----------------------------------------------------------"
                            putStrLn "testbench simulation dump:"
                            putStrLn $ unlines tbSimulationDump
                return report

{- |Make a model of NITTA process with one network and a specific algorithm. All
functions are already bound to the network.
-}
mkModelWithOneNetwork arch dfg =
    TargetSystem
        { mUnit = foldl (flip bind) arch $ functions dfg
        , mDataFlowGraph = dfg
        }

-- |Apply all refactor options untill they exist
simpleRefactor dfg =
    case refactorOptions dfg of
        [] -> dfg
        (r : _) -> simpleRefactor $ refactorDecision dfg r
