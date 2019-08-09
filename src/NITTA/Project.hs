{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-cse #-}

{-|
Module      : NITTA.Project
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
    # tName                                                                                           NITTA.Frontend
    # tMicroArch --------------------------\
    # tSourceCode ----+                    |     /--+-- mkModelWithOneNetwork
                      |                    |     |  |
                      *<--lua2functions    |     |  |
                      |                    |     |  v      NITTA.Model.TargetSystem:ModelState--\
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
        NITTA.Synthesis.Types:Node                       |                        |           NITTA.Synthesis.Types
            # nModel <------------------------------------------------------------+          NITTA.Synthesis.Method
            # ...                                        |
            # nEdges                                     *<-- search for best synthesis path
                |                                        |
                +-----> NITTA.Synthesis.Types:Edge       v
                            # eNode ----------------- // * // -----\
                            # ...                                  |
                                                                   v
                                                            NITTA.Synthesis.Types:Node
                                                                # nModel
                                       /--------------------------- # mUnit
                                       |        /------------------ # mDataFlowGraph
                                       |        |               # ...
                                       |        |
===================================================================================================================
                                       |        |                                         Target project generation
NITTA.Project.Types:Project      |        |                                                       NITTA.Project....
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
module NITTA.Project
    ( mkModelWithOneNetwork
    , TargetSynthesis(..), runTargetSynthesis
    ) where

import           Control.Monad                    (when)
import           Data.Default                     as D
import           Data.Text                        (Text)
import           NITTA.Frontend
import           NITTA.Intermediate.Simulation
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus         (BusNetwork)
import           NITTA.Model.Problems.Whole
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.TargetSystem
import           NITTA.Model.Types
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Types
import           NITTA.Project.Utils
import           NITTA.Synthesis.Method
import           NITTA.Synthesis.Types
import           System.FilePath                  (joinPath)


-- |Description of synthesis task. Applicable for target system synthesis and
-- testing purpose.
data TargetSynthesis u v x t = TargetSynthesis
    { -- |target name, used for top level module name and project path
      tName            :: String
      -- |composition of processor units, IO ports and its interconnect
    , tMicroArch       :: u
      -- |optional application source code (lua)
    , tSourceCode      :: Maybe Text
      -- |algorithm in intermediate data flow graph representation (if
      -- tSourceCode present will be overwritten)
    , tDFG             :: DataFlowGraph v x
      -- |values from input interface for testing purpose
    , tReceivedValues  :: [ (v, [x]) ]
      -- |verbose standard output (dumps, progress info, etc).
    , tVerbose         :: Bool
      -- |synthesis method
    , tSynthesisMethod :: Node' u v x t -> IO (Node' u v x t)
      -- |project writer, which defines necessary project part
    , tWriteProject    :: Project u v x -> IO ()
      -- |IP-core library directory
    , tLibPath         :: String
      -- |output directory, where CAD create project directory with 'tName' name
    , tPath            :: String
    }

type Node' u v x t = Node (ModelState u v x) (SynthesisDT u)


instance ( VarValTime v x t, Semigroup v ) => Default (TargetSynthesis (BusNetwork String v x t) v x t) where
    def = TargetSynthesis
        { tName=undefined
        , tMicroArch=undefined
        , tSourceCode=Nothing
        , tDFG=undefined
        , tReceivedValues=def
        , tVerbose=False
        , tSynthesisMethod=simpleSynthesisIO
        , tWriteProject=writeWholeProject
        , tLibPath="../../hdl"
        , tPath=joinPath [ "gen" ]
        }


runTargetSynthesis TargetSynthesis
            { tName, tMicroArch, tSourceCode, tDFG, tReceivedValues, tSynthesisMethod, tVerbose, tWriteProject
            , tLibPath, tPath
            } = do
    tDFG' <- maybe (return tDFG) translateToIntermediate tSourceCode
    rootNode <- mkRootNodeIO (mkModelWithOneNetwork tMicroArch tDFG')
    synthesisResult <- synthesis rootNode
    case synthesisResult of
        Left err -> return $ Left err
        Right leafNode -> do
            let prj = project leafNode
            write prj
            report <- testbench prj
            return $ Right report
    where
        translateToIntermediate src = do
            when tVerbose $ putStrLn "lua transpiler"
            let tmp = lua2functions src
            when tVerbose $ putStrLn "lua transpiler - ok"
            return tmp

        synthesis rootNode = do
            when tVerbose $ putStrLn "synthesis process"
            leafNode <- tSynthesisMethod rootNode
            let isComplete = isSynthesisFinish $ nModel leafNode
            when (tVerbose && isComplete) $ putStrLn "synthesis process - ok"
            when (tVerbose && not isComplete) $ putStrLn "synthesis process - fail"
            return $ if isComplete
                then Right leafNode
                else Left "synthesis process - fail"

        project Node{ nModel=ModelState{ mUnit, mDataFlowGraph } } = Project
            { pName=tName
            , pLibPath=tLibPath
            , pPath=joinPath [ tPath, tName ]
            , pUnit=mUnit
            , pTestCntx=simulateDataFlowGraph def tReceivedValues mDataFlowGraph -- because application algorithm can be refactored
            }

        write prj@Project{ pPath } = do
            when tVerbose $ putStrLn $ "write target project (" ++ pPath ++ ")"
            tWriteProject prj
            when tVerbose $ putStrLn $ "write target project (" ++ pPath ++ ") - ok"

        testbench prj = do
            when tVerbose $ putStrLn "run testbench"
            report@TestbenchReport{ tbStatus, tbCompilerDump, tbSimulationDump, tbLogicalSimulationCntx } <- runTestbench prj
            when tVerbose $ case tbStatus of
                True  -> putStrLn "run testbench - ok"
                False -> do
                    putStrLn "run testbench - fail"
                    putStrLn "-----------------------------------------------------------"
                    putStrLn "testbench compiler dump:"
                    putStrLn $ unlines tbCompilerDump
                    putStrLn "-----------------------------------------------------------"
                    putStrLn "testbench simulation dump:"
                    putStrLn $ unlines tbSimulationDump
                    putStrLn $ show tbLogicalSimulationCntx
            return report


-- |Make a model of NITTA process with one network and a specific algorithm. All
-- functions are already bound to the network.
mkModelWithOneNetwork arch dfg = ModelState
    { mUnit=foldl (flip bind) arch $ functions dfg
    , mDataFlowGraph=dfg
    }
