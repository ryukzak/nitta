{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-cse #-}

{-|
Module      : NITTA.TargetSynthesis
Description : Entry point for synthesis process.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

TargetSynthesis is an entry point for synthesis process. TargetSynthesis flow shown on fig.

@
================================================================================================================
                                                                                                         Prepare
NITTA.TargetSynthesis:TargetSynthesis                                                      NITTA.TargetSynthesis
    # tName                                                                                          NITTA.Model
    # tMicroArch --------------------------\                                                      NITTA.Frontend
    # tSourceCode ----+                    |     /--+-- mkModelWithOneNetwork
                      |                    |     |  |
                      *<--lua2functions    |     |  |
                      |                    |     |  v          NITTA.Model:ModelState
    # tDFG <----------+                    +--------*------------> # mUnit        |
        |                                        |                                |
        |                                        v                                |
        +----------------------------------------*-----------> # mDataFlowGraph   |
                                                                                  |
    # tReceivedValues                                                             |
    # tVerbose                                                                    |
    # tSynthesisMethod ----------------------------------\                        |
    # tWriteProject                                      |                        |
                                                         |                        |
================================================================================================================
                                                         |                        |            Synthesis process
        NITTA.Types.Synthesis:Node                       |                        |        NITTA.Types.Synthesis
            # nModel <------------------------------------------------------------+        NITTA.SynthesisMethod
            # ...                                        |
            # nEdges                                     *<-- search for best synthesis path
                |                                        |
                +-----> NITTA.Types.Synthesis:Edge       v
                            # eNode ----------------- // * // -----\
                            # ...                                  |
                                                                   v
                                                            NITTA.Types.Synthesis:Node
                                                                # nModel
                                       /--------------------------- # mUnit
                                       |        /------------------ # mDataFlowGraph
                                       |        |               # ...
                                       |        |
================================================================================================================
                                       |        |                                      Target project generation
NITTA.Types.Project:Project            |        |                                            NITTA.Types.Project
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
module NITTA.TargetSynthesis
    ( mkModelWithOneNetwork
    , TargetSynthesis(..), runTargetSynthesis
    ) where

import           Control.Monad         (when)
import           Data.Default          as D
import qualified Data.Map              as M
import           Data.Text             (Text)
import           NITTA.BusNetwork      (BusNetwork)
import           NITTA.Frontend
import           NITTA.Model
import           NITTA.Project
import           NITTA.SynthesisMethod
import           NITTA.Types
import           NITTA.Types.Project
import           NITTA.Types.Synthesis (Node (..), SynthesisDT, mkRootNodeIO)
import           System.FilePath       (joinPath)


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
        , tLibPath="../.."
        , tPath=joinPath [ "hdl", "gen" ]
        }

runTargetSynthesis TargetSynthesis
            { tName, tMicroArch, tSourceCode, tDFG, tReceivedValues, tSynthesisMethod, tVerbose, tWriteProject
            , tLibPath, tPath
            } = do
    tDFG' <- maybe (return tDFG) translateToIntermediate tSourceCode
    rootNode <- mkRootNodeIO (mkModelWithOneNetwork tMicroArch tDFG')
    synthesis rootNode >>= \case
        Left err -> return $ Left err
        Right leafNode -> fmap Right $ do
            let prj = project leafNode
            write prj
            testbench prj
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

        project Node{ nModel=ModelState{ mUnit } } = Project
            { pName=tName
            , pLibPath=tLibPath
            , pPath=joinPath [ tPath, tName ]
            , pUnit=mUnit
            , pTestCntx=D.def{ cntxReceived=M.fromList tReceivedValues }
            }

        write prj@Project{ pPath } = do
            when tVerbose $ putStrLn $ "write target project (" ++ pPath ++ ")"
            tWriteProject prj
            when tVerbose $ putStrLn $ "write target project (" ++ pPath ++ ") - ok"

        testbench prj = do
            when tVerbose $ putStrLn "run testbench"
            report@TestbenchReport{ tbStatus, tbCompilerDump, tbSimulationDump } <- runTestbench prj
            when tVerbose $ case tbStatus of
                True  -> putStrLn "run testbench - ok"
                False -> do
                    putStrLn "run testbench - fail"
                    putStrLn "-----------------------------------------------------------"
                    putStrLn "testbench compiler dump:"
                    putStrLn tbCompilerDump
                    putStrLn "-----------------------------------------------------------"
                    putStrLn "testbench simulation dump:"
                    putStrLn tbSimulationDump
            return report


-- |Make a model of NITTA process with one network and a specific algorithm. All
-- functions are already bound to the network.
mkModelWithOneNetwork arch dfg = ModelState
    { mUnit=foldl (flip bind) arch $ functions dfg
    , mDataFlowGraph=dfg
    }
