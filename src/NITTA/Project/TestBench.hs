{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Project.TestBench
Description : Generation a test bench for the target system.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.TestBench (
    Testable (..),
    IOTestBench (..),
    TestEnvironment (..),
    TestbenchReport (..),
    testBenchTopModuleName,
    projectFiles,
    SnippetTestBenchConf (..),
    snippetTestBench,
) where

import Data.Default
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.String.Utils as S
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics (Generic)
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Types
import NITTA.Project.Types
import NITTA.Project.VerilogSnippets
import NITTA.Utils
import Prettyprinter
import System.FilePath.Posix (joinPath, (</>))

-- |Type class for all testable parts of a target system.
class Testable m v x | m -> v x where
    testBenchImplementation :: Project m v x -> Implementation

{- |Processor units with input/output ports should be tested by generation
external input ports signals and checking output port signals.
-}
class IOTestBench pu v x | pu -> v x where
    testEnvironmentInitFlag :: T.Text -> pu -> Maybe T.Text
    testEnvironmentInitFlag _title _pu = Nothing

    testEnvironment :: T.Text -> pu -> UnitEnv pu -> TestEnvironment v x -> Maybe Verilog
    testEnvironment _title _pu _env _tEnv = Nothing

-- |Information required for testbench generation.
data TestEnvironment v x = TestEnvironment
    { -- |expected data
      teCntx :: Cntx v x
    , -- |duration of computational process
      teComputationDuration :: Int
    }

data TestbenchReport v x = TestbenchReport
    { tbStatus :: Bool
    , tbPath :: FilePath
    , tbFiles :: [FilePath]
    , tbFunctions :: [T.Text]
    , tbSynthesisSteps :: [T.Text]
    , tbCompilerDump :: T.Text
    , tbSimulationDump :: T.Text
    , tbFunctionalSimulationCntx :: [HM.HashMap v x]
    , tbLogicalSimulationCntx :: Cntx v x
    }
    deriving (Generic)

instance Show (TestbenchReport v x) where
    show
        TestbenchReport
            { tbPath
            , tbFiles
            , tbFunctions
            , tbSynthesisSteps
            , tbCompilerDump
            , tbSimulationDump
            } =
            (show :: Doc () -> String)
                [__i|
                    Project: #{ tbPath }
                    Files:
                        #{ nest 4 $ pretty tbFiles }
                    Functional blocks:
                        #{ nest 4 $ pretty tbFunctions }
                    Steps:
                        #{ nest 4 $ pretty tbSynthesisSteps }
                    compiler dump:
                        #{ nest 4 $ pretty tbCompilerDump }
                    simulation dump:
                        #{ nest 4 $ pretty tbSimulationDump }
                |]

-- |Get name of testbench top module.
testBenchTopModuleName ::
    (TargetSystemComponent m, Testable m v x) => Project m v x -> FilePath
testBenchTopModuleName prj = S.replace ".v" "" $ last $ projectFiles prj

-- |Generate list of project verilog files (including testbench).
projectFiles prj@Project{pName, pUnit, pInProjectNittaPath} =
    map
        (pInProjectNittaPath </>)
        $ L.nub $
            concatMap (addPath "") [hardware pName pUnit, testBenchImplementation prj]
    where
        addPath p (Aggregate (Just p') subInstances) = concatMap (addPath $ joinPath [p, p']) subInstances
        addPath p (Aggregate Nothing subInstances) = concatMap (addPath $ joinPath [p]) subInstances
        addPath p (Immediate fn _) = [joinPath [p, fn]]
        addPath _ (FromLibrary fn) = [joinPath ["lib", fn]]
        addPath _ Empty = []

-- |Data Type for SnippetTestBench function
data SnippetTestBenchConf m = SnippetTestBenchConf
    { tbcSignals :: [T.Text]
    , tbcPorts :: Ports m
    , tbcMC2verilogLiteral :: Microcode m -> T.Text
    }

-- |Function for testBench PU test
snippetTestBench ::
    forall m v x t.
    ( VarValTime v x t
    , Show (EndpointRole v)
    , WithFunctions m (F v x)
    , ProcessorUnit m v x t
    , TargetSystemComponent m
    , UnambiguouslyDecode m
    , Typeable m
    , Show (Instruction m)
    , Default (Microcode m)
    ) =>
    Project m v x ->
    SnippetTestBenchConf m ->
    T.Text
snippetTestBench
    Project{pName, pUnit, pTestCntx = Cntx{cntxProcess}, pUnitEnv}
    SnippetTestBenchConf{tbcSignals, tbcPorts, tbcMC2verilogLiteral} =
        let cycleCntx : _ = cntxProcess
            name = moduleName pName pUnit
            p@Process{steps, nextTick} = process pUnit
            fs = functions pUnit
            inst =
                hardwareInstance
                    pName
                    pUnit
                    pUnitEnv
                        { ctrlPorts = Just tbcPorts
                        , valueIn = Just ("data_in", "attr_in")
                        , valueOut = Just ("data_out", "attr_out")
                        }
            controlSignals =
                map
                    ( \t ->
                        let setSignals = pretty $ tbcMC2verilogLiteral (microcodeAt pUnit t)
                            x = targetVal t
                            setValueBus = [i|data_in <= #{ dataLiteral x }; attr_in <= #{ attrLiteral x };|]
                         in setSignals <> " " <> setValueBus <> " @(posedge clk);"
                    )
                    [0 .. nextTick + 1]
            targetVal t
                | Just (Target v) <- endpointAt t p =
                    getCntx cycleCntx v
                | otherwise = 0
            busCheck = map busCheck' [0 .. nextTick + 1]
                where
                    busCheck' t
                        | Just (Source vs) <- endpointAt t p =
                            let v = oneOf vs
                                x = getCntx cycleCntx v
                             in [i|@(posedge clk); assertWithAttr(0, 0, data_out, attr_out, #{ dataLiteral x }, #{ attrLiteral x }, #{ show v });|]
                        | otherwise =
                            [i|@(posedge clk); traceWithAttr(0, 0, data_out, attr_out);|]
            tbcSignals' = map (\x -> [i|reg #{x};|]) tbcSignals
         in doc2text
                [__i|
                    module #{name}_tb();

                    parameter DATA_WIDTH = #{ dataWidth (def :: x) };
                    parameter ATTR_WIDTH = #{ attrWidth (def :: x) };

                    /*
                    Algorithm:
                        #{ nest 4 $ vsep $ map viaShow fs }
                    Process:
                        #{ nest 4 $ vsep $ map viaShow $ reverse steps }
                    Context:
                        #{ nest 4 $ viaShow cycleCntx }
                    */

                    reg clk, rst;
                    #{ vsep tbcSignals' }
                    reg [DATA_WIDTH-1:0]  data_in;
                    reg [ATTR_WIDTH-1:0]  attr_in;
                    wire [DATA_WIDTH-1:0] data_out;
                    wire [ATTR_WIDTH-1:0] attr_out;

                    #{ inst }

                    #{ snippetClkGen }
                    #{ snippetDumpFile name }

                    initial begin
                        @(negedge rst);
                        #{nest 4 $ vsep controlSignals}
                        $finish;
                    end

                    initial begin
                        @(negedge rst);
                        #{ nest 4 $ vsep busCheck }
                                $finish;
                    end

                    #{ verilogHelper (def :: x) }

                    endmodule
                |]
