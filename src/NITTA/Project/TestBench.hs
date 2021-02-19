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
import NITTA.Utils hiding (codeBlock, codeLine, inline)
import NITTA.Utils.CodeFormatText
import System.FilePath.Posix (joinPath, (</>))
import Text.InterpolatedString.Perl6 (qc)

-- |Type class for all testable parts of a target system.
class Testable m v x | m -> v x where
    testBenchImplementation :: Project m v x -> Implementation

{- |Processor units with input/output ports should be tested by generation
external input ports signals and checking output port signals.
-}
class IOTestBench pu v x | pu -> v x where
    testEnvironmentInitFlag :: String -> pu -> Maybe String
    testEnvironmentInitFlag _title _pu = Nothing

    testEnvironment :: String -> pu -> UnitEnv pu -> TestEnvironment v x -> String
    testEnvironment _title _pu _env _tEnv = ""

-- |Information required for testbench generation.
data TestEnvironment v x = TestEnvironment
    { -- |expected data
      teCntx :: Cntx v x
    , -- |duration of computational process
      teComputationDuration :: Int
    }

data TestbenchReport v x = TestbenchReport
    { tbStatus :: Bool
    , tbPath :: String
    , tbFiles :: [String]
    , tbFunctions :: [String]
    , tbSynthesisSteps :: [String]
    , tbCompilerDump :: [String]
    , tbSimulationDump :: [String]
    , tbFunctionalSimulationCntx :: [HM.HashMap v x]
    , tbLogicalSimulationCntx :: Cntx v x
    }
    deriving (Generic)

instance (Show v, Show x) => Show (TestbenchReport v x) where
    show
        TestbenchReport
            { tbPath
            , tbFiles
            , tbFunctions
            , tbSynthesisSteps
            , tbCompilerDump
            , tbSimulationDump
            } =
            T.unpack $
                codeBlock
                    [qc|
            Project: { tbPath }
            Files:
                { inline $ showLst tbFiles }
            Functional blocks:
                { inline $ showLst tbFunctions }
            Steps:
                { inline $ showLst tbSynthesisSteps }
            compiler dump:
                { inline $ showLst tbCompilerDump }
            simulation dump:
                { inline $ showLst tbSimulationDump }
            |]
            where
                showLst = T.unlines . map (("    " <>) . T.pack)

-- |Get name of testbench top module.
testBenchTopModuleName prj = S.replace ".v" "" $ last $ projectFiles prj

-- |Generate list of project verilog files (including testbench).
projectFiles prj@Project{pName, pUnit, pNittaPath} =
    map
        (pNittaPath </>)
        $ L.nub $
            concatMap (addPath "") [hardware pName pUnit, testBenchImplementation prj]
    where
        addPath p (Aggregate (Just p') subInstances) = concatMap (addPath $ joinPath [p, T.unpack p']) subInstances
        addPath p (Aggregate Nothing subInstances) = concatMap (addPath $ joinPath [p]) subInstances
        addPath p (Immediate fn _) = [joinPath [p, T.unpack fn]]
        addPath _ (FromLibrary fn) = [joinPath ["lib", T.unpack fn]]
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
            name = moduleName (T.pack pName) pUnit
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
                T.unlines $
                    map
                        ( \t ->
                            let setSignals = tbcMC2verilogLiteral (microcodeAt pUnit t)
                                x = targetVal t
                                setValueBus = [qc|data_in <= { dataLiteral x }; attr_in <= { attrLiteral x };|]
                             in setSignals <> " " <> setValueBus <> " @(posedge clk);"
                        )
                        [0 .. nextTick + 1]
            targetVal t
                | Just (Target v) <- endpointAt t p =
                    getCntx cycleCntx v
                | otherwise = 0
            busCheck = T.concat $ map busCheck' [0 .. nextTick + 1]
                where
                    busCheck' t
                        | Just (Source vs) <- endpointAt t p
                          , let v = oneOf vs
                                x = getCntx cycleCntx v =
                            codeBlock
                                [qc|
                        @(posedge clk); assertWithAttr(0, 0, data_out, attr_out, { dataLiteral x }, { attrLiteral x }, { v });
                        |]
                        | otherwise =
                            codeLine [qc|@(posedge clk); traceWithAttr(0, 0, data_out, attr_out);|]
            tbcSignals' = map (\x -> "reg " <> x <> ";") tbcSignals
         in codeBlock
                [qc|
        module {name}_tb();

        parameter DATA_WIDTH = { dataWidth (def :: x) };
        parameter ATTR_WIDTH = { attrWidth (def :: x) };

        /*
        Algorithm:
        { inline $ T.unlines $ map (T.pack . show) $ fs }
        Process:
        { inline $ T.unlines $ map (T.pack . show) $ reverse steps }
        Context:
        { inline $ (T.pack . show) cycleCntx }
        */

        reg clk, rst;
        { inline $ T.unlines tbcSignals' }
        reg [DATA_WIDTH-1:0]  data_in;
        reg [ATTR_WIDTH-1:0]  attr_in;
        wire [DATA_WIDTH-1:0] data_out;
        wire [ATTR_WIDTH-1:0] attr_out;

        { inline $ T.pack inst }

        { inline snippetClkGen }
        { inline $ snippetDumpFile name }

        initial begin
            @(negedge rst);
            {inline controlSignals}
            $finish;
        end

        initial begin
            @(negedge rst);
            {inline busCheck}
            $finish;
        end

        { inline $ verilogHelper (def :: x) }

        endmodule
        |]
