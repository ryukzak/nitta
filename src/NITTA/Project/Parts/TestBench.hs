{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Project.Parts.TestBench
Description : Generation a test bench for the target system.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Parts.TestBench
    ( TestBench(..)
    , TestEnvironment(..)
    , Testable(..), IOTestBench(..), TestbenchReport(..)
    , testBenchTopModuleName
    , projectFiles
    , snippetTestBench, SnippetTestBenchConf(..)
    ) where

import           Data.Default
import qualified Data.HashMap.Strict             as HM
import qualified Data.List                       as L
import qualified Data.String.Utils               as S
import qualified Data.Text                       as T
import           Data.Typeable
import           GHC.Generics                    (Generic)
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems
import           NITTA.Model.ProcessorUnits.Time
import           NITTA.Model.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Parts.Utils
import           NITTA.Project.Snippets
import           NITTA.Project.Types
import           NITTA.Utils
import           System.Directory                (createDirectoryIfMissing)
import           System.FilePath.Posix           (joinPath)
import           Text.InterpolatedString.Perl6   (qc)


data TestBench = TestBench

instance ( Testable (m v x t) v x
        ) => ProjectPart TestBench (Project (m v x t) v x) where
    writePart TestBench prj@Project{ pPath } = do
        createDirectoryIfMissing True pPath
        writeImplementation pPath $ testBenchImplementation prj


-- |Type class for all testable parts of a target system.
class Testable m v x | m -> v x where
    testBenchImplementation :: Project m v x -> Implementation


-- |Processor units with input/output ports should be tested by generation
-- external input ports signals and checking output port signals.
class IOTestBench pu v x | pu -> v x where
    testEnvironmentInitFlag :: String -> pu -> Maybe String
    testEnvironmentInitFlag _title _pu = Nothing

    testEnvironment :: String -> pu -> TargetEnvironment -> Ports pu -> IOPorts pu -> TestEnvironment v x -> String
    testEnvironment _title _pu _env _ports _io _tEnv = ""


-- |Information required for testbench generation.
data TestEnvironment v x = TestEnvironment
        { teCntx                :: Cntx v x
          -- ^expected data
        , teComputationDuration :: Int
          -- ^duration of computational process
        }


data TestbenchReport v x
    = TestbenchReport
        { tbStatus                   :: Bool
        , tbPath                     :: String
        , tbFiles                    :: [ String ]
        , tbFunctions                :: [ String ]
        , tbSynthesisSteps           :: [ String ]
        , tbCompilerDump             :: [ String ]
        , tbSimulationDump           :: [ String ]
        , tbFunctionalSimulationCntx :: [ HM.HashMap v x ]
        , tbLogicalSimulationCntx    :: [ HM.HashMap v x ]
        }
    deriving ( Generic )


instance ( Show v, Show x ) => Show ( TestbenchReport v x ) where
    show TestbenchReport
            { tbPath, tbFiles
            , tbFunctions, tbSynthesisSteps
            , tbCompilerDump, tbSimulationDump
            }
        = codeBlock [qc|
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
            showLst = unlines . map ("    " ++)

-- |Generate list of project verilog files (including testbench).
projectFiles prj@Project{ pName, pUnit }
    = L.nub $ concatMap (addPath "") [ hardware pName pUnit, testBenchImplementation prj ]
    where
        addPath p (Aggregate (Just p') subInstances) = concatMap (addPath $ joinPath [p, p']) subInstances
        addPath p (Aggregate Nothing subInstances) = concatMap (addPath $ joinPath [p]) subInstances
        addPath p (Immediate fn _) = [ joinPath [ p, fn ] ]
        addPath _ (FromLibrary fn) = [ joinPath [ "lib", T.unpack $ L.last $ T.split (=='/') (T.pack fn) ] ]
        addPath _ Empty = []


-- |Get name of testbench top module.
testBenchTopModuleName prj = S.replace ".v" "" $ last $ projectFiles prj

-- |Data Type for SnippetTestBench function
data SnippetTestBenchConf m
    = SnippetTestBenchConf
        { tbcSignals       :: [String]
        , tbcPorts         :: Ports m
        , tbcIOPorts       :: IOPorts m
        , tbcSignalConnect :: SignalTag -> String
        , tbcCtrl          :: Microcode m -> String
        , tbDataBusWidth   :: Int
        }

-- |Function for testBench PU test
snippetTestBench ::
    ( VarValTime v x t, Num x
    , Show (EndpointRole v)
    , WithFunctions m (F v x)
    , ProcessorUnit m v x t, TargetSystemComponent m, UnambiguouslyDecode m, Typeable m
    , Show (Instruction m), Default (Microcode m)
    ) => Project m v x -> SnippetTestBenchConf m -> String
snippetTestBench
        Project{ pName, pUnit, pTestCntx=Cntx{ cntxProcess } }
        SnippetTestBenchConf{ tbcSignals, tbcSignalConnect, tbcPorts, tbcIOPorts, tbcCtrl, tbDataBusWidth }
    = let
        cycleCntx:_ = cntxProcess
        name = moduleName pName pUnit
        p@Process{ steps, nextTick } = process pUnit
        fs = functions pUnit

        inst = hardwareInstance pName pUnit
            TargetEnvironment
                { signalClk="clk"
                , signalRst="rst"
                , signalCycleBegin="flag_cycle_begin"
                , signalInCycle="flag_in_cycle"
                , signalCycleEnd="flag_cycle_end"
                , inputPort=undefined
                , outputPort=undefined
                , inoutPort=undefined
                , unitEnv=ProcessUnitEnv
                    { parameterAttrWidth=IntParam 4
                    , dataIn="data_in"
                    , attrIn="attr_in"
                    , dataOut="data_out"
                    , attrOut="attr_out"
                    , signal=tbcSignalConnect
                    }
                }
            tbcPorts
            tbcIOPorts

        controlSignals = S.join "\n" $ map (\t -> tbcCtrl (microcodeAt pUnit t) ++ [qc|data_in <= { targetVal t }; @(posedge clk);|]) [ 0 .. nextTick + 1 ]
        targetVal t
            | Just (Target v) <- endpointAt t p
            = either error id $ getX cycleCntx v
            | otherwise = 0
        busCheck = concatMap busCheck' [ 0 .. nextTick + 1 ]
            where
                busCheck' t
                    | Just (Source vs) <- endpointAt t p
                    , let v = oneOf vs
                    , let x = either error id $ getX cycleCntx v
                    = codeBlock [qc|
                        @(posedge clk);
                            $write( "data_out: %d == %d    (%s)", data_out, { x }, { v } );
                            if ( !( data_out === { x } ) ) $display(" FAIL");
                            else $display();
                        |]
                    | otherwise
                    = codeLine [qc|@(posedge clk); $display( "data_out: %d", data_out );|]
        tbcSignals' = map (\x -> "reg " ++ x ++ ";") tbcSignals

    in codeBlock [qc|
        {"module"} {name}_tb();

        parameter DATA_WIDTH = { tbDataBusWidth };
        parameter ATTR_WIDTH = 4;

        /*
        Algorithm:
        { inline $ unlines $ map show $ fs }
        Process:
        { inline $ unlines $ map show $ reverse steps }
        Context:
        { inline $ show cycleCntx }
        */

        reg clk, rst;
        { inline $ S.join "\\n" tbcSignals' }
        reg [DATA_WIDTH-1:0]  data_in;
        reg [ATTR_WIDTH-1:0]  attr_in;
        wire [DATA_WIDTH-1:0] data_out;
        wire [ATTR_WIDTH-1:0] attr_out;

        { inline inst }

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
        endmodule
        |] :: String
