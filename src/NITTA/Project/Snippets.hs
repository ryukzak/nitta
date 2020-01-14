{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Project.Snippets
Description : Snippets for Verilog code-generation
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Snippets
    ( snippetClkGen
    , snippetDumpFile
    , snippetInitialFinish
    , snippetTestBench, SnippetTestBenchConf(..)
    , snippetTraceAndCheck, assertRe
    ) where

import           Data.Default
import qualified Data.String.Utils               as S
import           Data.Typeable
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.ProcessorUnits.Time
import           NITTA.Model.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Types
import           NITTA.Utils
import           Text.InterpolatedString.Perl6   (qc)
import           Text.Regex



snippetClkGen :: String
snippetClkGen = codeBlock [qc|
    initial begin
        clk = 1'b0;
        rst = 1'b1;
        repeat(4) #1 clk = ~clk;
        rst = 1'b0;
        forever #1 clk = ~clk;
    end
    |]


snippetDumpFile :: String -> String
snippetDumpFile mn = codeBlock [qc|
    initial begin
        $dumpfile("{ mn }_tb.vcd");
        $dumpvars(0, { mn }_tb);
    end
    |]


snippetInitialFinish :: String -> String
snippetInitialFinish block = codeBlock [qc|
    initial begin
        {block}
        $finish;
    end
    |]

snippetTraceAndCheck :: Int -> String
snippetTraceAndCheck width = codeBlock [qc|
    task trace;
        input integer cycle;
        input integer tick;
        input [{ width }-1:0] dt;
        begin
            $display("cycle: %d\ttick: %d\tactual: %d", cycle, tick, dt);
        end
    endtask

    task check;
        input integer cycle;
        input integer tick;
        input [{ width }-1:0] dt;
        input [{ width }-1:0] expect;
        input [256*8-1:0] var;
        begin
            $write("cycle: %d\ttick: %d\tactual: %d\texpect: %d\t%0s", cycle, tick, dt, expect, var);
            if ( !( dt === expect ) ) $display("\t\tFAIL");
            else $display();
        end
    endtask
    |]

assertRe = mkRegex $ concat
    -- cycle:           4	tick:           2	actual:          1	expect:          1	nst_inline_1_0:0
    [ "cycle:[[:space:]]*([[:digit:]]+)[[:space:]]*"
    , "tick:[[:space:]]*([[:digit:]]+)[[:space:]]*"
    , "actual:[[:space:]]*([[:digit:]]+)[[:space:]]*"
    , "expect:[[:space:]]*([[:digit:]]+)[[:space:]]*"
    , "([^\t ]+)"
    ]

data SnippetTestBenchConf m
    = SnippetTestBenchConf
        { tbcSignals       :: [String]
        , tbcPorts         :: Ports m
        , tbcIOPorts       :: IOPorts m
        , tbcSignalConnect :: SignalTag -> String
        , tbcCtrl          :: Microcode m -> String
        , tbDataBusWidth   :: Int
        }

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
                , signalCycle="cycle"
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
        { inline $ S.join "\\n" tbcSignals' };
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
