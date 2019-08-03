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
    , codeBlock, codeLine
    ) where

import qualified Data.String.Utils                as S
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Types
import           NITTA.Utils
import           Text.InterpolatedString.Perl6    (qc)
import           Text.Regex


codeBlock indent str0 = let
        str = drop 1 $ lines str0
        inputIndent = minimum $ filter (> 0) $ map (length . takeWhile (== ' ')) str
        res = unlines $ map ((replicate (indent * 4) ' ' ++) . drop inputIndent) str
    in take (length res - 1) res

codeLine indent str = replicate (indent * 4) ' ' ++ dropWhile (== ' ') str ++ "\n"
    
snippetClkGen :: String
snippetClkGen = [qc|initial begin
    clk = 1'b0;
    rst = 1'b1;
    repeat(4) #1 clk = ~clk;
    rst = 1'b0;
    forever #1 clk = ~clk;
end
|]


snippetDumpFile :: String -> String
snippetDumpFile mn = [qc|initial begin
    $dumpfile("{ mn }_tb.vcd");
    $dumpvars(0, { mn }_tb);
end
|]


snippetInitialFinish :: String -> String
snippetInitialFinish block = [qc|initial begin
{block}
    $finish;
end
|]

snippetTraceAndCheck :: Int -> String
snippetTraceAndCheck width = codeBlock 0 [qc|
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
        , tbcSignalConnect :: SignalTag -> String
        , tbcCtrl          :: Microcode m -> String
        , tbDataBusWidth   :: Int
        }

snippetTestBench
        Project{ pName, pUnit, pTestCntx=Cntx{ cntxProcess } }
        SnippetTestBenchConf{ tbcSignals, tbcSignalConnect, tbcPorts, tbcCtrl, tbDataBusWidth }
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

        controlSignals = S.join "\n    " $ map (\t -> tbcCtrl (microcodeAt pUnit t) ++ [qc| data_in <= { targetVal t }; @(posedge clk);|]) [ 0 .. nextTick + 1 ]
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
                    = fixIndent [qc|
|                       @(posedge clk);
|                           $write( "data_out: %d == %d    (%s)", data_out, { x }, { v } );
|                           if ( !( data_out === { x } ) ) $display(" FAIL");
|                           else $display();
|                   |]
                    | otherwise
                    = fixIndent [qc|
|                        @(posedge clk); $display( "data_out: %d", data_out );
|                   |]

    in fixIndent [qc|
|       {"module"} {name}_tb();
|
|       parameter DATA_WIDTH = { tbDataBusWidth };
|       parameter ATTR_WIDTH = 4;
|
|       /*
|       Algorithm:
|       { unlines $ map show $ fs }
|       Process:
|       { unlines $ map show $ reverse steps }
|       Context:
|       { show cycleCntx }
|       */
|
|       reg clk, rst;
|       reg { S.join ", " tbcSignals };
|       reg [DATA_WIDTH-1:0]  data_in;
|       reg [ATTR_WIDTH-1:0]  attr_in;
|       wire [DATA_WIDTH-1:0] data_out;
|       wire [ATTR_WIDTH-1:0] attr_out;
|
|       { inst }
|
|       { snippetClkGen }
|       { snippetDumpFile name }
|       { snippetInitialFinish $ "    @(negedge rst);\\n    " ++ controlSignals }
|       { snippetInitialFinish $ "    @(negedge rst);\\n" ++ busCheck }
|       endmodule
|       |] :: String
