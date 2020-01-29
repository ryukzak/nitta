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
    , snippetTraceAndCheck, assertRe
    ) where

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

