{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

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
    ) where

import           NITTA.Utils
import           Text.InterpolatedString.Perl6 ( qc )


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
