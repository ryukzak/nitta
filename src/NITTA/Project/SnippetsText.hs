{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Module      : NITTA.Project.Snippets
Description : Snippets for Verilog code-generation
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.SnippetsText (
    snippetClkGen,
    snippetDumpFile,
) where

import Data.String.Interpolate (i)
import Data.Text
import NITTA.UtilsText

snippetClkGen :: Text
snippetClkGen =
    codeBlock
        [i|
    initial begin
        clk = 1'b0;
        rst = 1'b1;
        repeat(4) #1 clk = ~clk;
        rst = 1'b0;
        forever #1 clk = ~clk;
    end
    |]

snippetDumpFile :: Text -> Text
snippetDumpFile mn =
    codeBlock
        [i|
    initial begin
        $dumpfile("#{ mn }_tb.vcd");
        $dumpvars(0, #{ mn }_tb);
    end
    |]
