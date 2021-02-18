{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Project.Parts.Icarus
Description : Makefile for running testbench by IcarusVerilog
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Parts.Icarus (
    writeIcarusMakefile,
) where

import NITTA.Project.Template
import NITTA.Project.Types

writeIcarusMakefile prj = writeRenderedTemplates prj{pTemplates = ["board/Icarus"]}
