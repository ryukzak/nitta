{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    IcarusMakefile (..),
) where

import qualified Data.String.Utils as S
import NITTA.Project.Implementation
import NITTA.Project.Parts.TestBench
import NITTA.Project.Types
import NITTA.Utils
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (joinPath)
import Text.InterpolatedString.Perl6 (qc)

data IcarusMakefile = IcarusMakefile

instance
    ( TargetSystemComponent (m v x t)
    , Testable (m v x t) v x
    ) =>
    ProjectPart IcarusMakefile (Project (m v x t) v x)
    where
    writePart IcarusMakefile prj@Project{pPath} = do
        createDirectoryIfMissing True pPath
        makefile prj

makefile prj@Project{pPath} =
    writeFile (joinPath [pPath, "Makefile"]) $
        space2tab 4 $
            codeBlock
                [qc|
        icarus:
            iverilog { S.join " " $ projectFiles prj }
            vvp a.out
        |]
