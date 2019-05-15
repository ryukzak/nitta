{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Project.Utils
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Utils
    ( writeAndRunTestbench
    , runTestbench
    , writeProjectForTest
    , writeWholeProject
    ) where

import           Control.Monad                    (unless)
import qualified Data.List                        as L
import qualified Data.String.Utils                as S
import           Data.Text                        (pack)
import           NITTA.Intermediate.Types
import           NITTA.Project.Parts.Icarus
import           NITTA.Project.Parts.Quartus
import           NITTA.Project.Parts.TargetSystem
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Types
import           NITTA.Utils
import           System.Exit
import           System.IO                        (hPutStrLn, stderr)
import           System.Process
import           Text.InterpolatedString.Perl6    (qc)


-- |Write project with @TargetSystem@, @TestBench@ and @IcarusMakefile@ parts.
writeProjectForTest prj = do
    writePart TargetSystem prj
    writePart TestBench prj
    writePart IcarusMakefile prj


-- |Write project with all available parts.
writeWholeProject prj = do
    writePart TargetSystem prj
    writePart TestBench prj
    writePart QuartusProject prj
    writePart IcarusMakefile prj


-- |Write project and run testbench by Icarus verilog.
writeAndRunTestbench prj = do
    writeProjectForTest prj
    report@TestbenchReport{ tbStatus, tbCompilerDump, tbSimulationDump } <- runTestbench prj
    unless tbStatus $ hPutStrLn stderr (tbCompilerDump ++ tbSimulationDump)
    return report


runTestbench prj@Project{ pPath, pUnit } = do
    let files = projectFiles prj
        dump type_ out err = fixIndent [qc|
|           Project: { pPath }
|           Type: { type_ }
|           Files:
|               { files' }
|           Functional blocks:
|               { functions' }
|           -------------------------
|           stdout:
|           { pack out }
|           -------------------------
|           stderr:
|           { pack err }
|           |]
            where
                files' = S.join "\n    " files
                functions' = S.join "\n    " $ map show $ functions pUnit

    ( compileExitCode, compileOut, compileErr )
        <- readCreateProcessWithExitCode (createIVerilogProcess pPath files) []
    let isCompileOk = compileExitCode == ExitSuccess && null compileErr


    (simExitCode, simOut, simErr)
        <- readCreateProcessWithExitCode (shell "vvp a.out"){ cwd=Just pPath } []
    let isSimOk = simExitCode == ExitSuccess && not ("FAIL" `L.isSubsequenceOf` simOut)

    return TestbenchReport
        { tbStatus=isCompileOk && isSimOk
        , tbPath=pPath
        , tbFiles=files
        , tbFunctions=map show $ functions pUnit
        , tbCompilerDump=dump "Compiler" compileOut compileErr
        , tbSimulationDump=dump "Simulation" simOut simErr
        }
    where
        createIVerilogProcess workdir files = (proc "iverilog" files){ cwd=Just workdir }
