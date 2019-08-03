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
import qualified Data.HashMap.Strict              as HM
import qualified Data.List                        as L
import qualified Data.Map                         as M
import           Data.Maybe
import qualified Data.String.Utils                as S
import           Data.Text                        (pack)
import           NITTA.Intermediate.Types
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Project.Parts.Icarus
import           NITTA.Project.Parts.Quartus
import           NITTA.Project.Parts.TargetSystem
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Snippets
import           NITTA.Project.Types
import           NITTA.Utils
import           System.Exit
import           System.IO                        (hPutStrLn, stderr)
import           System.Process
import           Text.InterpolatedString.Perl6    (qc)
import           Text.Regex


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
    unless tbStatus $ hPutStrLn stderr (unlines $ tbCompilerDump ++ tbSimulationDump)
    return report


runTestbench prj@Project{ pPath, pUnit, pTestCntx=Cntx{ cntxProcess, cntxCycleNumber } } = do
    let files = projectFiles prj
        dump type_ out err = lines $ fixIndent [qc|
|           Project: { pPath }
|           Type: { type_ }
|           Files:
|               { files' }
|           Functional blocks:
|               { functions' }
|           Steps:
|               { steps' }
|           -------------------------
|           stdout:
|           { pack out }
|           -------------------------
|           stderr:
|           { pack err }
|           |]
            where
                files' = S.join "\n    " files
                steps' = S.join "\n    " $ map show $ steps $ process $ pUnit
                functions' = S.join "\n    " $ map show $ functions pUnit

    ( compileExitCode, compileOut, compileErr )
        <- readCreateProcessWithExitCode (createIVerilogProcess pPath files) []
    let isCompileOk = compileExitCode == ExitSuccess && null compileErr

    ( simExitCode, simOut, simErr )
        <- readCreateProcessWithExitCode (shell "vvp a.out"){ cwd=Just pPath } []
    let isSimOk = simExitCode == ExitSuccess && not ("FAIL" `L.isSubsequenceOf` simOut)


    return TestbenchReport
        { tbStatus=isCompileOk && isSimOk
        , tbPath=pPath
        , tbFiles=files
        , tbFunctions=map show $ functions pUnit
        , tbCompilerDump=dump "Compiler" compileOut compileErr
        , tbSimulationDump=dump "Simulation" simOut simErr
        , tbFunctionalSimulationCntx=map (HM.fromList . M.assocs . cycleCntx) $ take cntxCycleNumber cntxProcess
        , tbLogicalSimulationCntx=toCntxs $ extractLogValues simOut
        }
    where
        createIVerilogProcess workdir files = (proc "iverilog" files){ cwd=Just workdir }


extractLogValues text = mapMaybe f $ lines text
    where
        f s = case matchRegex assertRe s of
            Just [c, _t, x, _e, v] -> Just (read c, v, read x)
            _                      -> Nothing

toCntxs lst0 = inner (0 :: Int) lst0
    where
        inner n lst
            | (xs, ys) <- L.partition (\(c, _v, _x) -> c == n) lst
            , not $ null xs = (HM.fromList $ map (\(_c, v, x) -> (v, x)) xs) : inner (n + 1) ys
            | otherwise = []
