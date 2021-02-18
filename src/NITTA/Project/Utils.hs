{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Project.Utils
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Utils (
    writeWholeProject,
    runTestbench,
) where

import Data.Default
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import NITTA.Intermediate.Types
import NITTA.Model.ProcessorUnits.Types
import NITTA.Project.Parts.TargetSystem
import NITTA.Project.Parts.TestBench
import NITTA.Project.Template
import NITTA.Project.Types
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.Log.Logger
import System.Process
import Text.Regex

-- |Write project with all available parts.
writeWholeProject prj@Project{pPath} = do
    infoM "NITTA" $ "write target project to: \"" <> pPath <> "\"..."
    writeTargetSystem prj
    writeTestBench prj
    writeRenderedTemplates prj
    noticeM "NITTA" $ "write target project to: \"" <> pPath <> "\"...ok"

runTestbench prj@Project{pPath, pUnit, pTestCntx = Cntx{cntxProcess, cntxCycleNumber}} = do
    infoM "NITTA" $ "run logical synthesis(" <> pPath <> ")..."
    let files = projectFiles prj
    wd <- getCurrentDirectory

    (compileExitCode, compileOut, compileErr) <-
        readCreateProcessWithExitCode (createIVerilogProcess pPath files) []
    let isCompileOk = compileExitCode == ExitSuccess && null compileErr

    (simExitCode, simOut, simErr) <-
        readCreateProcessWithExitCode (shell "vvp a.out"){cwd = Just pPath} []
    let isSimOk = simExitCode == ExitSuccess && not ("FAIL" `L.isSubsequenceOf` simOut)

    let tbStatus = isCompileOk && isSimOk
        tbCompilerDump = dump compileOut compileErr
        tbSimulationDump = dump simOut simErr

    if tbStatus
        then noticeM "NITTA" $ "run testbench (" <> pPath <> ")...ok"
        else do
            noticeM "NITTA" $ "run testbench (" <> pPath <> ")...fail"
            noticeM "NITTA" "-----------------------------------------------------------"
            noticeM "NITTA" "testbench compiler dump:"
            noticeM "NITTA" $ unlines tbCompilerDump
            noticeM "NITTA" "-----------------------------------------------------------"
            noticeM "NITTA" "testbench simulation dump:"
            noticeM "NITTA" $ unlines tbSimulationDump
    return
        TestbenchReport
            { tbStatus
            , tbPath = joinPath [wd, pPath]
            , tbFiles = files
            , tbFunctions = map show $ functions pUnit
            , tbSynthesisSteps = map show $ steps $ process pUnit
            , tbCompilerDump
            , tbSimulationDump
            , tbFunctionalSimulationCntx = map (HM.fromList . M.assocs . cycleCntx) $ take cntxCycleNumber cntxProcess
            , tbLogicalSimulationCntx = log2cntx $ extractLogValues (defX pUnit) simOut
            }
    where
        createIVerilogProcess workdir files = (proc "iverilog" files){cwd = Just workdir}
        dump out err = ["stdout:"] ++ lines out ++ ["stderr:"] ++ lines err

extractLogValues x0 text = mapMaybe f $ lines text
    where
        f s = case matchRegex (verilogAssertRE x0) s of
            Just [c, _t, x, _e, v] -> Just (read c, v, read x)
            _ -> Nothing

log2cntx lst0 =
    Cntx
        { cntxProcess
        , cntxReceived = def
        , cntxCycleNumber = length cntxProcess
        }
    where
        cntxProcess = inner (0 :: Int) lst0
        inner n lst
            | (xs, ys) <- L.partition (\(c, _v, _x) -> c == n) lst
              , not $ null xs =
                let cycleCntx = CycleCntx $ M.fromList $ map (\(_c, v, x) -> (v, x)) xs
                 in cycleCntx : inner (n + 1) ys
            | otherwise = []
