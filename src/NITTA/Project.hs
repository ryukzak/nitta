{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Module      : NITTA.Project
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project (
    module NITTA.Project.Template,
    module NITTA.Project.TestBench,
    module NITTA.Project.Types,
    module NITTA.Project.VerilogSnippets,
    writeProject,
    runTestbench,
) where

import Control.Exception
import Control.Monad.Identity (runIdentity)
import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.Maybe
import Data.String
import Data.String.Interpolate (__i)
import Data.String.ToString
import Data.Text qualified as T
import Data.Text.IO qualified as T
import NITTA.Intermediate.Types
import NITTA.Model.ProcessorUnits.Types
import NITTA.Project.Context
import NITTA.Project.Template
import NITTA.Project.TestBench
import NITTA.Project.Types
import NITTA.Project.VerilogSnippets
import NITTA.Utils.Base
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.Log.Logger
import System.Process.ListLike (CreateProcess (..), proc)
import System.Process.Text
import Text.Ginger hiding (length)
import Text.Regex

-- |Write project with all available parts.
writeProject prj@Project{pTargetProjectPath} = do
    infoM "NITTA" $ "write target project to: \"" <> pTargetProjectPath <> "\"..."
    writeTargetSystem prj
    writeTestBench prj
    writeRenderedTemplates prj
    noticeM "NITTA" $ "write target project to: \"" <> pTargetProjectPath <> "\"...ok"

writeTargetSystem prj@Project{pName, pTargetProjectPath, pInProjectNittaPath, pUnit} = do
    createDirectoryIfMissing True $ pTargetProjectPath </> pInProjectNittaPath
    writeImplementation prj $ hardware pName pUnit
    writeImplementation prj $ software pName pUnit
    copyLibraryFiles prj

writeTestBench prj@Project{pTargetProjectPath, pInProjectNittaPath} = do
    createDirectoryIfMissing True $ pTargetProjectPath </> pInProjectNittaPath
    writeImplementation prj $ testBenchImplementation prj

runTestbench prj@Project{pTargetProjectPath, pUnit, pTestCntx = Cntx{cntxProcess, cntxCycleNumber}} = do
    infoM "NITTA" $ "run logical synthesis(" <> pTargetProjectPath <> ")..."
    let files = verilogProjectFiles prj
    wd <- getCurrentDirectory

    (compileExitCode, compileOut, compileErr) <- do
        res <- try $ readCreateProcessWithExitCode (createIVerilogProcess pTargetProjectPath files) ""
        case res of
            Left (_ :: IOException) ->
                error
                    ( [__i|iverilog is not available on your system
                           try to install it:
                                MacOS: $ brew install icarus-verilog
                                Ubuntu: $ sudo apt-get install iverilog 
                        |] ::
                        String
                    )
            Right val -> return val

    let isCompileOk = compileExitCode == ExitSuccess && T.null compileErr

    (simExitCode, simOut, simErr) <-
        readCreateProcessWithExitCode (proc "vvp" ["a.out"]){cwd = Just pTargetProjectPath} ""
    let isSimOk = simExitCode == ExitSuccess && not ("FAIL" `T.isInfixOf` simOut)

    let tbStatus = isCompileOk && isSimOk
        tbCompilerDump = dump compileOut compileErr
        tbSimulationDump = dump simOut simErr

    if tbStatus
        then noticeM "NITTA" $ "run testbench (" <> pTargetProjectPath <> ")...ok"
        else do
            noticeM "NITTA" $ "run testbench (" <> pTargetProjectPath <> ")...fail"
            noticeM "NITTA" "-----------------------------------------------------------"
            noticeM "NITTA" "testbench compiler dump:"
            noticeM "NITTA" $ T.unpack tbCompilerDump
            noticeM "NITTA" "-----------------------------------------------------------"
            noticeM "NITTA" "testbench simulation dump:"
            noticeM "NITTA" $ T.unpack tbSimulationDump
    return
        TestbenchReport
            { tbStatus
            , tbPath = joinPath [wd, pTargetProjectPath]
            , tbFiles = files
            , tbFunctions = map showText $ functions pUnit
            , tbSynthesisSteps = map showText $ steps $ process pUnit
            , tbCompilerDump
            , tbSimulationDump
            , tbFunctionalSimulationLog = map cycleCntx $ take cntxCycleNumber cntxProcess
            , tbLogicalSimulationLog = log2hms $ extractLogValues (defX pUnit) $ T.unpack simOut
            }
    where
        createIVerilogProcess workdir files = (proc "iverilog" files){cwd = Just workdir}
        dump "" "" = ""
        dump out err = "stdout:\n" <> out <> "stderr:\n" <> err

extractLogValues x0 text = mapMaybe f $ lines text
    where
        f s = case matchRegex (verilogAssertRE x0) $ toString s of
            Just [c, _t, x, _e, v] -> Just (read c, fromString $ toString v, read x)
            _ -> Nothing

log2hms lst0 = cntxProcess
    where
        cntxProcess = inner (0 :: Int) lst0
        inner n lst
            | (xs, ys) <- L.partition (\(c, _v, _x) -> c == n) lst
            , not $ null xs =
                let cycleCntx = HM.fromList $ map (\(_c, v, x) -> (v, x)) xs
                 in cycleCntx : inner (n + 1) ys
            | otherwise = []

-- |Ginger is powerfull but slow down testing two times.
enableGingerForImplementation = True

-- |Write 'Implementation' to the file system.
writeImplementation prj@Project{pTargetProjectPath = prjPath, pInProjectNittaPath = nittaPath} impl = writeImpl nittaPath impl
    where
        writeImpl p (Immediate fn src0) | enableGingerForImplementation = do
            let src = T.unpack src0
                implCtx = implementationContext prj p
            template <-
                either (error . formatParserError (Just src)) return <$> runIdentity $
                    parseGinger (const $ return Nothing) Nothing src
            T.writeFile (joinPath [prjPath, p, fn]) $ runGinger implCtx template
        writeImpl p (Immediate fn src0) =
            T.writeFile (joinPath [prjPath, p, fn]) $ T.replace "{{ nitta.paths.nest }}" (T.pack p) src0
        writeImpl p (Aggregate p' subInstances) = do
            let path = joinPath $ maybe [p] (\x -> [p, x]) p'
            createDirectoryIfMissing True $ joinPath [prjPath, path]
            mapM_ (writeImpl path) subInstances
        writeImpl _ (FromLibrary _) = return ()
        writeImpl _ Empty = return ()

-- |Copy library files to target path.
copyLibraryFiles prj = mapM_ (copyLibraryFile prj) $ libraryFiles prj
    where
        copyLibraryFile Project{pTargetProjectPath, pInProjectNittaPath, pLibPath} file = do
            let fullNittaPath = joinPath [pTargetProjectPath, pInProjectNittaPath]
            source <- makeAbsolute $ normalise $ joinPath [pLibPath, file]
            target <- makeAbsolute $ normalise $ joinPath [fullNittaPath, "lib", file]
            directory <- makeAbsolute $ normalise $ joinPath [fullNittaPath, "lib", takeDirectory file]

            createDirectoryIfMissing True directory
            copyFile source target

        libraryFiles Project{pName, pUnit} =
            L.nub $ concatMap (args "") [hardware pName pUnit]
            where
                args p (Aggregate (Just p') subInstances) = concatMap (args $ joinPath [p, p']) subInstances
                args p (Aggregate Nothing subInstances) = concatMap (args p) subInstances
                args _ (FromLibrary fn) = [fn]
                args _ _ = []
