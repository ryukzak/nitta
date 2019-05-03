{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.Project
Description : Target system project generation
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project
    ( writeAndRunTestbench
    , runTestbench
    ) where


import           Control.Monad                   (mapM_, unless)
import qualified Data.List                       as L
import qualified Data.String.Utils               as S
import           Data.Text                       (pack)
import           NITTA.BusNetwork
import           NITTA.PlatformSpecific.DE0Nano
import           NITTA.PlatformSpecific.Makefile
import           NITTA.Types
import           NITTA.Types.Project
import           NITTA.Utils
import           System.Directory
import           System.Exit
import           System.FilePath.Posix           (joinPath, pathSeparator)
import           System.IO                       (hPutStrLn, stderr)
import           System.Process
import           Text.InterpolatedString.Perl6   (qc)



-- |Сохранить проект и выполнить test bench.
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



instance ( TargetSystemComponent (m v x t)
        ) => ProjectPart TargetSystem (Project (m v x t) v x) where
    writePart TargetSystem prj@Project{ pName, pPath, pUnit } = do
        createDirectoryIfMissing True pPath
        writeImplementation pPath $ hardware pName pUnit
        writeImplementation pPath $ software pName pUnit
        copyLibraryFiles prj

instance ( Testable (m v x t) v x
        ) => ProjectPart TestBench (Project (m v x t) v x) where
    writePart TestBench prj@Project{ pPath } = do
        createDirectoryIfMissing True pPath
        writeImplementation pPath $ testBenchImplementation prj

instance ( TargetSystemComponent (m v x t), Testable (m v x t) v x
        ) => ProjectPart IcarusMakefile (Project (m v x t) v x) where
    writePart IcarusMakefile prj@Project{ pPath } = do
        createDirectoryIfMissing True pPath
        makefile prj

instance ( Var v, Time t, Val x, Show x
        ) => ProjectPart QuartusProject (Project (BusNetwork String v x t) v x) where
    writePart QuartusProject prj@Project{ pPath } = do
        createDirectoryIfMissing True pPath
        de0nano prj



-- |Записать реализацию на диск. Данные размещаются в указанном рабочем каталоге.
--
-- Ключ $path$ используется для корректной адресации между вложенными файлами. К примеру, в папке
-- DIR лежит два файла f1 и f2, и при этом f1 импортирует в себя f2. Для этого, зачастую, необходимо
-- указать его адресс относительно рабочего каталога, что осуществляется путём вставки этого адреса
-- на место ключа $path$.
writeImplementation pwd = writeImpl ""
    where
        writeImpl p (Immediate fn src)
            = writeFile (joinPath [pwd, p, fn]) $ S.replace "$path$" (if null p then "" else p ++ [pathSeparator]) src
        writeImpl p (Aggregate p' subInstances) = do
            let path = joinPath $ maybe [p] (\x -> [p, x]) p'
            createDirectoryIfMissing True $ joinPath [ pwd, path ]
            mapM_ (writeImpl path) subInstances
        writeImpl _ (FromLibrary _) = return ()
        writeImpl _ Empty = return ()


-- |Скопировать файл в lib, если он находится в pLibPath
copyLibraryFiles prj = mapM_ (copyLibraryFile prj) $ libraryFiles prj
    where
        copyLibraryFile Project{ pPath } file = do
            pLibPath' <- makeAbsolute $ joinPath [pPath, "lib"]
            createDirectoryIfMissing True pLibPath'
            let fileName = last $ S.split "/" file
            from <- makeAbsolute $ joinPath [pPath, file]
            to <- makeAbsolute $ joinPath [pPath, "lib", fileName]
            copyFile from to

        libraryFiles Project{ pName, pLibPath, pUnit }
            = L.nub $ concatMap (args "") [ hardware pName pUnit ]
            where
                args p (Aggregate (Just p') subInstances) = concatMap (args $ joinPath [p, p']) subInstances
                args p (Aggregate Nothing subInstances) = concatMap (args $ joinPath [p]) subInstances
                args _ (FromLibrary fn) = [ joinPath [ pLibPath, fn ] ]
                args _ _ = []
