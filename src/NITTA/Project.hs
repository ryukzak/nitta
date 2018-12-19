{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Project
Description : Target system project generation
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project
    ( writeProject
    , writeAndRunTestBench
    -- *Snippets for Verilog code-generation
    , snippetClkGen
    , snippetDumpFile
    , snippetInitialFinish
    , snippetTestBench
    ) where


import           Control.Monad                   (mapM_, unless)
import qualified Data.List                       as L
import qualified Data.String.Utils               as S
import           Data.Text                       (pack)
import           NITTA.Functions                 as F
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
writeAndRunTestBench prj = do
    writeProject prj
    report@TestBenchReport{ tbStatus, tbCompilerDump, tbSimulationDump } <- runTestBench prj
    unless tbStatus $ hPutStrLn stderr (tbCompilerDump ++ tbSimulationDump)
    return report



runTestBench prj@Project{ projectPath, processorModel } = do
    let (_tb, files) = projectFiles prj

    let dump type_ out err = fixIndent [qc|
|           Project: { projectPath }
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
                functions' = S.join "\n    " $ map show $ functions processorModel

    ( compileExitCode, compileOut, compileErr )
        <- readCreateProcessWithExitCode (createIVerilogProcess projectPath files) []
    let isCompileOk = compileExitCode == ExitSuccess && null compileErr


    (simExitCode, simOut, simErr)
        <- readCreateProcessWithExitCode (shell "vvp a.out"){ cwd=Just projectPath } []
    let isSimOk = simExitCode == ExitSuccess && not ("FAIL" `L.isSubsequenceOf` simOut)

    return TestBenchReport
        { tbStatus=isCompileOk && isSimOk
        , tbPath=projectPath
        , tbFiles=files
        , tbFunctions=map show $ functions processorModel
        , tbCompilerDump=dump "Compiler" compileOut compileErr
        , tbSimulationDump=dump "Simulation" simOut simErr
        }
    where
        createIVerilogProcess workdir files = (proc "iverilog" files){ cwd=Just workdir }



-- |Записать на диск проект вычислителя.
writeProject prj@Project{ projectName, projectPath, processorModel, targetPlatforms } = do
    createDirectoryIfMissing True projectPath
    writeImplementation projectPath $ hardware projectName processorModel
    writeImplementation projectPath $ software projectName processorModel
    writeImplementation projectPath $ testBenchDescription prj
    copyLibraryFiles prj
    mapM_ (`writePlatformSpecific` prj) targetPlatforms


-- |Записать реализацию на диск. Данные размещаются в указанном рабочем каталоге.
--
-- Ключ $path$ используется для корректной адресации между вложенными файлами. К примеру, в папке
-- DIR лежит два файла f1 и f2, и при этом f1 импортирует в себя f2. Для этого, зачастую, необходимо
-- указать его адресс относительно рабочего каталога, что осуществляется путём вставки этого адреса
-- на место ключа $path$.
writeImplementation pwd = writeImpl ""
    where
        writeImpl p (Immidiate fn src)
            = writeFile (joinPath [pwd, p, fn]) $ S.replace "$path$" (if null p then "" else p ++ [pathSeparator]) src
        writeImpl p (Aggregate p' subInstances) = do
            let path = joinPath $ maybe [p] (\x -> [p, x]) p'
            createDirectoryIfMissing True $ joinPath [ pwd, path ]
            mapM_ (writeImpl path) subInstances
        writeImpl _ (FromLibrary _) = return ()
        writeImpl _ Empty = return ()


-- |Скопировать файл в lib, если он находится в libraryPath
copyLibraryFiles prj = mapM_ (copyLibraryFile prj) $ libraryFiles prj

copyLibraryFile Project{ projectPath } file = do
    libraryPath' <- makeAbsolute $ joinPath [projectPath, "lib"]
    createDirectoryIfMissing True libraryPath'
    let fileName = last $ S.split "/" file
    from <- makeAbsolute $ joinPath [projectPath, file]
    to <- makeAbsolute $ joinPath [projectPath, "lib", fileName]
    copyFile from to

libraryFiles prj@Project{ projectName, libraryPath, processorModel }
    = L.nub $ concatMap (args "") [ hardware projectName processorModel, testBenchDescription prj ]
    where
        args p (Aggregate (Just p') subInstances) = concatMap (args $ joinPath [p, p']) subInstances
        args p (Aggregate Nothing subInstances) = concatMap (args $ joinPath [p]) subInstances
        args _ (FromLibrary fn) = [ joinPath [ libraryPath, fn ] ]
        args _ _ = []



-- *Platform specific

writePlatformSpecific Makefile = makefile
writePlatformSpecific DE0Nano  = de0nano




-----------------------------------------------------------


snippetClkGen :: String
snippetClkGen = [qc|initial begin
    clk = 1'b0;
    rst = 1'b1;
    repeat(4) #1 clk = ~clk;
    rst = 1'b0;
    forever #1 clk = ~clk;
end
|]

snippetDumpFile :: String -> String
snippetDumpFile mn = [qc|initial begin
    $dumpfile("{ mn }_tb.vcd");
    $dumpvars(0, { mn }_tb);
end
|]

snippetInitialFinish :: String -> String
snippetInitialFinish block = [qc|initial begin
{block}
    $finish;
end
|]

snippetTestBench
        Project{ projectName, processorModel=pu, testCntx }
        TestBenchSetup{ tbcSignals, tbcSignalConnect, tbcPorts, tbcCtrl }
    = let
        mn = moduleName projectName pu
        p@Process{ steps, nextTick } = process pu
        Just cntx = foldl ( \(Just cntx') fb -> simulateOn cntx' pu fb ) testCntx $ functions pu

        inst = hardwareInstance projectName pu
            Enviroment
                { signalClk="clk"
                , signalRst="rst"
                , signalCycle="cycle"
                , inputPort=undefined
                , outputPort=undefined
                , net=NetEnv
                    { parameterAttrWidth=IntParam 4
                    , dataIn="data_in"
                    , attrIn="attr_in"
                    , dataOut="data_out"
                    , attrOut="attr_out"
                    , signal=tbcSignalConnect
                    }
                }
            tbcPorts

        controlSignals = S.join "\n    " $ map (\t -> tbcCtrl (microcodeAt pu t) ++ [qc| data_in <= { targetVal t }; @(posedge clk);|]) [ 0 .. nextTick + 1 ]
        targetVal t
            | Just (Target v) <- endpointAt t p
            , Just val <- F.get cntx v
            = val
            | otherwise = 0

        busCheck = concatMap busCheck' [ 0 .. nextTick + 1 ]
            where
                busCheck' t
                    | Just (Source vs) <- endpointAt t p
                    , let v = oneOf vs
                    , let (Just val) = F.get cntx v
                    = fixIndent [qc|
|                       @(posedge clk);
|                           $write( "data_out: %d == %d    (%s)", data_out, { val }, { v } );
|                           if ( !( data_out === { val } ) ) $display(" FAIL");
|                           else $display();
|                   |]
                    | otherwise
                    = fixIndent [qc|
|                        @(posedge clk); $display( "data_out: %d", data_out );
|                   |]

    in fixIndent [qc|
|       {"module"} {mn}_tb();
|
|       parameter DATA_WIDTH = 32;
|       parameter ATTR_WIDTH = 4;
|
|       /*
|       Algorithm:
|       { unlines $ map show $ functions pu }
|       Process:
|       { unlines $ map show $ reverse steps }
|       Context:
|       { show cntx }
|       */
|
|       reg clk, rst;
|       reg { S.join ", " tbcSignals };
|       reg [DATA_WIDTH-1:0]  data_in;
|       reg [ATTR_WIDTH-1:0]  attr_in;
|       wire [DATA_WIDTH-1:0] data_out;
|       wire [ATTR_WIDTH-1:0] attr_out;
|
|       { inst }
|
|       { snippetClkGen }
|       { snippetDumpFile mn }
|       { snippetInitialFinish $ "    @(negedge rst);\\n    " ++ controlSignals }
|       { snippetInitialFinish $ "    @(negedge rst);\\n" ++ busCheck }
|       endmodule
|       |] :: String
