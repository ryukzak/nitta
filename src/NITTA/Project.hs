{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- |Модуль отвечающий за генерацию проектов на базе процессора NITTA.
module NITTA.Project
    ( Project(..)
    , TestBench(..)
    , writeAndRunTestBench
    , writeAndRunTestBenchDevNull
    , writeProject
    -- *Snippets for Verilog code-generation
    , snippetClkGen
    , snippetDumpFile
    , snippetInitialFinish
    ) where

-- FIXME: Файлы библиотек должны копироваться в проект.

-- TODO: Сделать выбор вендора, сейчас это Quartus и IcarusVerilog.

import           Control.Monad                 (when)
import           Data.FileEmbed
import           Data.List                     (isSubsequenceOf)
import qualified Data.List                     as L
import qualified Data.String.Utils             as S
import           NITTA.Types
import           NITTA.Utils
import           System.Directory
import           System.Exit
import           System.FilePath.Posix         (joinPath, pathSeparator)
import           System.Info.Extra             (isWindows)
import           System.IO                     (IOMode (WriteMode), hPrint,
                                                hPutStrLn, stderr, withFile)
import           System.Process
import           Text.InterpolatedString.Perl6 (qq)


-- |Данный класс позволяет для реализующих его вычислительных блоков сгенировать test bench.
class TestBench pu v x | pu -> v x where
    testBenchDescription :: Project pu v x -> Implementation


-- |Проект вычислителя NITTA.
data Project pu v x
    = Project
        { projectName :: String -- ^Наименование проекта.
        , libraryPath :: String -- ^Директория библиотеки с вычислительными блоками.
        , projectPath :: String -- ^Директория проекта, куда будут размещены его файлы.
        , model       :: pu     -- ^Модель вычислительного блока.
        , testCntx    :: Maybe (Cntx v x) -- ^Контекст для генерации test bench.
        } deriving ( Show )


-- |Сохранить проект и выполнить test bench.
writeAndRunTestBench prj = do
    writeProject prj
    runTestBench stderr prj


-- |Сохранить проект и выполнить test bench. При этом вывод текста будет отправлен в @/dev/null@.
-- Используется для unittest-ов, которые должны "падать".
writeAndRunTestBenchDevNull prj = do
    writeProject prj
    withFile (if isWindows then "NUL" else "/dev/null") WriteMode (`runTestBench` prj)


-- |Записать на диск проект вычислителя.
writeProject prj@Project{ projectName, projectPath, model } = do
    createDirectoryIfMissing True projectPath
    writeImplementation projectPath $ hardware projectName model
    writeImplementation projectPath $ software projectName model
    writeImplementation projectPath $ testBenchDescription prj
    writeModelsimDo prj
    writeQuartus prj
    writeFile (joinPath [ projectPath, "Makefile" ])
        $ renderST $(embedStringFile "template/Makefile")
            [ ( "iverilog_args", S.join " " $ snd $ projectFiles prj ) ]


-- |Сгенерировать служебные файлы для симуляции при помощи ModelSim.

-- FIXME: Исправить интеграцию Modelsim и Quartus (прозрачный запуск симуляции по кнопке из
-- Quartus).
writeModelsimDo prj@Project{ projectPath } = do
    let (tb, files) = projectFiles prj
    writeFile ( joinPath [ projectPath, "wave.do" ] )
        $ renderST
            $(embedStringFile "template/modelsim/wave.do")
            [ ( "top_level", tb ) ]
    writeFile ( joinPath [ projectPath, "sim.do" ] )
        $ renderST
            $(embedStringFile "template/modelsim/sim.do")
            [ ( "top_level", tb )
            , ( "verilog_files", S.join "\n" $ map (\fn -> "vlog -vlog01compat -work work +incdir+$path $path/" ++ fn) files )
            ]

-- |Сгенерировать служебные файлы для Quartus.
writeQuartus prj@Project{ projectName, projectPath, model } = do
    let (tb, files) = projectFiles prj
    writeFile (joinPath [ projectPath, "nitta.qpf" ]) quartusQPF
    writeFile (joinPath [ projectPath, "nitta.qsf" ]) $ quartusQSF tb files
    writeFile (joinPath [ projectPath, "nitta.sdc" ]) quartusSDC
    writeFile ( joinPath [ projectPath, "nitta.v" ] )
        $ renderST
            $(embedStringFile "template/quartus/nitta.v")
            [ ( "top_level_module", moduleName projectName model ) ]
    writeFile ( joinPath [ projectPath, "pll.v" ] )
        $(embedStringFile "template/quartus/pll.v")

quartusQPF = $(embedStringFile "template/quartus/project_file.qpf") :: String

quartusQSF tb files = renderST $(embedStringFile "template/quartus/settings_file.qsf")
    [ ( "verilog_files"
      , S.join "\n" $ map ("set_global_assignment -name VERILOG_FILE " ++) files
      )
    , ( "test_bench_files"
      , S.join "\n" $ map (\fn -> "set_global_assignment -name EDA_TEST_BENCH_FILE " ++ fn ++ " -section_id " ++ tb) files
      )
    , ( "testbench_module", tb )
    ]

quartusSDC = $(embedStringFile "template/quartus/synopsys_design_constraint.sdc") :: String


-----------------------------------------------------------


-- |Записать реализацию на диск. Данные размещаются в указанном рабочем каталоге.
--
-- Ключ $path$ используется для корректной адресации между вложенными файлами. К примеру, в папке
-- DIR лежит два файла f1 и f2, и при этом f1 импортирует в себя f2. Для этого, зачастую, необходимо
-- указать его адресс относительно рабочего каталога, что осуществляется путём вставки этого адреса
-- на место ключа $path$.
writeImplementation pwd impl = writeImpl "" impl
    where
        writeImpl p (Immidiate fn src)
            = writeFile (joinPath [pwd, p, fn]) $ S.replace "$path$" (if null p then "" else p ++ [pathSeparator]) src
        writeImpl p (Aggregate p' subInstances) = do
            let path = joinPath $ maybe [p] (\x -> [p, x]) p'
            createDirectoryIfMissing True $ joinPath [ pwd, path ]
            mapM_ (writeImpl path) subInstances
        writeImpl _ (FromLibrary _) = return ()
        writeImpl _ Empty = return ()




-- |Запустить testbench в указанной директории.
-- TODO: Сделать вывод через Control.Monad.Writer.
runTestBench h prj@Project{ projectPath, model } = do
    let (_tb, files) = projectFiles prj
    ( compileExitCode, compileOut, compileErr )
        <- readCreateProcessWithExitCode (createIVerilogProcess projectPath files) []
    when (compileExitCode /= ExitSuccess || not (null compileErr)) $ do
        mapM_ (hPrint h) $ functions model
        hPutStrLn h $ "compiler stdout:\n-------------------------\n" ++ compileOut
        hPutStrLn h $ "compiler stderr:\n-------------------------\n" ++ compileErr
        die "Verilog compilation failed!"

    (simExitCode, simOut, simErr)
        <- readCreateProcessWithExitCode (shell "vvp a.out"){ cwd=Just projectPath } []

    -- Yep, we can't stop simulation with bad ExitCode...
    when (simExitCode /= ExitSuccess || "FAIL" `isSubsequenceOf` simOut) $ do
        mapM_ (hPrint h) $ functions model
        hPutStrLn h $ "sim stdout:\n-------------------------\n" ++ simOut
        hPutStrLn h $ "sim stderr:\n-------------------------\n" ++ simErr

    return $ not ("FAIL" `isSubsequenceOf` simOut)


-- |Сгенерировать команду для компиляции icarus verilog-ом вычислительного блока и его тестового
-- окружения.
createIVerilogProcess workdir files = (proc "iverilog" files){ cwd=Just workdir }

projectFiles prj@Project{ projectName, libraryPath, model }
    = let
        files = L.nub $ concatMap (args "") [ hardware projectName model, testBenchDescription prj ]
        tb = S.replace ".v" "" $ last files
    in (tb, files)
    where
        args p (Aggregate (Just p') subInstances) = concatMap (args $ joinPath [p, p']) subInstances
        args p (Aggregate Nothing subInstances) = concatMap (args $ joinPath [p]) subInstances
        args p (Immidiate fn _) = [ joinPath [ p, fn ] ]
        args _ (FromLibrary fn) = [ joinPath [ libraryPath, fn ] ]
        args _ Empty = []


-----------------------------------------------------------


snippetClkGen :: String
snippetClkGen = [qq|initial begin
    clk = 1'b0;
    rst = 1'b1;
    repeat(4) #1 clk = ~clk;
    rst = 1'b0;
    forever #1 clk = ~clk;
end
|]

snippetDumpFile :: String -> String
snippetDumpFile mn = [qq|initial begin
    \\\$dumpfile("{ mn }_tb.vcd");
    \\\$dumpvars(0, { mn }_tb);
end
|]

snippetInitialFinish :: String -> String
snippetInitialFinish block = [qq|initial begin
$block
    \\\$finish;
end
|]
