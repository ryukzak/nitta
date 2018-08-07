{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Project where

import           Control.Monad         (when)
import           Data.FileEmbed
import           Data.List             (isSubsequenceOf)
import qualified Data.List             as L
import qualified Data.String.Utils     as S
import           NITTA.Types
import           NITTA.Utils
import           System.Directory
import           System.Exit
import           System.FilePath.Posix (joinPath, pathSeparator)
import           System.Process


-- | Данный класс позволяет для реализующих его вычислительных блоков сгенировать test bench.
class TestBench pu v x | pu -> v x where
  testBenchDescription :: Project pu -> Cntx v x -> Implementation


-- | Проект вычислителя NITTA.
data Project pu
  = Project
    { projectName :: String
    , libraryPath :: String
    , projectWD   :: String
    , model       :: pu
    } deriving ( Show )


-- | Сгенерировать и выполнить testbench.
writeAndRunTestBench prj@Project{ projectWD } cntx = do
  writeProject prj
  writeImplementation projectWD $ testBenchDescription prj cntx
  runTestBench prj


-- | Записать на диск проект вычислителя.
writeProject prj@Project{ projectName, projectWD, model } = do
  createDirectoryIfMissing True projectWD
  writeImplementation projectWD $ hardware projectName model
  writeImplementation projectWD $ software projectName model
  writeModelsimDo prj
  writeQuartus prj
  writeFile (joinPath [ projectWD, "Makefile" ])
    $ renderST $(embedStringFile "template/Makefile")
      [ ( "iverilog_args", S.join " " $ snd $ projectFiles prj ) ]

-----------------------------------------------------------
-- ModelSim

writeModelsimDo prj@Project{ projectWD } = do
  let (tb, files) = projectFiles prj
  writeFile ( joinPath [ projectWD, "wave.do" ] )
    $ renderST
      $(embedStringFile "template/modelsim/wave.do")
      [ ( "top_level", tb ) ]
  writeFile ( joinPath [ projectWD, "sim.do" ] )
    $ renderST
      $(embedStringFile "template/modelsim/sim.do")
      [ ( "top_level", tb )
      , ( "verilog_files", S.join "\n" $ map (\fn -> "vlog -vlog01compat -work work +incdir+$path $path/" ++ fn) files )
      ]

-----------------------------------------------------------
-- Quartus

writeQuartus prj@Project{ projectName, projectWD, model } = do
  let (tb, files) = projectFiles prj
  writeFile (joinPath [ projectWD, "nitta.qpf" ]) quartusQPF
  writeFile (joinPath [ projectWD, "nitta.qsf" ]) $ quartusQSF tb files
  writeFile (joinPath [ projectWD, "nitta.sdc" ]) quartusSDC
  writeFile ( joinPath [ projectWD, "nitta.v" ] )
    $ renderST
      $(embedStringFile "template/quartus/nitta.v")
      [ ( "top_level_module", moduleName projectName model ) ]
  writeFile ( joinPath [ projectWD, "pll.v" ] )
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


-- | Записать реализацию на диск. Данные размещаются в указанном рабочем каталоге.
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




-- | Запустить testbench в указанной директории.
-- TODO: Сделать вывод через Control.Monad.Writer.
runTestBench prj@Project{ projectWD, model } = do
  let (_tb, files) = projectFiles prj
  ( compileExitCode, compileOut, compileErr )
    <- readCreateProcessWithExitCode (createIVerilogProcess projectWD files) []
  when (compileExitCode /= ExitSuccess || not (null compileErr)) $ do
    mapM_ print $ functionalBlocks model
    putStrLn $ "compiler stdout:\n-------------------------\n" ++ compileOut
    putStrLn $ "compiler stderr:\n-------------------------\n" ++ compileErr
    die "Verilog compilation failed!"

  (simExitCode, simOut, simErr)
    <- readCreateProcessWithExitCode (shell "vvp a.out"){ cwd=Just projectWD } []
  -- Yep, we can't stop simulation with bad ExitCode...

  when (simExitCode /= ExitSuccess || "FAIL" `isSubsequenceOf` simOut) $ do
    mapM_ print $ functionalBlocks model
    putStrLn $ "sim stdout:\n-------------------------\n" ++ simOut
    putStrLn $ "sim stderr:\n-------------------------\n" ++ simErr

  return $ not ("FAIL" `isSubsequenceOf` simOut)


-- | Сгенерировать команду для компиляции icarus verilog-ом вычислительного блока и его тестового
-- окружения.
createIVerilogProcess workdir files
  = let cp = proc "iverilog" files
    in cp { cwd=Just workdir }

projectFiles prj@Project{ projectName, libraryPath, model }
  = let files = L.nub $ concatMap (args "") [ hardware projectName model, testBenchDescription prj undefined ]
        tb = S.replace ".v" "" $ last files
    in (tb, files)
  where
    args p (Aggregate (Just p') subInstances) = concatMap (args $ joinPath [p, p']) subInstances
    args p (Aggregate Nothing subInstances) = concatMap (args $ joinPath [p]) subInstances
    args p (Immidiate fn _) = [ joinPath [ p, fn ] ]
    args _ (FromLibrary fn) = [ joinPath [ libraryPath, fn ] ]
    args _ Empty = []


---------------------------------------------------------------------
-- * Snippets для генерации Verilog-а


verilogClockGenerator = unlines
  [ "initial begin                                                                                             "
  , "  clk = 1'b0;                                                                                             "
  , "  rst = 1'b1;                                                                                             "
  , "  repeat(4) #1 clk = ~clk;                                                                               "
  , "  rst = 1'b0;                                                                                             "
  , "  forever #1 clk = ~clk;                                                                                 "
  , "end                                                                                                       "
  ]


verilogWorkInitialze = unlines
  [ "initial                                                                                                   "
  , "  begin                                                                                                   "
  , "    \\$dumpfile(\"$moduleName$_tb.vcd\");                                                                 "
  , "    \\$dumpvars(0, $moduleName$_tb);                                                                      "
  , "  end                                                                                                     "
  ]


initialFinish inner = unlines
  [ "  initial                                                                                                 "
  , "    begin                                                                                                 "
  , inner
  , "      \\$finish;                                                                                          "
  , "    end                                                                                                   "
  ]
