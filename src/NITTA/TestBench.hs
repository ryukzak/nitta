{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.TestBench where

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


-- | Для реализующие этот класс вычислительных блоков могут быть сгенерированы testbench-и.
class TestBench pu v x | pu -> v x where
  testBenchDescription :: String -> pu -> Cntx v x -> Implementation


-- | Сгенерировать и выполнить testbench.
testBench title library workdir pu values = do
  writeProject title library workdir pu values
  runTestBench title library workdir pu


-- | Записать на диск testbench и все необходимые модули.
writeProject title library pwd pu cntx = do
  createDirectoryIfMissing True pwd
  writeImplementation pwd $ hardware title pu
  writeImplementation pwd $ software title pu
  writeImplementation pwd $ testBenchDescription title pu cntx
  writeModelsimDo title library pwd pu
  writeQuartus title library pwd pu
  writeFile (joinPath [pwd, "Makefile"])
    $ renderST $(embedStringFile "template/Makefile")
      [ ( "iverilog_args", S.join " " $ snd $ projectFiles library pu title ) ]

-----------------------------------------------------------
-- ModelSim

writeModelsimDo title library workdir pu = do
  let (tb, files) = projectFiles library pu title
  writeFile ( joinPath [ workdir, "wave.do" ] )
    $ renderST
      $(embedStringFile "template/modelsim/wave.do")
      [ ( "top_level", tb ) ]
  writeFile ( joinPath [ workdir, "modelsim.do" ] )
    $ renderST
      $(embedStringFile "template/modelsim/sim.do")
      [ ( "top_level", tb )
      , ( "verilog_files", S.join "\n" $ map (\fn -> "vlog -vlog01compat -work work +incdir+$path $path/" ++ fn) files )
      ]

-----------------------------------------------------------
-- Quartus

writeQuartus title library workdir pu = do
  let (tb, files) = projectFiles library pu title
  writeFile (joinPath [ workdir, "nitta.qpf" ]) quartusQPF
  writeFile (joinPath [ workdir, "nitta.qsf" ]) $ quartusQSF tb files
  writeFile (joinPath [ workdir, "nitta.sdc" ]) quartusSDC
  writeFile ( joinPath [ workdir, "nitta.v" ] )
    $ renderST
      $(embedStringFile "template/quartus/nitta.v")
      [ ( "top_level_module", moduleName title pu ) ]
  writeFile ( joinPath [ workdir, "pll.v" ] )
    $(embedStringFile "template/quartus/pll.v")


quartusQPF = $(embedStringFile "template/quartus/project_file.qpf") :: String

quartusQSF tb files = renderST $(embedStringFile "template/quartus/settings_file.qsf")
  [ ( "verilog_files"
    , S.join "\n" $ map ("set_global_assignment -name VERILOG_FILE " ++) files
    )
  , ( "testbench_module", tb )
  ]

quartusSDC = $(embedStringFile "template/quartus/synopsys_design_constraint.sdc") :: String


-- | Записать реализацию (программную или аппаратную) на диск. Реализация может быть представлена
-- как отдельным файлом, так и целым деревом каталогов. ДанныеАА размещаются в указанном рабочем
-- каталоге.
--
-- Ключ $path$ используется для корректной адресации между вложенными файлами. К примеру, в папке
-- DIR лежит два файла f1 и f2, и при этом f1 импортирует в себя f2. Для этого, зачастую, необходимо
-- указать его адресс относительно рабочего каталога, что осуществляется путём вставки этого адреса
-- на место ключа $path$.
writeImplementation pwd impl = writeImpl "" impl
  where
    writeImpl p (Immidiate fn src)
      = writeFile (joinPath [pwd, p, fn]) $ S.replace "$path$" (if null p then "" else p ++ [pathSeparator]) src
    writeImpl p (Project p' subInstances) = do
      let path = joinPath [p, p']
      createDirectoryIfMissing True $ joinPath [ pwd, path ]
      mapM_ (writeImpl path) subInstances
    writeImpl _ (FromLibrary _) = return ()
    writeImpl _ Empty = return ()




-- | Запустить testbench в указанной директории.
-- TODO: Сделать вывод через Control.Monad.Writer.
runTestBench title library workdir pu = do
  let (_tb, files) = projectFiles library pu title
  (compileExitCode, compileOut, compileErr)
    <- readCreateProcessWithExitCode (createIVerilogProcess workdir files) []
  when (compileExitCode /= ExitSuccess || not (null compileErr)) $ do
    mapM_ print $ functionalBlocks pu
    putStrLn $ "compiler stdout:\n-------------------------\n" ++ compileOut
    putStrLn $ "compiler stderr:\n-------------------------\n" ++ compileErr
    die "Verilog compilation failed!"

  (simExitCode, simOut, simErr)
    <- readCreateProcessWithExitCode (shell "vvp a.out"){ cwd=Just workdir } []
  -- Yep, we can't stop simulation with bad ExitCode...

  when (simExitCode /= ExitSuccess || "FAIL" `isSubsequenceOf` simOut) $ do
    mapM_ print $ functionalBlocks pu
    putStrLn $ "sim stdout:\n-------------------------\n" ++ simOut
    putStrLn $ "sim stderr:\n-------------------------\n" ++ simErr

  return $ not ("FAIL" `isSubsequenceOf` simOut)


-- | Сгенерировать команду для компиляции icarus verilog-ом вычислительного блока и его тестового
-- окружения.
createIVerilogProcess workdir files
  = let cp = proc "iverilog" files
    in cp { cwd=Just workdir }

projectFiles library pu title
  = let files = L.nub $ concatMap (args "") [ hardware title pu, testBenchDescription title pu undefined ]
        tb = S.replace ".v" "" $ last files
    in (tb, files)
  where
    args p (Project p' subInstances) = concatMap (args $ joinPath [p, p']) subInstances
    args p (Immidiate fn _) = [ joinPath [ p, fn ] ]
    args _ (FromLibrary fn) = [ joinPath [ library, fn ] ]
    args _ Empty = []


---------------------------------------------------------------------
-- * Snippets для генерации Verilog-а


verilogClockGenerator = unlines
  [ "initial begin                                                                                             "
  , "  clk = 1'b0;                                                                                             "
  , "  rst = 1'b1;                                                                                             "
  , "  repeat(4) #10 clk = ~clk;                                                                               "
  , "  rst = 1'b0;                                                                                             "
  , "  forever #10 clk = ~clk;                                                                                 "
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
