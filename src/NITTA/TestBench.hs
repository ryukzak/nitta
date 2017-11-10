{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.TestBench where

import           Control.Monad.State
import           Data.List           (isSubsequenceOf)
import qualified Data.List           as L
import qualified Data.Map            as M
import           Data.String.Utils   (replace)
import           NITTA.Types
import           System.Directory
import           System.Exit
import           System.FilePath     (joinPath, pathSeparator)
import           System.Process


-- | Для реализующие этот класс вычислительных блоков могут быть сгенерированы testbench-и.
class TestBench pu v x | pu -> v x where
  testEnviroment :: SimulationContext v x -> pu -> Implementation


-- | Сгенерировать и выполнить testbench.
testBench library workdir pu values = do
  writeTestBench workdir pu values
  runTestBench library workdir pu


-- | Записать реализацию (программную или аппаратную) на диск. Реализация может быть представлена
-- как отдельным файлом, так и целым деревом каталогов. Данные размещаются в указанном рабочем
-- каталоге.
--
-- Ключ $path$ используется для корректной адресации между вложенными файлами. К примеру, в папке
-- DIR лежит два файла f1 и f2, и при этом f1 импортирует в себя f2. Для этого, зачастую, необходимо
-- указать его адресс относительно рабочего каталога, что осуществляется путём вставки этого адреса
-- на место ключа $path$.
writeImplementation workdir p (Immidiate fn src)
  = writeFile (joinPath [workdir, p, fn]) $ replace "$path$" (if null p then "" else p ++ [pathSeparator]) src
writeImplementation workdir p (Project p' subInstances) = do
  let path = joinPath [p, p']
  createDirectoryIfMissing True $ joinPath [ workdir, path ]
  mapM_ (writeImplementation workdir path) subInstances
writeImplementation _ _ (FromLibrary _) = return ()
writeImplementation _ _ Empty = return ()


-- | Записать на диск testbench и все необходимые модули.
writeTestBench workdir pu values = do
  createDirectoryIfMissing True workdir
  writeImplementation workdir "" $ hardware pu
  writeImplementation workdir "" $ software pu
  let cntx = M.fromList $ map (\(v, x) -> ((v, 0), x)) values
  writeImplementation workdir "" $ testEnviroment cntx pu


-- | Запустить testbench в указанной директории.
runTestBench library workdir pu = do
  (compileExitCode, compileOut, compileErr)
    <- readCreateProcessWithExitCode (createIVerilogProcess library workdir pu) []
  when (compileExitCode /= ExitSuccess || not (null compileErr)) $ do
    putStrLn $ "compiler stdout:\n-------------------------\n" ++ compileOut
    putStrLn $ "compiler stderr:\n-------------------------\n" ++ compileErr
    die "Verilog compilation failed!"

  (simExitCode, simOut, simErr)
    <- readCreateProcessWithExitCode (shell "./a.out"){ cwd=Just workdir } []
  -- Yep, we can't stop simulation with bad ExitCode...
  if simExitCode /= ExitSuccess || "FAIL" `isSubsequenceOf` simOut
    then do
      putStrLn $ "sim stdout:\n-------------------------\n" ++ simOut
      putStrLn $ "sim stderr:\n-------------------------\n" ++ simErr
    else return () -- putStrLn $ "Simulation correct"

  return $ not ("FAIL" `isSubsequenceOf` simOut)


-- | Сгенерировать команду для компиляции icarus verilog-ом вычислительного блока и его тестового
-- окружения.
createIVerilogProcess library workdir pu
  = let cp = proc "iverilog" iverilogArgs
    in cp { cwd=Just workdir }
  where
    iverilogArgs = L.nub $ concatMap (args "") $ hardware pu : testEnviroment undefined pu : []

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
  , "  repeat(2) #10 clk = ~clk;                                                                               "
  , "  rst = 1'b0;                                                                                             "
  , "  forever #10 clk = ~clk;                                                                                 "
  , "end                                                                                                       "
  ]


verilogWorkInitialze = unlines
  [ "initial                                                                                                   "
  , "  begin                                                                                                   "
  , "    \\$dumpfile(\"$moduleName$_tb.vcd\");                                                                 "
  , "    \\$dumpvars(0, $moduleName$_tb);                                                                      "
  , "    @(negedge rst);                                                                                       "
  , "  end                                                                                                     "
  ]


initial_finish inner = unlines
  [ "  initial                                                                                                 "
  , "    begin                                                                                                 "
  , inner
  , "      \\$finish;                                                                                          "
  , "    end                                                                                                   "
  ]
