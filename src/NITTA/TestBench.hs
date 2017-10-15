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
import           NITTA.Types
import           System.Directory
import           System.Exit
import           System.Process


class TestBenchRun pu where
  buildArgs :: pu -> [String]

class TestBench pu v x | pu -> v, pu -> x where
  components :: pu -> [(String, pu -> SimulationContext v x -> String)]
  simulateContext :: pu -> SimulationContext v x -> SimulationContext v x

simulate pu values = simulateContext pu $ M.fromList $ map (\(v, x) -> ((v, 0), x)) values

writeTestBench pu values = do
  let cntx = simulate pu values
  createDirectoryIfMissing True "hdl/gen"
  mapM_ (\(fn, gen) -> writeFile fn (gen pu cntx)) $ components pu



testBench pu values = do
  writeTestBench pu values
  runTestBench pu

runTestBench pu = do
  let args = L.nub $ buildArgs pu
  -- putStrLn $ "iverilog " ++ show args
  (compileExitCode, compileOut, compileErr) <-
    readProcessWithExitCode "iverilog" args []
  when (compileExitCode /= ExitSuccess) $ do
    putStrLn $ "stdout:\n" ++ compileOut
    putStrLn $ "stderr:\n" ++ compileErr
    die "Verilog compilation failed!"

  (ExitSuccess, simOut, simErr) <- readProcessWithExitCode "./a.out" [] []
  -- Yep, we can't stop simulation with bad ExitCode...
  if "FAIL" `isSubsequenceOf` simOut
    then do
      putStrLn $ "stdout:\n" ++ simOut
      putStrLn $ "stderr:\n" ++ simErr
    else return () -- putStrLn $ "Simulation correct"

  return $ not ("FAIL" `isSubsequenceOf` simOut)
