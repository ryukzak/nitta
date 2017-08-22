{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module NITTA.TestBench where

import           Control.Monad.State
import           Data.Default
import           Data.List            (find, intersect, isSubsequenceOf,
                                       partition)
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, fromMaybe, isJust)
import           Data.Typeable        (Typeable, cast, typeOf)
import           NITTA.FunctionBlocks
import qualified NITTA.FunctionBlocks as FB
import           NITTA.Types
import           NITTA.Utils
import           System.Exit
import           System.Process

import           Debug.Trace




class TestBenchRun pu where
  buildArgs :: pu -> [String]


class ( Typeable pu
      , TestBenchRun pu
      , Var v
      ) => TestBench pu v x | pu -> v, pu -> x where
  components :: pu -> [(String, pu -> SimulationContext v x -> String)]

  simulate :: pu -> [(v, x)] -> SimulationContext v x
  simulate pu values = simulateContext pu $ M.fromList $ map (\(v, x) -> ((v, 0), x)) values

  simulateContext :: pu -> SimulationContext v x -> SimulationContext v x

  writeTestBench :: pu -> [(v, x)] -> IO ()
  writeTestBench pu values = do
    let cntx = simulate pu values
    mapM_ (\(fn, gen) -> writeFile fn (gen pu cntx)) $ components pu



testBench pu values = do
  writeTestBench pu values
  runTestBench pu

runTestBench pu = do
  (compileExitCode, compileOut, compileErr) <-
    readProcessWithExitCode "iverilog" (buildArgs pu)[]
  when (compileExitCode /= ExitSuccess) $ do
    putStrLn $ "stdout:\n" ++ compileOut
    putStrLn $ "stderr:\n" ++ compileErr
    die "Verilog compilation failed!"

  (ExitSuccess, simOut, simErr) <- readProcessWithExitCode "./a.out" [] []
  -- Yep, we can't stop simulation with bad ExitCode...
  when ("FAIL" `isSubsequenceOf` simOut) $ do
    putStrLn $ "stdout:\n" ++ simOut
    putStrLn $ "stderr:\n" ++ simErr
    -- die "Simulation failed!"

  return $ not ("FAIL" `isSubsequenceOf` simOut)
