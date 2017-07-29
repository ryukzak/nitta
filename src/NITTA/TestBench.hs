{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
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







class TestBenchFiles pu where
  fileName :: pu -> String





class ( TestBenchFiles (pu ty v t)
      , Typeable (pu ty v t)
      , Var v
      ) => TestBench pu ty v t x where
  testSignals :: pu ty v t -> SimulationContext v x -> String
  testInputs :: pu ty v t -> SimulationContext v x -> String
  testOutputs :: pu ty v t -> SimulationContext v x -> String

  simulate :: pu ty v t -> [(v, x)] -> SimulationContext v x
  simulate pu values = simulateContext pu $ M.fromList $ map (\(v, x) -> ((v, 0), x)) values

  simulateContext :: pu ty v t -> SimulationContext v x -> SimulationContext v x


  writeTestBench :: pu ty v t -> [(v, x)] -> IO ()
  writeTestBench pu values = do
    let cntx = simulate pu values
    let fn = fileName pu
    writeFile (fn ++ ".tb.signals.v") $ testSignals pu cntx
    writeFile (fn ++ ".tb.inputs.v") $ testInputs pu cntx
    writeFile (fn ++ ".tb.outputs.v") $ testOutputs pu cntx

-- passiveInputValue :: ( Var v, Time t ) => t -> [Step v t] -> M.Map v Int -> String
-- passiveInputValue time steps values = case infoAt time steps of
--   [Push v] | v `M.member` values -> "value_i <= " ++ show (values M.! v) ++ ";"
--   (_ :: [Effect v]) -> "/* input placeholder */"


testBench pu values = do
  writeTestBench pu values
  runTestBench pu

runTestBench pu = do
  let fn = fileName pu
  (compileExitCode, compileOut, compileErr) <- readProcessWithExitCode "iverilog" [ fn ++ ".v"
                                                            , fn ++ ".tb.v"
                                                            ] []
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
