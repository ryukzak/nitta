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


class ( PUClass pu ty v t, Var v ) => TestBench pu ty v t where
  fileName :: (pu ty v t) -> String

  -- showSignalsAt :: pu ty v t -> t -> String
  testControl :: pu ty v t -> M.Map v Int -> String
  testAsserts :: pu ty v t -> M.Map v Int -> String

  writeTestBench :: pu ty v t -> [(v, Int)] -> IO ()
  writeTestBench pu values = do
    writeFile (fileName pu ++ ".tb.control.v") $ testControl pu $ M.fromList values
    writeFile (fileName pu ++ ".tb.asserts.v") $ testAsserts pu $ M.fromList values

passiveInputValue :: ( Var v, Time t ) => t -> [Step v t] -> M.Map v Int -> String
passiveInputValue time steps values = case infoAt time steps of
  [Push v] | v `M.member` values -> "value_i <= " ++ show (values M.! v) ++ ";"
  (_ :: [Effect v]) -> "/* input placeholder */"


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
