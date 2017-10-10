{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Array                  (elems)
import           Data.Default
import           Data.List                   (nub)
import           Data.Maybe
import           Data.Set                    (fromList, (\\))
import           Data.Typeable
import qualified NITTA.Compiler              as C
import           NITTA.FunctionBlocks
import           NITTA.ProcessUnits.Fram
import           NITTA.ProcessUnits.FramSpec
import           NITTA.ProcessUnitsSpec
import           NITTA.Timeline
import           NITTA.Types
import           NITTA.Utils
import           Test.QuickCheck

import           System.Environment
import           Test.Tasty
import           Test.Tasty.QuickCheck       as QC



main = do
  setEnv "TASTY_QUICKCHECK_TESTS" "5"
  -- TODO: Это не порядок. Необходимо: 1) распараллелить, 2) в ram fs положить.
  setEnv "TASTY_NUM_THREADS" "1"
  defaultMain tests

tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]


qcProps = testGroup "FRAM check"
  [ QC.testProperty "Сompleteness with a maximum load"
    (prop_formalCompletness . bindAllAndNaiveSelects :: FramIdealDataFlow -> Bool)
  , QC.testProperty "Сompleteness with a partial load"
    (prop_formalCompletness . framDataFlow :: FramDataFlow -> Bool)
  , QC.testProperty "Simulation with a maximum load"
    (prop_simulation . bindAllAndNaiveSelects :: FramIdealDataFlow -> Property)
  , QC.testProperty "Simulation with a partial load"
    (prop_simulation . framDataFlow :: FramDataFlow -> Property)
  ]

-- unitTests = testGroup "Unit tests"
--   [ testCase "List comparison (different length)" $
--       [1, 2, 3] `compare` [1,2] @?= GT

--   -- the following test does not hold
--   , testCase "List comparison (same length)" $
--       [1, 2, 3] `compare` [1,2,2] @?= LT
--   ]

