{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module Main where

import           Data.Default
import qualified NITTA.Compiler              as C
import qualified NITTA.FunctionBlocks        as FB
import           NITTA.ProcessUnits.Fram
import           NITTA.ProcessUnits.FramSpec
import           NITTA.ProcessUnitsSpec
import           NITTA.TestBench
import           NITTA.Types
import           System.Environment
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck       as QC



main = do
  setEnv "TASTY_QUICKCHECK_TESTS" "5"
  -- TODO: Это не порядок. Необходимо: 1) распараллелить, 2) в ram fs положить.
  setEnv "TASTY_NUM_THREADS" "1"
  defaultMain tests

tests = testGroup "Tests" [ properties ]

properties :: TestTree
properties = testGroup "Properties" [ qcFramProps, huFramTests ]


qcFramProps = testGroup "FRAM check"
  [ QC.testProperty "Сompleteness with a maximum load"
    (prop_formalCompletness . bindAllAndNaiveSelects :: FramIdealDataFlow -> Bool)
  , QC.testProperty "Сompleteness with a partial load"
    (prop_formalCompletness . framDataFlow :: FramDataFlow -> Bool)
  , QC.testProperty "Simulation with a maximum load"
    (prop_simulation . bindAllAndNaiveSelects :: FramIdealDataFlow -> Property)
  , QC.testProperty "Simulation with a partial load"
    (prop_simulation . framDataFlow :: FramDataFlow -> Property)
  ]

huFramTests
  = let alg = [ FB.reg (I "aa") $ O ["ab"]
              , FB.framOutput 9 $ I "ac"
              ]
        fram = (def :: Fram String Int)
        fram' = C.bindAllAndNaiveSelects fram alg
    in testGroup "Unit tests for fram."
      [ testCase "Simple unit test"
      $ assert (testBench fram' [("aa", 42), ("ac", 0x1009)])
      ]





