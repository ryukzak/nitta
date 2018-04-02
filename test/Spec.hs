{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module Main where

import           Control.Applicative           ((<$>))
import           Data.Atomics.Counter
import           NITTA.Test.BusNetwork
import           NITTA.Test.FunctionBlocks
import           NITTA.Test.ProcessUnits
import           NITTA.Test.ProcessUnits.Fram
import           NITTA.Test.ProcessUnits.Shift
import           NITTA.Test.Utils
import           System.Environment
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck         as QC

main = do
  counter <- newCounter 0 -- Используется для того, что бы раскладывать файлы в разные папки при симуляции.
  setEnv "TASTY_QUICKCHECK_TESTS" "10"
  -- TODO: Положть gen в ram fs, нечего насиловать диск.
  defaultMain $ testGroup "NITTA"
    [ testGroup "Fram process unit"
      [ testCase "framRegAndOut" framRegAndOut
      , testCase "framRegAndConstant" framRegAndConstant
      , QC.testProperty "completeness" $ prop_completness <$> processGen framProxy
      , QC.testProperty "Fram simulation" $ fmap (prop_simulation "prop_simulation_fram_" counter) $ inputsGen =<< processGen framProxy
      ]
    -- , testGroup "Shift process unit"
    --   [ testCase "shiftBiDirection" shiftBiDirection
    --   ]
    , testGroup "FunctionalBlock"
      [ testCase "reorderAlgorithm" reorderAlgorithmTest
      , testCase "fibonacci" simulateFibonacciTest
      ]
    , testGroup "BusNetwork"
      [ testCase "testAccumAndFram" testAccumAndFram
      , testCase "testShiftAndFram" testShiftAndFram
      ]
    , testGroup "Utils"
      [ testCase "values2dump" values2dumpTests
      , testCase "inputsOfFBs" inputsOfFBsTests
      , testCase "outputsOfFBsTests" outputsOfFBsTests
      , testCase "endpointRoleEq" endpointRoleEq
      ]
    ]
