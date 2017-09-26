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

qc g = quickCheckWith stdArgs{ maxSuccess=10 } g

main = do
  qc (prop_formalCompletness . bindAllAndNaiveSelects :: FramIdealDataFlow -> Bool)
  qc (prop_formalCompletness . framDataFlow :: FramDataFlow -> Bool)

  qc (prop_simulation . bindAllAndNaiveSelects :: FramIdealDataFlow -> Property)
  qc (prop_simulation . framDataFlow :: FramDataFlow -> Property)
