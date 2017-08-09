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



main = do
  quickCheck (prop_formalCompletness . bindAllAndNaiveSelects :: FramIdealDataFlow -> Bool)
  quickCheck (prop_formalCompletness . framDataFlow :: FramDataFlow -> Bool)

  quickCheck (prop_simulation . bindAllAndNaiveSelects :: FramIdealDataFlow -> Property)
  quickCheck (prop_simulation . framDataFlow :: FramDataFlow -> Property)

  -- quickCheckWith stdArgs { maxSuccess=1 } ((\alg -> do
                                               -- let x = bindAllAndNaiveSteps alg
                                               -- prop_simulation x
                                           -- ) :: FramIdealAlg -> Property)


  -- quickCheckWith stdArgs { maxSuccess=1 }
    -- (prop_formalCompletness . bindAllAndNaiveSteps :: FramIdealAlg -> Bool)
  -- quickCheckWith stdArgs { maxSuccess=10000 }
    -- (prop_formalCompletness . framAlg :: FramAlg -> Bool)
