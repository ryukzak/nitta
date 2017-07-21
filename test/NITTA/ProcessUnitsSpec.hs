{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module NITTA.ProcessUnitsSpec
  ( prop_simulatePassivePu
  , prop_completeWorkPassivePu
  , Alg(..)
  ) where

import           Data.List               (nub)
import           Data.Maybe              (catMaybes)
import           Data.Set                (fromList)
import           Data.Typeable
import           NITTA.Base
import           NITTA.Compiler
import           NITTA.FunctionBlocks
import           NITTA.Types
import           Test.QuickCheck.Monadic


data Alg pu v t = Alg
  { algFB     :: [FB v]
  , algValues :: [(v, Int)]
  , algPu     :: pu Passive v t
  }
instance ( Show v, Show t, Show (pu Passive v t) ) => Show (Alg pu v t) where
  show Alg{..} = "alg = [\n" ++ concatMap (\x -> "  " ++ show x ++ ",\n") algFB  ++ "  ]\n"
    ++ "Values: " ++ show algValues

prop_simulatePassivePu (Alg alg values pu) = monadicIO $ do
  let pu' = bindAllAndNaiveSteps pu alg
  res <- run $ testBench pu' values
  assert res


prop_completeWorkPassivePu (Alg alg _values pu) =
  let vars = concatMap variables alg
      pu' = bindAllAndNaiveSteps pu alg
      steps' = steps $ process $ pu'
      vars' = concatMap variables $ catMaybes
              $ map (\Step{..} -> (cast info :: Maybe (Effect String))) steps'
      alg' = catMaybes $ map (\Step{..} -> (cast info :: Maybe (FB String))) steps'
  in and [ fromList vars == fromList vars'
         , length (nub vars') == length vars'
         , length (nub alg') == length alg'
         , fromList alg == fromList alg'
         ]
