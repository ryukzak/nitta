{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module NITTA.ProcessUnitsSpec
  ( prop_simulationNaive
  , prop_simulation
  , prop_formalCompletnessNaive
  , prop_formalCompletness
  , naiveGen
  , Alg(..)
  ) where

import           Data.List               (nub, (\\))
import           Data.Maybe              (catMaybes)
import           Data.Set                (fromList)
import           Data.Typeable
import           Debug.Trace
import           NITTA.Base
import           NITTA.Compiler
import           NITTA.FunctionBlocks
import           NITTA.Types
import           Test.QuickCheck
import           Test.QuickCheck.Monadic


data Alg pu v t = Alg
  { algFB     :: [FB v]
  , algValues :: [(v, Int)]
  , algPu     :: pu Passive v t
  }

instance ( Show v, Show t, Show (pu Passive v t) ) => Show (Alg pu v t) where
  show Alg{..} = "alg = [\n" ++ concatMap (\x -> "  " ++ show x ++ ",\n") algFB  ++ "  ]\n"
    ++ "Values: " ++ show algValues




prop_simulationNaive (Alg alg values pu) =
  simulation (bindAllAndNaiveSteps pu alg) values

prop_simulation (Alg _alg values _, pu) =
  simulation pu values


simulation pu values = monadicIO $ do
  res <- run $ testBench pu values
  assert res




prop_formalCompletnessNaive (Alg alg _values pu) =
  formalCompletness (bindAllAndNaiveSteps pu alg) alg

prop_formalCompletness ((Alg alg _values _), pu) =
  formalCompletness pu alg


formalCompletness pu' alg =
  let vars = concatMap variables alg
      steps' = steps $ process $ pu'
      vars' = concatMap variables $ catMaybes
              $ map (\Step{..} -> (cast info :: Maybe (Effect String))) steps'
      alg' = catMaybes $ map (\Step{..} -> (cast info :: Maybe (FB String))) steps'
  in and [ --trace (">>" ++ show (vars \\ vars')) $
           fromList vars == fromList vars'
         , length (nub vars') == length vars'
         , length (nub alg') == length alg'
         , --trace (">>" ++ show (alg \\ alg')) $
           fromList alg == fromList alg'
         ]





naiveGen pu alg = do
  s1 <- choose (0 :: Int, 1)
  let vars = variants pu
  case s1 of
    0 | not $ null vars -> do
          i <- choose (0, length vars - 1)
          let pu' = step pu $ puVar2puAct $ vars !! i
          trace ("step: " ++ show (vars !! i)) $ naiveGen pu' alg
    1 | not $ null alg -> do
          i <- choose (0, length alg - 1)
          let fb = alg !! i
          let alg' = [ x | x <- alg, x /= fb ]
          let Just pu' = bind pu fb
          trace ("bind: " ++ show fb) $ naiveGen pu' alg'
    _ | null vars && null alg -> return pu
    _ -> naiveGen pu alg
