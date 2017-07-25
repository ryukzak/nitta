{-# LANGUAGE BangPatterns         #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module NITTA.ProcessUnitsSpec
  -- ( prop_simulationNaive
  -- , prop_simulation
  -- , prop_formalCompletnessNaive
  -- , prop_formalCompletness
  -- , naiveGen
  -- , Alg(..)
  -- )
where

import           Data.Array
import           Data.List               (nub)
import           Data.Maybe              (catMaybes)
import           Data.Set                (fromList)
import           Data.Set                (fromList, (\\))
import           Data.Typeable
import           Debug.Trace
import qualified NITTA.Compiler          as C
import           NITTA.ProcessUnits.Fram
import           NITTA.TestBench
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
  simulation (C.bindAllAndNaiveSteps pu alg) values

prop_simulation (Alg _alg values _, pu) =
  simulation pu values


simulation pu values = monadicIO $ do
  res <- run $ testBench pu values
  assert res




-- prop_formalCompletnessNaive (Alg alg _values pu) =
  -- formalCompletness (bindAllAndNaiveSteps pu alg) alg

-- prop_formalCompletness ((Alg alg _values _), pu) =
  -- formalCompletness pu alg

bindAllAndNaiveSteps (Alg alg values pu) =
  Alg alg values (C.bindAllAndNaiveSteps pu alg)


prop_formalCompletness (Alg alg _values pu) =
  let vars = concatMap variables alg
      steps' = steps $ process pu
      vars' = concatMap variables $ catMaybes
              $ map (\Step{..} -> (cast info :: Maybe (Effect String))) steps'
      alg' = catMaybes $ map (\Step{..} -> (cast info :: Maybe (FB String))) steps'
  in if and
        [ --trace (">>" ++ show (vars \\ vars')) $
          fromList vars == fromList vars'
        , length (nub vars') == length vars'
        , length (nub alg') == length alg'
        , --trace (">>" ++ show (alg \\ alg')) $
          fromList alg == fromList alg'
        ]
     then True
     else
       trace ( "vars: " ++ show (fromList vars \\ fromList vars') ++ "\n"
             ++ "fbs: " ++ show (fromList alg \\ fromList alg') ++ "\n"
             ++ "cells:\n" ++ concatMap ((++ "\n") . show) (assocs $ frMemory pu)
             ++ "remains:\n" ++ concatMap ((++ "\n") . show) (frRemains pu)
             )
       False




naiveGen pu alg = naiveGen' pu alg []

naiveGen' pu alg passedAlg = do
  s1 <- choose (0 :: Int, 1)
  let vars =
        -- trace ("vars: " ++ show (variants pu) ++ "\n"
        --         ++ "cells:\n" ++ concatMap ((++ "\n") . show) (assocs $ frMemory pu)
        --         ++ "remains:\n" ++ concatMap ((++ "\n") . show) (frRemains pu)
        --       ) $
        variants pu
  case s1 of
    0 | not $ null vars -> do
          i <- choose (0, length vars - 1)
          let pu' =
                trace ("step: " ++ show (vars !! i)
                       ++ " tick: " ++ show (tick $ process pu)) $
                step pu $ C.puVar2puAct $ vars !! i
          naiveGen' pu' alg passedAlg
    1 | not $ null alg -> do
          i <- choose (0, length alg - 1)
          let fb = alg !! i
          let alg' = [ x | x <- alg, x /= fb ]
          case bind pu fb of
            Right pu' -> trace ("bind: " ++ show fb) $
                         naiveGen' pu' alg' (fb : passedAlg)
            Left r -> trace ("skip: " ++ show fb ++ " reason: " ++ show r) $
                      naiveGen' pu alg' passedAlg
    _ | null vars && null alg -> return (pu, passedAlg)
    _ -> naiveGen' pu alg passedAlg
