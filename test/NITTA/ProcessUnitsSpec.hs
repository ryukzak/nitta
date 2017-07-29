{-# LANGUAGE BangPatterns         #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module NITTA.ProcessUnitsSpec where

import           Data.Array
import           Data.List               (nub)
import           Data.Maybe              (catMaybes)
import           Data.Set                (fromList, (\\))
import           Data.Typeable
import qualified NITTA.Compiler          as C
import           NITTA.ProcessUnits.Fram
import           NITTA.TestBench
import           NITTA.Timeline
import           NITTA.Types
import           NITTA.Utils
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Debug.Trace


data Alg pu v t = Alg
  { algFB     :: [FB v]
  , algValues :: [(v, Int)]
  , algPu     :: pu Passive v t
  }

instance ( Show v, Show t, Show (pu Passive v t) ) => Show (Alg pu v t) where
  show Alg{..} = "alg = [\n" ++ concatMap (\x -> "  " ++ show x ++ ",\n") algFB  ++ "  ]\n"
    ++ "Values: " ++ show algValues




prop_simulation (Alg _alg values pu) = monadicIO $ do
  res <- run $ testBench pu values
  run $   timeline "resource/data.json" pu
  assert res



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



bindAllAndNaiveSteps (Alg alg values pu) =
  Alg alg values (C.bindAllAndNaiveSteps pu alg)



naiveGen pu alg = naiveGen' pu alg []

naiveGen' pu alg passedAlg = do
  s1 <- choose (0 :: Int, 1)
  let opts =
        -- trace ("vars: " ++ show (variants pu) ++ "\n"
        --         ++ "cells:\n" ++ concatMap ((++ "\n") . show) (assocs $ frMemory pu)
        --         ++ "remains:\n" ++ concatMap ((++ "\n") . show) (frRemains pu)
        --       ) $
        options pu
  case s1 of
    0 | not $ null opts -> do
          i <- choose (0, length opts - 1)
          let opt = opts !! i
          let vs = variables opt
          opt' <- if isPull opt
                  then do
                    vs' <- suchThat (sublistOf vs) (not . null)
                    return $ opt{ eoEffect=Pull vs' }
                  else return opt
          let pu' =
                -- trace ("step: " ++ show opts' ++ " vs: " ++ show vs
                       -- ++ " tick: " ++ show (tick $ process pu)) $
                  step pu $ C.effectVar2act opt'
          naiveGen' pu' alg passedAlg
    1 | not $ null alg -> do
          i <- choose (0, length alg - 1)
          let fb = alg !! i
          let alg' = [ x | x <- alg, x /= fb ]
          case bind fb pu of
            Right pu' -> --trace ("bind: " ++ show fb) $
                         naiveGen' pu' alg' (fb : passedAlg)
            Left _r -> --trace ("skip: " ++ show fb ++ " reason: " ++ show r) $
                      naiveGen' pu alg' passedAlg
    _ | null opts && null alg -> return (pu, passedAlg)
    _ -> naiveGen' pu alg passedAlg
