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
import           Data.Set                (fromList, (\\))
import qualified NITTA.Compiler          as C
import           NITTA.ProcessUnits.Fram
import           NITTA.TestBench
import           NITTA.Timeline
import           NITTA.Types
import           NITTA.Utils
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Debug.Trace


data DataFlow pu v t = DataFlow
  { dfFB     :: [FB Parcel v]
  , dfValues :: [(v, Int)]
  , dfPU     :: pu
  }

instance ( Show v, Show t, Show pu ) => Show (DataFlow pu v t) where
  show DataFlow{..} = "data flow = [\n" ++ concatMap (\x -> "  " ++ show x ++ ",\n") dfFB  ++ "  ]\n"
    ++ "Values: " ++ show dfValues




prop_simulation (DataFlow _df values pu) = monadicIO $ do
  res <- run $ testBench pu values
  run $ timeline "resource/data.json" pu
  assert res



prop_formalCompletness (DataFlow df _values pu) =
  let vars = concatMap variables df
      p = process pu
      vars' = concatMap variables $ getEffects p
      df' = getFBs p
  in if and
        [ fromList vars == fromList vars'
        , length (nub vars') == length vars'
        , length (nub df') == length df'
        , fromList df == fromList df'
        ]
     then True
     else
       trace ( "vars: " ++ show (fromList vars \\ fromList vars') ++ "\n"
             ++ "fbs: " ++ show (fromList df \\ fromList df') ++ "\n"
             ++ "cells:\n" ++ concatMap ((++ "\n") . show) (assocs $ frMemory pu)
             ++ "remains:\n" ++ concatMap ((++ "\n") . show) (frRemains pu)
             )
       False



bindAllAndNaiveSchedule (DataFlow df values pu) =
  DataFlow df values (C.bindAllAndNaiveSchedule pu df)



naiveGen pu df = naiveGen' pu df []

naiveGen' pu df passedDF = do
  s1 <- choose (0 :: Int, 1)
  let opts = options pu
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
                  select pu $ C.passiveOption2action opt'
          naiveGen' pu' df passedDF
    1 | not $ null df -> do
          i <- choose (0, length df - 1)
          let fb = df !! i
          let df' = [ x | x <- df, x /= fb ]
          case bind fb pu of
            Right pu' -> --trace ("bind: " ++ show fb) $
                         naiveGen' pu' df' (fb : passedDF)
            Left _r -> --trace ("skip: " ++ show fb ++ " reason: " ++ show r) $
                      naiveGen' pu df' passedDF
    _ | null opts && null df -> return (pu, passedDF)
    _ -> naiveGen' pu df passedDF
