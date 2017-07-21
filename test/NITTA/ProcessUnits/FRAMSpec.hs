{-# LANGUAGE TupleSections        #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module NITTA.ProcessUnits.FRAMSpec where

import           Control.Monad
import           Data.Default
import           Data.List                (intersect, notElem, nub, union)
import           Data.Typeable
import           NITTA.Base
import           NITTA.FunctionBlocks
import           NITTA.FunctionBlocksSpec ()
import           NITTA.ProcessUnits.FRAM
import           NITTA.ProcessUnitsSpec
import           NITTA.Types
import           Test.QuickCheck

type FramAlg = Alg FRAM String Int
type FramProcess = (Alg FRAM String Int, FRAM Passive String Int)

data ST = ST { acc           :: [FB String]
             , forInput      :: [Int]
             , forOutput     :: [Int]
             , numberOfLoops :: Int
             , usedVariables :: [String]
             , values        :: [(String, Int)]
             } deriving (Show)


instance {-# OVERLAPPING #-} Arbitrary FramProcess where
  arbitrary = do
    alg <- arbitrary
    pu <- naiveGen def (algFB alg)
    return (alg, pu)


instance Arbitrary FramAlg where
  arbitrary = (\ST{..} -> Alg acc values def) <$> suchThat (do
      size <- sized pure -- getSize
      n <- choose (0, size)
      foldM maker (ST [] [] [] 0 [] []) [ 0..n ])
              (\ST{..} -> (length forInput + numberOfLoops) > 0)
    where
      maker st0@ST{..} _ = nextState st0 <$> do
        fb <- suchThat (oneof [ FB <$> (arbitrary :: Gen (FRAMInput String))
                              , FB <$> (arbitrary :: Gen (FRAMOutput String))
                              , FB <$> (arbitrary :: Gen (Loop String))
                              , FB <$> (arbitrary :: Gen (Reg String))
                              ]
                       ) check
        v <- choose (0 :: Int, 0xFF)
        return (fb, v)
        where
          nextState st (fb, v) = specificUpdate fb v st
            { acc=fb : acc
            , usedVariables=variables fb ++ usedVariables
            }
          specificUpdate (FB fb) value st
            | Just (FRAMInput addr vs) <- cast fb = st{ forInput=addr : forInput
                                                      , values=[ (v, 0x0A00 + addr)
                                                               | v <- vs
                                                               ] ++ values
                                                      }
            | Just (FRAMOutput addr v) <- cast fb = st{ forOutput=addr : forOutput
                                                      , values=(v, 0x0A00 + addr) : values
                                                      }
            | Just (Loop a _bs) <- cast fb = st{ numberOfLoops=numberOfLoops + 1
                                               , values=(a, value) : values
                                               -- bs check making with independently with TestBench
                                              }
            | Just (Reg a bs) <- cast fb = st{ values=(a, value) : [(b, value) | b <- bs] ++ values
                                             }
            | otherwise = error $ "Bad FB: " ++ show fb
          check fb0@(FB fb)
            | not $ null (variables fb0 `intersect` usedVariables) = False
            | Just (Reg _ _ :: Reg String) <- cast fb = True
            | not (length (nub $ forOutput `union` forInput) + numberOfLoops < framSize) = False
            | Just (FRAMInput addr (_ :: [String])) <- cast fb = addr `notElem` forInput
            | Just (FRAMOutput addr (_ :: String)) <- cast fb = addr `notElem` forOutput
            | otherwise = True -- for Loop
