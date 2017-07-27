{-# LANGUAGE TupleSections        #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module NITTA.ProcessUnits.FramSpec where

import           Control.Monad
import           Data.Default
import           Data.List                (intersect, notElem, nub, sortBy,
                                           union)
import           Data.Typeable
import           NITTA.FunctionBlocks
import           NITTA.FunctionBlocksSpec ()
import           NITTA.ProcessUnits.Fram
import           NITTA.ProcessUnitsSpec
import           NITTA.Types
import           Test.QuickCheck

import           Debug.Trace

type FramIdealAlg = Alg Fram String Int


data FramAlg = FramAlg{ framAlg :: FramIdealAlg }

instance Show FramAlg where
  show = show . framAlg


data ST = ST { acc           :: [FB String]
             , forInput      :: [Int]
             , forOutput     :: [Int]
             , numberOfLoops :: Int
             , usedVariables :: [String]
             , values        :: [(String, Int)]
             } deriving (Show)


instance Arbitrary FramAlg where
  arbitrary = do
    ST{..} <- framAlgGen False (const True)
    (pu', alg') <- naiveGen def acc
    return $ FramAlg $ Alg alg' values pu'


instance Arbitrary FramIdealAlg where
  arbitrary = do
    ST{..} <- framAlgGen True (\ST{..} -> (length forInput + numberOfLoops) > 0)
    return $ Alg (sortBy compareFB acc) values def{ frAllowBlockingInput=False }
      where
        compareFB (FB a) (FB b)
          | Just (_ :: Reg String) <- cast a = GT
          | Just (_ :: Reg String) <- cast b = LT
          | otherwise = EQ


framAlgGen checkCellUsage generalPred =
  suchThat (do
               size <- sized pure -- getSize
               n <- choose (0, size)
               foldM maker (ST [] [] [] 0 [] []) [ 0..n ]
           ) generalPred
  where
    maker st0@ST{..} _ = nextState st0 <$> do
      fb <- suchThat (oneof [ FB <$> (arbitrary :: Gen (FramInput String))
                            , FB <$> (arbitrary :: Gen (FramOutput String))
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
            | Just (FramInput addr vs) <- cast fb = st{ forInput=addr : forInput
                                                      , values=[ (v, 0x0A00 + addr)
                                                               | v <- vs
                                                               ] ++ values
                                                      }
            | Just (FramOutput addr v) <- cast fb = st{ forOutput=addr : forOutput
                                                      , values=(v, 0x0A00 + addr) : values
                                                      }
            | Just (Loop _bs a) <- cast fb = st{ numberOfLoops=numberOfLoops + 1
                                               , values=(a, value) : values
                                                 -- bs check making with independently with TestBench
                                               }
            | Just (Reg a bs) <- cast fb = st{ values=(a, value) : [(b, value) | b <- bs] ++ values
                                             }
            | otherwise = error $ "Bad FB: " ++ show fb
          -- check fb = trace ("check ioUses: " ++ show ioUses
                            -- ++ " checkCellUsage: " ++ show checkCellUsage
                           -- ) $ check' fb
          check fb0@(FB fb)
            | not $ null (variables fb0 `intersect` usedVariables) = False
            | Just (Reg _ _ :: Reg String) <- cast fb = True
            | not checkCellUsage = True
            | not (ioUses < framSize) = False
            | Just (FramInput addr (_ :: [String])) <- cast fb = addr `notElem` forInput
            | Just (FramOutput addr (_ :: String)) <- cast fb = addr `notElem` forOutput
            | otherwise = True -- for Loop
          ioUses = length (nub $ forInput `union` forOutput) + numberOfLoops
