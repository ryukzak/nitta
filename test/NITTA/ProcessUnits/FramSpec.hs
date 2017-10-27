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
import           NITTA.FunctionBlocks
import           NITTA.FunctionBlocksSpec
import           NITTA.ProcessUnits.Fram
import           NITTA.ProcessUnitsSpec
import           NITTA.Types
import           Test.QuickCheck



type FramIdealDataFlow = DataFlow (Fram String Int) String Int


data FramDataFlow = FramDataFlow{ framDataFlow :: FramIdealDataFlow }

instance Show FramDataFlow where
  show = show . framDataFlow


data ST = ST { acc           :: [FB Parcel String]
             , forInput      :: [Int]
             , forOutput     :: [Int]
             , numberOfLoops :: Int
             , usedVariables :: [String]
             , values        :: [(String, Int)]
             } deriving (Show)


instance Arbitrary FramDataFlow where
  arbitrary = do
    ST{..} <- framDataFlowGen False (const True)
    (pu', df') <- naiveGen def acc
    return $ FramDataFlow $ DataFlow df' values pu'


instance Arbitrary FramIdealDataFlow where
  arbitrary = do
    ST{..} <- framDataFlowGen True (\ST{..} -> (length forInput + numberOfLoops) > 0)
    return $ DataFlow (sortBy compareFB acc) values def{ frAllowBlockingInput=False }
      where
        compareFB a b
          | Just (Reg _ _) <- castFB a = GT
          | Just (Reg _ _) <- castFB b = LT
          | otherwise = EQ


framDataFlowGen checkCellUsage generalPred =
  suchThat (do
               size <- sized pure -- getSize
               n <- choose (0, size)
               foldM maker (ST [] [] [] 0 [] []) [ 0..n ]
           ) generalPred
  where
    maker st0@ST{..} _ = nextState st0 <$> do
      fb <- suchThat (oneof [ FB <$> (arbitrary :: Gen (FramInput Parcel String))
                            , FB <$> (arbitrary :: Gen (FramOutput Parcel String))
                            , FB <$> (arbitrary :: Gen (Loop Parcel String))
                            , FB <$> (arbitrary :: Gen (Reg Parcel String))
                            ]
                     ) check
      v <- choose (0 :: Int, 0xFF)
      return (fb, v)
        where
          nextState st (fb, v) = specificUpdate fb v st
            { acc=fb : acc
            , usedVariables=variables fb ++ usedVariables
            }
          specificUpdate fb value st
            | Just (FramInput addr _vs) <- castFB fb = st{ forInput=addr : forInput }
            | Just (FramOutput addr (I v)) <- castFB fb = st{ forOutput=addr : forOutput
                                                            , values=(v, value) : values
                                                            }
            | Just (Loop _bs (I a)) <- castFB fb = st{ numberOfLoops=numberOfLoops + 1
                                                     , values=(a, value) : values
                                                     }
            | Just (Reg (I a) _bs) <- castFB fb = st{ values=(a, value) : values }
            | otherwise = error $ "Bad FB: " ++ show fb
          check fb
            | not $ null (variables fb `intersect` usedVariables) = False
            | Just (Reg _ _ :: Reg Parcel String) <- castFB fb = True
            | not checkCellUsage = True
            | not (dfIoUses < framDefSize) = False
            | Just (FramInput addr _) <- castFB fb = addr `notElem` forInput
            | Just (FramOutput addr _) <- castFB fb = addr `notElem` forOutput
            | otherwise = True -- for Loop
          dfIoUses = length (nub $ forInput `union` forOutput) + numberOfLoops
