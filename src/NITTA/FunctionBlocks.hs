{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.FunctionBlocks where

import           Data.Bits
import           Data.Typeable (Typeable, cast)
import           NITTA.Types
import qualified Data.Map as M
import           Data.List (find)
import           Data.Maybe



class ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, Bits a ) => Addr a
instance ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, Bits a ) => Addr a


castFB :: ( Typeable v, Typeable (fb io v) ) => FB io v -> Maybe (fb io v)
castFB (FB fb) = cast fb

boxFB :: ( FunctionalBlock (fb io v) v
         , FunctionSimulation (fb io v) v Int
         , Variables (fb io v) v
         , IOType io v
         , Show (fb io v)
         ) => fb io v -> FB io v
boxFB = FB


out cntx step vs x = return $ foldl (\st v -> M.insert (v, step) x st) cntx vs


-- ^ Симмулировать алгоритм
simulateAlg vs fbs
  = let initSt = M.fromList [ ((k, 0), v :: Int) | (k, v) <- vs ]
        inner st [] n = inner st fbs (n + 1)
        inner st fs n
          = let Just f = find (isJust . simulate st n) fs
                fs' = filter (/= f) fs
                Just st' = simulate st n f
            in st' : inner st' fs' n
    in inner initSt fbs 0


----------------------------------------


data FramInput io v = FramInput Int (O io v) deriving ( Typeable )
deriving instance IOType io v => Show (FramInput io v)
deriving instance IOType io v => Eq (FramInput io v)
framInput addr vs = FB $ FramInput addr vs

instance IOType io v => FunctionalBlock (FramInput io v) v where
  outputs (FramInput _ o) = variables o
  isCritical _ = True
instance FunctionSimulation (FramInput Parcel v) v Int where
  simulate = error "Can't functional simulate FramInput!"



data FramOutput io v = FramOutput Int (I io v) deriving ( Typeable )
deriving instance IOType io v => Show (FramOutput io v)
deriving instance IOType io v => Eq (FramOutput io v)
framOutput addr v = FB $ FramOutput addr v

instance IOType io v => FunctionalBlock (FramOutput io v) v where
  inputs (FramOutput _ o) = variables o
  isCritical _ = True
instance FunctionSimulation (FramOutput Parcel v) v Int where
  simulate = error "Can't functional simulate FramOutput!"



data Reg io v = Reg (I io v) (O io v) deriving ( Typeable )
deriving instance IOType io v => Show (Reg io v)
deriving instance IOType io v => Eq (Reg io v)
reg a b = FB $ Reg a b

instance IOType io v => FunctionalBlock (Reg io v) v where
  inputs  (Reg a _b) = variables a
  outputs (Reg _a b) = variables b
  dependency (Reg i o) = [ (b, a) | a <- variables i
                                  , b <- variables o
                                  ]
instance ( Ord v ) => FunctionSimulation (Reg Parcel v) v Int where
  simulate cntx step (Reg (I a) (O bs)) = do
    a' <- cntx M.!? (a, step)
    out cntx step bs a'



data Loop io v = Loop (O io v) (I io v) deriving ( Typeable )
deriving instance IOType io v => Show (Loop io v)
deriving instance IOType io v => Eq (Loop io v)
loop bs a = FB $ Loop bs a

instance IOType io v => FunctionalBlock (Loop io v) v where
  inputs  (Loop _a b) = variables b
  outputs (Loop a _b) = variables a
  insideOut _ = True
instance ( Ord v ) => FunctionSimulation (Loop Parcel v) v Int where
  simulate cntx step (Loop (O bs) (I a)) = do
    a' <- cntx M.!? (a, step)
    out cntx (step + 1) bs a'



data Add io v = Add (I io v) (I io v) (O io v) deriving ( Typeable )
deriving instance IOType io v => Show (Add io v)
deriving instance IOType io v => Eq (Add io v)

instance IOType io v => FunctionalBlock (Add io v) v where
  inputs  (Add  a  b _c) = variables a ++ variables b
  outputs (Add _a _b  c) = variables c
  dependency (Add a b c) = [ (y, x) | x <- variables a ++ variables b
                                    , y <- variables c
                                    ]
instance ( Ord v ) => FunctionSimulation (Add Parcel v) v Int where
  simulate cntx step (Add (I a) (I b) (O cs)) = do
    a' <- cntx M.!? (a, step)
    b' <- cntx M.!? (b, step)
    let c' = a' + b'
    out cntx step cs c'



data Constant io v = Constant Int (O io v) deriving ( Typeable )
deriving instance ( IOType io v ) => Show (Constant io v)
deriving instance ( IOType io v ) => Eq (Constant io v)

instance ( IOType io v ) => FunctionalBlock (Constant io v) v where
  outputs (Constant _ o) = variables o
instance ( Ord v ) => FunctionSimulation (Constant Parcel v) v Int where
  simulate cntx step (Constant x (O as))
    = out cntx step as x



data ShiftL io v = ShiftL (I io v) (O io v) deriving ( Typeable )
deriving instance ( IOType io v ) => Show (ShiftL io v)
deriving instance ( IOType io v ) => Eq (ShiftL io v)

instance ( IOType io v ) => FunctionalBlock (ShiftL io v) v where
  outputs (ShiftL i o) = variables i ++ variables o
instance ( Ord v ) => FunctionSimulation (ShiftL Parcel v) v Int where
  simulate cntx step (ShiftL (I a) (O bs)) = do
    a' <- cntx M.!? (a, step)
    let b' = a' `shiftR` 1
    out cntx step bs b'
