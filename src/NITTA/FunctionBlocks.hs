{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.FunctionBlocks where

import           Data.Bits
import           Data.List     (find)
import qualified Data.Map      as M
import           Data.Maybe
import           Data.Typeable
import           NITTA.Types



class ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, Bits a ) => Addr a
instance ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, Bits a ) => Addr a


get cntx k = do
  values <- cntx M.!? k
  case values of
    []      -> Nothing
    value:_ -> Just value

pop cntx k = do
  value <- cntx `get` k
  let cntx' = M.adjust tail k cntx
  return (cntx', value)

push ks v cntx
  = return $ foldl (\cntx' k -> M.alter (maybe (Just [v]) (Just . (v:))) k cntx') cntx ks


-- ^ Симмулировать алгоритм.
simulateAlg cntx0 fbs
  = let cntx = M.fromList cntx0
    in inner cntx [] $ sort cntx fbs
  where
    step cntx fs
      = let f = maybe
              (error "Can't simulate algorithm, because can't define execution sequence or recived data is over.")
              id
              $ find (isJust . simulate cntx) fs
            fs' = filter (/= f) fs
            Just cntx' = simulate cntx f
        in (f, fs', cntx')

    sort _ [] = []
    sort cntx fs = let (f, fs', cntx') = step cntx fs
                   in f : sort cntx' fs'

    inner cntx [] fs0 = cntx : inner cntx fs0 fs0
    inner cntx fs fs0 = let (_f, fs', cntx') = step cntx fs
                        in inner cntx' fs' fs0



----------------------------------------


data FramInput io = FramInput Int (O io) deriving ( Typeable )
deriving instance IOType io v => Show (FramInput io)
deriving instance IOType io v => Eq (FramInput io)
framInput addr vs = FB $ FramInput addr vs

instance ( IOType io v ) => FunctionalBlock (FramInput io) v where
  outputs (FramInput _ o) = variables o
  isCritical _ = True
instance FunctionSimulation (FramInput (Parcel v)) v Int where
  simulate = error "Can't functional simulate FramInput!"



data FramOutput io = FramOutput Int (I io) deriving ( Typeable )
deriving instance IOType io v => Show (FramOutput io)
deriving instance IOType io v => Eq (FramOutput io)
framOutput addr v = FB $ FramOutput addr v

instance IOType io v => FunctionalBlock (FramOutput io) v where
  inputs (FramOutput _ o) = variables o
  isCritical _ = True
instance FunctionSimulation (FramOutput (Parcel v)) v Int where
  simulate = error "Can't functional simulate FramOutput!"



data Reg io = Reg (I io) (O io) deriving ( Typeable )
deriving instance IOType io v => Show (Reg io)
deriving instance IOType io v => Eq (Reg io)
reg a b = FB $ Reg a b

instance IOType io v => FunctionalBlock (Reg io) v where
  inputs  (Reg a _b) = variables a
  outputs (Reg _a b) = variables b
  dependency (Reg i o) = [ (b, a) | a <- variables i
                                  , b <- variables o
                                  ]
instance ( Ord v ) => FunctionSimulation (Reg (Parcel v)) v Int where
  simulate cntx (Reg (I k1) (O k2)) = do
    v <- cntx `get` k1
    push k2 v cntx



data Loop io = Loop (O io) (I io) deriving ( Typeable )
deriving instance ( IOType io v ) => Show (Loop io)
deriving instance ( IOType io v ) => Eq (Loop io)
loop bs a = FB $ Loop bs a

instance ( IOType io v ) => FunctionalBlock (Loop io) v where
  inputs  (Loop _a b) = variables b
  outputs (Loop a _b) = variables a
  insideOut _ = True
instance ( Ord v ) => FunctionSimulation (Loop (Parcel v)) v Int where
  simulate cntx (Loop (O k1) (I k2)) = do
    v <- cntx `get` k2
    push k1 v cntx



data Add io = Add (I io) (I io) (O io) deriving ( Typeable )
deriving instance IOType io v => Show (Add io)
deriving instance IOType io v => Eq (Add io)

instance IOType io v => FunctionalBlock (Add io) v where
  inputs  (Add  a  b _c) = variables a ++ variables b
  outputs (Add _a _b  c) = variables c
  dependency (Add a b c) = [ (y, x) | x <- variables a ++ variables b
                                    , y <- variables c
                                    ]
instance ( Ord v ) => FunctionSimulation (Add (Parcel v)) v Int where
  simulate cntx (Add (I k1) (I k2) (O k3)) = do
    v1 <- cntx `get` k1
    v2 <- cntx `get` k2
    let v3 = v1 + v2
    push k3 v3 cntx



data Constant io = Constant Int (O io) deriving ( Typeable )
deriving instance ( IOType io v ) => Show (Constant io)
deriving instance ( IOType io v ) => Eq (Constant io)

instance ( IOType io v ) => FunctionalBlock (Constant io) v where
  outputs (Constant _ o) = variables o
instance ( Ord v ) => FunctionSimulation (Constant (Parcel v)) v Int where
  simulate cntx (Constant v (O k))
    = push k v cntx



data ShiftL io = ShiftL (I io) (O io) deriving ( Typeable )
deriving instance ( IOType io v ) => Show (ShiftL io)
deriving instance ( IOType io v ) => Eq (ShiftL io)

instance ( IOType io v ) => FunctionalBlock (ShiftL io) v where
  outputs (ShiftL i o) = variables i ++ variables o
instance ( Ord v ) => FunctionSimulation (ShiftL (Parcel v)) v Int where
  simulate cntx (ShiftL (I k1) (O k2)) = do
    v1 <- cntx `get` k1
    let v2 = v1 `shiftR` 1
    push k2 v2 cntx



data Send io = Send (I io) deriving ( Typeable )
deriving instance ( IOType io v ) => Show (Send io)
deriving instance ( IOType io v ) => Eq (Send io)
instance ( IOType io v ) => FunctionalBlock (Send io) v where
  inputs (Send i) = variables i
instance FunctionSimulation (Send (Parcel String)) String Int where
  simulate cntx (Send (I k)) = do
    v <- cntx `get` k
    push [k ++ ".send"] v cntx



data Receive io = Receive (O io) deriving ( Typeable )
deriving instance ( IOType io v ) => Show (Receive io)
deriving instance ( IOType io v ) => Eq (Receive io)
instance ( IOType io v ) => FunctionalBlock (Receive io) v where
  outputs (Receive o) = variables o
instance FunctionSimulation (Receive (Parcel String)) String Int where
  simulate cntx (Receive (O ks)) = do
    (cntx', v) <- cntx `pop` (head ks ++ ".receive")
    push ks v cntx'
