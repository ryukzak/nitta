{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}

module NITTA.FunctionBlocks where

import           Data.Bits
import           Data.Dynamic  (Dynamic, fromDynamic, toDyn)
import           Data.Typeable (Typeable, cast, typeOf)
import           NITTA.Types



class ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, Bits a ) => Addr a
instance ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, Bits a ) => Addr a





unbox (FB x) = fromDynamic $ toDyn x



----------------------------------------






data FramInput v = FramInput Int [v] deriving (Show, Typeable, Eq, Ord)
framInput addr vs = FB $ FramInput addr vs

instance Vars (FramInput v) v where
  variables (FramInput _ vs) = vs
instance ( Var v ) => FBClass FramInput v where
  dependency _ = []
  isCritical _ = True




data FramOutput v = FramOutput Int v deriving (Show, Typeable, Eq, Ord)
framOutput addr v = FB $ FramOutput addr v

instance Vars (FramOutput v) v where
  variables (FramOutput _ v) = [v]
instance ( Var v ) => FBClass FramOutput v where
  dependency _ = []
  isCritical _ = True



data Reg v = Reg v [v] deriving (Show, Typeable, Eq, Ord)
reg a b = FB $ Reg a b

instance Vars (Reg v) v where
  variables (Reg a b) = a : b
instance ( Var v ) => FBClass Reg v where
  dependency (Reg a b) = map (, a) b



data Loop v = Loop [v] v deriving (Typeable, Show, Eq, Ord)
loop bs a = FB $ Loop bs a

instance Vars (Loop v) v where
  variables (Loop bs a) = a : bs

instance ( Var v ) => FBClass Loop v where
  dependency _ = []
  insideOut _ = True
