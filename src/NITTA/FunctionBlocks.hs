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




class ( Show fb, Eq fb, Ord fb, Vars fb var
      ) => FBClass fb var | fb -> var where
  dependency :: fb -> [(var, var)]
  insideOut :: fb -> Bool
  insideOut _ = False
  isCritical :: fb -> Bool
  isCritical _ = False

data FB v where
  FB :: ( FBClass fb v, Typeable fb, Typeable v ) => fb -> FB v

instance (Typeable v) => FBClass (FB v) v where
  dependency (FB fb) = dependency fb
  insideOut (FB fb) = insideOut fb
  isCritical (FB fb) = isCritical fb

deriving instance Show (FB var) --  where show (FB x) = show x

instance Vars (FB var) var where
  variables (FB fb) = variables fb

instance Eq (FB var) where
  FB a == FB b = Just a == cast b

instance Ord (FB var) where
  FB a `compare` FB b = case cast b of
    Just b' -> a `compare` b'
    Nothing -> typeOf a `compare` typeOf b

unbox (FB x) = fromDynamic $ toDyn x



----------------------------------------






data FRAMInput v = FRAMInput Int [v] deriving (Show, Typeable, Eq, Ord)
framInput addr vs = FB $ FRAMInput addr vs

instance Vars (FRAMInput var) var where
  variables (FRAMInput _ vs) = vs
instance ( Typeable var, Show var, Eq var, Ord var
         ) => FBClass (FRAMInput var) var where
  dependency _ = []
  isCritical _ = True




data FRAMOutput v = FRAMOutput Int v deriving (Show, Typeable, Eq, Ord)
framOutput addr v = FB $ FRAMOutput addr v

instance Vars (FRAMOutput v) v where
  variables (FRAMOutput _ v) = [v]
instance ( Typeable var, Show var, Eq var, Ord var
         ) => FBClass (FRAMOutput var) var where
  dependency _ = []
  isCritical _ = True



data Reg v = Reg v [v] deriving (Show, Typeable, Eq, Ord)
reg a b = FB $ Reg a b

instance Vars (Reg var) var where
  variables (Reg a b) = a : b
instance (Typeable var, Show var, Eq var, Ord var) => FBClass (Reg var) var where
  dependency (Reg a b) = map (, a) b



data Loop v = Loop v [v] deriving (Typeable, Show, Eq, Ord)
loop a b = FB $ Loop a b

instance Vars (Loop v) v where
  variables (Loop a b) = a : b

instance ( Var v ) => FBClass (Loop v) v where
  dependency (Loop a b) = []
  insideOut _ = True
