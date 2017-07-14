{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}

module NITTA.FunctionBlocks where

import           Data.Bits
import           Data.Dynamic  (Dynamic, fromDynamic, toDyn)
import           Data.Typeable (Typeable, cast, typeOf)



class ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, Bits a ) => Addr a
instance ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, Bits a ) => Addr a



class Vars a var | a -> var where
  variables :: a -> [var]



class ( Show fb, Typeable fb, Eq fb, Ord fb, Vars fb var
      ) => FBClass fb var | fb -> var where
  dependency :: fb ->[(var, var)]

data FB var where
  FB :: ( FBClass fb var ) => fb -> FB var

instance (Typeable var) => FBClass (FB var) var where
  dependency (FB fb) = dependency fb

instance Show (FB var) where show (FB x) = show x

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



-- data Loop v = Loop [v] v deriving (Show, Typeable, Eq, Ord)



data FRAMInput addr v = FRAMInput addr [v] deriving (Show, Typeable, Eq, Ord)
framInput (addr :: Int) vs = FB $ FRAMInput addr vs

instance Vars (FRAMInput addr var) var where
  variables (FRAMInput _ vs) = vs
instance (Show addr, Eq addr, Ord addr, Typeable addr
         , Typeable var, Show var, Eq var, Ord var
         ) => FBClass (FRAMInput addr var) var where
  dependency _ = []



data FRAMOutput addr v = FRAMOutput addr v deriving (Show, Typeable, Eq, Ord)
framOutput (addr :: Int) v = FB $ FRAMOutput addr v

instance Vars (FRAMOutput addr v) v where
  variables (FRAMOutput _ v) = [v]
instance (Show addr, Eq addr, Ord addr, Typeable addr
         , Typeable var, Show var, Eq var, Ord var
         ) => FBClass (FRAMOutput addr var) var where
  dependency _ = []



data Reg v = Reg v [v] deriving (Show, Typeable, Eq, Ord)
reg a b = FB $ Reg a b

instance Vars (Reg var) var where
  variables (Reg a b) = a : b
instance (Typeable var, Show var, Eq var, Ord var) => FBClass (Reg var) var where
  dependency (Reg a b) = map (, a) b
