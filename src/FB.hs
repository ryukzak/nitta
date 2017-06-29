{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module FB where

import           Data.Dynamic  (Dynamic, fromDynamic, toDyn)
import           Data.Typeable (Typeable, cast, typeOf)

class Vars a var | a -> var where
  variables :: a -> [var]

class ( Show fb
      , Typeable fb
      , Eq fb
      , Ord fb
      , Vars fb var
      ) => FBClass fb var | fb -> var where
  dependency :: fb ->[(var, var)]

data FB var where
  FB :: ( FBClass fb var
        ) => fb -> FB var

instance Vars (FB var) var where
  variables (FB fb) = variables fb

instance (Typeable var) => FBClass (FB var) var where
  dependency (FB fb) = dependency fb

instance Show (FB var) where show (FB x) = show x

instance Eq (FB var) where
  FB a == FB b = Just a == cast b

instance Ord (FB var) where
  FB a `compare` FB b = case cast b of
    Just b' -> a `compare` b'
    Nothing -> typeOf a `compare` typeOf b


unbox (FB x) = fromDynamic $ toDyn x



-- data Loop v = Loop [v] v deriving (Show, Typeable, Eq, Ord)

data FRAMInput addr v = FRAMInput addr [v] deriving (Show, Typeable, Eq, Ord)
instance Vars (FRAMInput addr var) var where
  variables (FRAMInput _ vs) = vs
instance (Show addr, Eq addr, Ord addr, Typeable addr
         , Typeable var, Show var, Eq var, Ord var
         ) => FBClass (FRAMInput addr var) var where
  dependency _ = []
framInput (addr :: Int) vs = FB $ FB.FRAMInput addr vs


data FRAMOutput addr v = FRAMOutput addr v deriving (Show, Typeable, Eq, Ord)
instance Vars (FRAMOutput addr var) var where
  variables (FRAMOutput _ v) = [v]
instance (Show addr, Eq addr, Ord addr, Typeable addr
         , Typeable var, Show var, Eq var, Ord var
         ) => FBClass (FRAMOutput addr var) var where
  dependency _ = []
framOutput (addr :: Int) v = FB $ FB.FRAMOutput addr v



data Reg v = Reg v [v] deriving (Show, Typeable, Eq, Ord)
instance Vars (Reg var) var where
  variables (Reg a b) = a : b
instance (Typeable var, Show var, Eq var, Ord var) => FBClass (Reg var) var where
  dependency (Reg a b) = map (, a) b
reg a b = FB $ Reg a b







-- class (Typeable a) => FunctionBlock a where
  -- box :: a -> Dynamic
  -- box = toDyn

-- instance FunctionBlock FB where box (FB x) = toDyn x
-- instance (Typeable v) => FunctionBlock (Reg v)

--unbox :: forall fb. FB -> fb
-- unbox (FB x) = fromDynamic (box x)
-- test = let
-- test fb | Just (Reg a b) <- fromDynamic fb = a + sum b + 1

-- wrap = let fb = reg 1 [2,3]
           -- in test $ box fb

-- data Input v = Input v deriving Show
-- data Output v = Output v deriving Show

-- class FBFamily v where
  -- data FB' v :: *

--  box :: FB' v -> Dynamic

