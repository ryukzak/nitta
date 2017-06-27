{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module FB
  ( Loop(..)
  , Reg(..)
  , FRAMInput(..)
  , FRAMOutput(..)
  , FB(..)
  , unbox
  , reg
  ) where

import Data.Typeable (Typeable)
import Data.Dynamic (toDyn, fromDynamic, Dynamic)



data FB where
  FB :: ( Show fb
        , Typeable fb
        ) => fb -> FB
instance Show FB where show (FB x) = show x
unbox (FB x) = fromDynamic $ toDyn x



data Loop v = Loop [v] v deriving (Show, Typeable)

data FRAMInput addr v = FRAMInput addr [v] deriving (Show, Typeable)

data FRAMOutput addr v = FRAMOutput addr v deriving (Show, Typeable)

data Reg v = Reg v [v] deriving (Show, Typeable)
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
  
