{-# LANGUAGE Arrows                    #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}

module NITTA.FunctionBlocks where

import           Control.Arrow
import           Control.Arrow.Operations
import           Control.Arrow.Transformer.State
import           Data.Bits
import           Data.Dynamic                    (Dynamic, fromDynamic, toDyn)
import           Data.Typeable                   (Typeable, cast, typeOf)
import           NITTA.Types



class ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, Bits a ) => Addr a
instance ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, Bits a ) => Addr a


castFB :: ( Typeable v, Typeable (fb v) ) => FB v -> Maybe (fb v)
castFB (FB x) = cast x



----------------------------------------




data FramInput v = FramInput Int (O v) deriving ( Typeable )
deriving instance ( Variable v ) => Show (FramInput v)
deriving instance ( Variable v ) => Eq (FramInput v)
deriving instance ( Variable v ) => Ord (FramInput v)
framInput addr vs = FB $ FramInput addr vs

instance ( Variable v ) => Variables ( FramInput v ) v where
  variables (FramInput _ o) = variables o
instance ( Variable v ) => FBClass FramInput v where
  dependency _ = []
  isCritical _ = True




data FramOutput v = FramOutput Int (I v) deriving (Typeable)
deriving instance ( Variable v ) => Show (FramOutput v)
deriving instance ( Variable v ) => Eq (FramOutput v)
deriving instance ( Variable v ) => Ord (FramOutput v)
framOutput addr v = FB $ FramOutput addr v

instance ( Variable v ) => Variables (FramOutput v) v where
  variables (FramOutput _ i) = variables i
instance ( Variable v ) => FBClass FramOutput v where
  dependency _ = []
  isCritical _ = True



data Reg v = Reg (I v) (O v) deriving (Typeable)
deriving instance ( Variable v ) => Show (Reg v)
deriving instance ( Variable v ) => Eq (Reg v)
deriving instance ( Variable v ) => Ord (Reg v)
reg a b = FB $ Reg a b

instance ( Variable v ) =>  Variables (Reg v) v where
  variables (Reg a b) = variables a ++ variables b
instance ( Variable v ) => FBClass Reg v where
  dependency (Reg i o) = [ (a, b) | a <- variables i
                                  , b <- variables o
                                  ]



data Loop v = Loop (O v) (I v) deriving (Typeable)
deriving instance ( Variable v ) => Show (Loop v)
deriving instance ( Variable v ) => Eq (Loop v)
deriving instance ( Variable v ) => Ord (Loop v)
loop bs a = FB $ Loop bs a

instance ( Variable v ) =>  Variables (Loop v) v where
  variables (Loop b a) = variables a ++ variables b

instance ( Variable v ) => FBClass Loop v where
  dependency _ = []
  insideOut _ = True



-- data Mux v = Mux
--   { mSelect :: v
--   , mInputs :: [v]
--   , mOutput :: [v]
--   } deriving (Typeable, Show, Eq, Ord)
-- mux s as bs = FB $ Mux s as bs

-- instance Variables (Mux v) v where
--   variables Mux{..} = mSelect : mInputs ++ mOutput

-- instance ( Var v ) => FBClass Mux v where
--   dependency Mux{..} = [ (o, mSelect) | o <- mOutput ]
--                        ++ [ (o, i) | o <- mOutput, i <- mInputs ]



-- data BAdd v = BAdd v v [v]
--   deriving (Typeable, Show, Eq, Ord)
-- bAdd a b cs = FB $ bAdd a b cs

-- instance Variables (BAdd v) v where
--   variables (BAdd a b cs) = a : b : cs

-- instance ( Var v ) => FBClass BAdd v where
--   dependency (BAdd a b cs) = [ (c, a) | c <- cs ] ++ [ (c, b) | c <- cs ]









-- data Builder v
--   = Builder
--   { fbs :: [FB v]
--   , vs  :: [v]
--   } deriving ( Show )


-- framInputA :: Int -> StateArrow (Builder v) (->) () v
-- framInputA addr = proc _ -> do
--   bld@Builder{ vs=y:vs, .. } <- fetch -< ()
--   () <- store -< bld{ fbs=framInput addr [v] : fbs
--                     , vs=vs }
--   returnA -< y


-- regA :: StateArrow (Builder v) (->) v v
-- regA = proc b -> do
--   bld@Builder{ vs=c:vs } <- fetch -< ()
--   () <- store -< bld{ vs=vs }
--   returnA -< c
