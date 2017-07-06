{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module NITTA.Base where

import           Control.Monad.State
import           Data.Default
import           Data.List            (find, intersect, partition)
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe, isJust)
import           Data.Typeable        (Typeable, cast, typeOf)
import           NITTA.FunctionBlocks
import qualified NITTA.FunctionBlocks as FB




class ( Default t, Num t, Bounded t, Ord t, Show t ) => Time t
instance ( Default t, Num t, Bounded t, Ord t, Show t ) => Time t

class ( Typeable v, Eq v, Ord v, Show v ) => Var v
instance ( Typeable v, Eq v, Ord v, Show v ) => Var v

class ( Default k, Ord k, Enum k, Show k ) => Key k
instance ( Default k, Ord k, Enum k, Show k ) => Key k


data Value = X | V Int | B Bool

-- v - var; t - time; k - key
class (Typeable (Signals pu)) => PUClass pu variant action step v t k where
  evaluate :: pu variant action step v t k -> FB v -> Maybe (pu variant action step v t k)
  variants :: pu variant action step v t k -> [variant v t]
  step     :: pu variant action step v t k -> action v t -> pu variant action step v t k
  process  :: pu variant action step v t k -> Process step v t k

  data Signals pu :: *
  signal :: pu variant action step v t k -> S -> t -> Value
  signal' :: pu variant action step v t k -> Signals pu -> t -> Value

  signal pu (S s) = let s' = fromMaybe (error "Wrong signal!") $ cast s
                    in signal' pu s'


data S where
  S :: Typeable (Signals a) => Signals a -> S


data PU variant action step v t k where
  PU :: (PUClass pu variant action step v t k
        ) => pu variant action step v t k -> PU variant action step v t k

instance PUClass PU variant action step v t k where
  evaluate (PU pu) fb = PU <$> evaluate pu fb
  variants (PU pu) = variants pu
  step (PU pu) act = PU $ step pu act
  process (PU pu) = process pu
  data Signals PU
  signal' = error ""




data TimeConstrain t
  = TimeConstrain
  { tcDuration :: t
  , tcFrom     :: t
  , tcTo       :: t
  } deriving (Show, Eq)

data Event t
  = Event
  { eStart    :: t
  , eDuration :: t
  } deriving (Show, Eq)




data Process step v t k
  = Process
    { tick      :: t
    , nextStep  :: k
    , steps     :: [step v t k]
    , relations :: [Relation k]
    } deriving (Show)

instance (Key k, Time t) => Default (Process step v t k) where
  def = Process { tick=def
                , nextStep=def
                , steps=[]
                , relations=[]
                }




data Step info v t k =
  Step { time :: Event t
       , info :: info v
       , key  :: k
       } deriving (Show)

instance (Key k) => Default (Step info v t k) where
  def = Step undefined undefined def


data NestedStep info nId nInfo v t k
  = NStep { nTime :: Event t
          , nInfo :: info v
          , nKey  :: k
          }
  | Nested { nId    :: nId
           , nested :: nInfo v t k
           , nKey   :: k
           } deriving (Show)

instance (Key k) => Default (NestedStep info nId nInfo v t k) where
  def = NStep undefined undefined def




class Level a where
  level :: a -> String




data Relation key = Seq [key]
                  | Vertical key key
                  deriving (Show, Eq)




modifyProcess p state = runState state p

add st = do
  p@Process{ nextStep=key, .. } <- get
  put p { nextStep=succ key
        , steps=st key : steps
        }
  return key

add' desc time = do
  p@Process{ nextStep=key, .. } <- get
  put p { nextStep=succ key
        , steps=Step time desc key : steps
        }
  return key

relation r = do
  p@Process{..} <- get
  put p{ relations=r : relations }

setTime t = do
  p <- get
  put p{ tick=t }

whatsHappen t = filter (\Step{ time=Event{..} } -> eStart <= t && t <= eStart + eDuration)
