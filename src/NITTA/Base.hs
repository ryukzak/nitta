{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module NITTA.Base where

import           Control.Monad.State
import           Data.Default
import           Data.List            (find, intersect, partition)
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe, isJust, catMaybes)
import           Data.Typeable        (Typeable, cast, typeOf)
import           NITTA.FunctionBlocks
import qualified NITTA.FunctionBlocks as FB


type ProcessUid = Int

class ( Default t, Num t, Bounded t, Ord t, Show t, Typeable t ) => Time t
instance ( Default t, Num t, Bounded t, Ord t, Show t, Typeable t ) => Time t

class ( Typeable v, Eq v, Ord v, Show v ) => Var v
instance ( Typeable v, Eq v, Ord v, Show v ) => Var v

data Value = X | V Int | B Bool | Broken
  deriving (Show)

X +++ v = v
v +++ X = v
_ +++ _ = Broken






data Effect v
  = Push v
  | Pull [v]
  deriving (Show, Eq)





data Passive

class PUType t where
  data Variant t :: * -> * -> *
  data Action t :: * -> * -> *



data Interaction mt v t = Interaction { effect :: Effect v
                                      , at     :: mt t
                                      } deriving (Show)

instance PUType Passive where
  data Variant Passive v t
    = PUVar
    { vEffect :: Effect v
    , vAt :: TimeConstrain t
    } deriving (Show)
  data Action Passive v t
    = PUAct
    { aEffect :: Effect v
    , aAt :: Event t
    } deriving (Show)





class ( Typeable (Signals pu)
      , ProcessInfo (Instruction pu v t)
      ) => PUClass pu ty v t where
  evaluate :: pu ty v t -> FB v -> Maybe (pu ty v t)
  variants :: pu ty v t -> [Variant ty v t]
  step     :: pu ty v t -> Action ty v t -> pu ty v t

  process :: pu ty v t -> Process v t

  data Instruction pu v t :: *
    
  data Signals pu :: *
  signal :: pu ty v t -> S -> t -> Value
  signal' :: pu ty v t -> Signals pu -> t -> Value

  signal pu (S s) = let s' = fromMaybe (error "Wrong signal!") $ cast s
                    in signal' pu s'


data S where
  S :: Typeable (Signals a) => Signals a -> S





data PU ty v t where
  PU :: ( PUClass pu ty v t
        ) => pu ty v t -> PU ty v t



deriving instance Show (Instruction PU v t) 
instance ( Var v, Time t ) => ProcessInfo (Instruction PU v t)

instance ( Var v, Time t ) => PUClass PU Passive v t where
  evaluate (PU pu) fb = PU <$> evaluate pu fb
  variants (PU pu) = variants pu
  step (PU pu) act = PU $ step pu act
  process (PU pu) = process pu
    -- let p@Process{..} = process pu
    -- in Process{ tick=tick
              -- , nextUid=def
              -- , steps=map (\st@Step{..} -> st{ key=genericKey key }) steps
              -- , relations=map (\case
                                  -- (Seq lst) -> Seq $ map genericKey lst
                                  -- (Vertical a b) -> Vertical (genericKey a) (genericKey b)
                              -- ) relations
              -- }
  data Signals PU = Signals ()
  data Instruction PU v t
    
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




data Process v t
  = Process
    { tick      :: t
    , nextUid   :: ProcessUid
    , steps     :: [Step v t]
    , relations :: [Relation]
    } deriving (Show)

instance (Time t) => Default (Process v t) where
  def = Process { tick=def
                , nextUid=def
                , steps=[]
                , relations=[]
                }


class ( Show i, Typeable i ) => ProcessInfo i
instance ProcessInfo ()

instance ProcessInfo String
instance (Var v) => ProcessInfo (FB v)
instance (Var v) => ProcessInfo (Effect v)
  

data Step v t where
  Step :: ( ProcessInfo info ) =>
    { uid  :: ProcessUid
    , time :: Event t
    , info :: info
    } -> Step v t


instance Default (Step v t) where
  def = Step def undefined ()
  
deriving instance ( Show v, Show t ) => Show (Step v t)







data Relation = Seq [ProcessUid]
                  | Vertical ProcessUid ProcessUid
                  deriving (Show, Eq)



modifyProcess p state = runState state p

add time info = do
  p@Process{..} <- get
  put p { nextUid=succ nextUid
        , steps=Step nextUid time info : steps
        }
  return nextUid

relation r = do
  p@Process{..} <- get
  put p{ relations=r : relations }

setTime t = do
  p <- get
  put p{ tick=t }

whatsHappen t = filter (\Step{ time=Event{..} } -> eStart <= t && t <= eStart + eDuration)
infoAt t = catMaybes . map (\Step{..} -> cast info) . whatsHappen t
