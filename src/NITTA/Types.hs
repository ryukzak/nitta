{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module NITTA.Types where


import           Data.Default
import qualified Data.List     as L
import qualified Data.Map      as M
import           Data.Maybe
import           Data.Typeable



class ( Show (fb v), Eq (fb v), Ord (fb v), Vars (fb v) v, Typeable (fb v)
      ) => FBClass fb v where
  dependency :: fb v -> [(v, v)]
  insideOut :: fb v -> Bool
  insideOut _ = False
  isCritical :: fb v -> Bool
  isCritical _ = False

data FB v where
  FB :: ( FBClass fb v, Typeable (fb v) ) => fb v -> FB v

instance (Typeable v) => FBClass FB v where
  dependency (FB fb) = dependency fb
  insideOut (FB fb) = insideOut fb
  isCritical (FB fb) = isCritical fb

deriving instance Show (FB v) --  where show (FB x) = show x

instance Vars (FB v) v where
  variables (FB fb) = variables fb

instance Eq (FB v) where
  FB a == FB b = Just a == cast b

instance Ord (FB v) where
  FB a `compare` FB b = case cast b of
    Just b' -> a `compare` b'
    Nothing -> typeOf a `compare` typeOf b




class ( Typeable v, Eq v, Ord v, Show v ) => Var v
instance ( Typeable v, Eq v, Ord v, Show v ) => Var v

class Vars a v | a -> v where
  variables :: a -> [v]




class ( Default t, Num t, Bounded t, Ord t, Show t, Typeable t, Enum t ) => Time t
instance ( Default t, Num t, Bounded t, Ord t, Show t, Typeable t, Enum t ) => Time t

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




type ProcessUid = Int

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



data Step v t where
  Step :: ( ProcessInfo info ) =>
    { uid  :: ProcessUid
    , time :: Event t
    , info :: info
    } -> Step v t

deriving instance ( Show v, Show t ) => Show (Step v t)

instance Default (Step v t) where
  def = Step def undefined ()



data Relation = Seq [ProcessUid]
                  | Vertical ProcessUid ProcessUid
                  deriving (Show, Eq)



class ( Show i, Typeable i ) => ProcessInfo i where
  level :: i -> String
  level = show . typeOf

instance ProcessInfo ()

instance ProcessInfo String where
  level _ = "Compiler"

instance {-# OVERLAPS #-}
  ( Typeable pu, Var v, Time t, Show (Instruction pu v t)
  ) => ProcessInfo (Instruction pu v t) where
  level _ = "Instruction"

instance (Var v) => ProcessInfo (FB v) where
  level _ = "Function block"

instance (Var v) => ProcessInfo (Effect v) where
  level _ = "Effect"




data Nested title v t where
  Nested :: ( ProcessInfo info ) =>
    { nestedUid  :: ProcessUid
    , nestedTitle :: title
    , nestedInfo :: info
    } -> Nested title v t

deriving instance ( Show title ) => Show (Nested title v t)

instance ( Typeable title
         , Show title
         , Var v, Time t
         ) => ProcessInfo (Nested title v t)




class PUType t where
  data Option t :: * -> * -> *
  data Action t :: * -> * -> *



data Passive

instance PUType Passive where
  data Option Passive v t
    = EffectOpt
    { eoEffect :: Effect v
    , eoAt :: TimeConstrain t
    } deriving (Show)
  data Action Passive v t
    = EffectAct
    { eaEffect :: Effect v
    , eaAt :: Event t
    } deriving (Show)

instance Vars (Option Passive v t) v where
  variables EffectOpt{..} = variables eoEffect



data Network title

instance PUType (Network title) where
  data Option (Network title) v t
    = TransportOpt
    { toPullFrom :: title
    , toPullAt   :: TimeConstrain t
    , toPush     :: M.Map v (Maybe (title, TimeConstrain t))
    } deriving (Show)
  data Action (Network title) v t
    = TransportAct
    { taPullFrom :: title
    , taPullAt   :: Event t
    , taPush     :: M.Map v (Maybe (title, Event t))
    } deriving (Show)

instance Vars (Option (Network title) v t) v where
  variables TransportOpt{..} = M.keys toPush





data Value = X | B Bool | Broken

instance Show Value where
  show X         = "x"
  show (B True)  = "1"
  show (B False) = "0"
  show Broken    = "B"

X +++ v = v
v +++ X = v
_ +++ _ = Broken




data Effect v
  = Push v
  | Pull [v]
  deriving (Show, Eq)

instance Vars (Effect v) v where
  variables (Push var)  = [var]
  variables (Pull vars) = vars

(Push a) << (Push b) | a == b = True
(Pull a) << (Pull b)          = all (`elem` a) b
_        << _                 = False

(Pull a) \\\ (Pull b) = Pull (a L.\\ b)
_ \\\ _ = error "Only for Pulls"




class ( Typeable (Signals pu)
      , ProcessInfo (Instruction pu v t)
      ) => PUClass pu ty v t where
  bind :: FB v -> pu ty v t -> Either String (pu ty v t)
  options :: pu ty v t -> [Option ty v t]
  step     :: pu ty v t -> Action ty v t -> pu ty v t

  process :: pu ty v t -> Process v t

  data Instruction pu v t :: *

  data Signals pu :: *
  signal :: pu ty v t -> S -> t -> Value
  signal pu (S s) = let s' = fromMaybe (error "Wrong signal!") $ cast s
                    in signal' pu s'
  signal' :: pu ty v t -> Signals pu -> t -> Value

data S where
  S :: Typeable (Signals a) => Signals a -> S




data PU ty v t where
  PU :: ( PUClass pu ty v t
        , Typeable (pu ty v t)
        ) => pu ty v t -> PU ty v t
        
deriving instance Show (Instruction PU v t)

instance ( Var v, Time t ) => PUClass PU Passive v t where
  bind fb (PU pu) = PU <$> bind fb pu
  options (PU pu) = options pu
  step (PU pu) act = PU $ step pu act
  process (PU pu) = process pu
  data Signals PU = Signals ()
  data Instruction PU v t
  signal' = error ""
