{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Types where

import           Data.Default
-- import           Data.Functor.Const
import qualified Data.List     as L
import qualified Data.Map      as M
import           Data.Maybe
import           Data.Typeable



data TaggetTime tag t
  = TaggetTime
  { tag   :: Maybe tag
  , clock :: t
  } deriving ( Show, Typeable )

instance ( Eq t ) => Eq (TaggetTime tag t) where
  (TaggetTime _ a) == (TaggetTime _ b) = a == b
instance ( Ord t ) => Ord (TaggetTime tag t) where
  (TaggetTime _ a) `compare` (TaggetTime _ b) = a `compare` b

instance ( Default t ) => Default (TaggetTime tag t) where
  def = TaggetTime Nothing def

instance ( Enum t ) => Enum (TaggetTime tag t) where
  toEnum i = TaggetTime Nothing $ toEnum i
  fromEnum (TaggetTime _ i) = fromEnum i

instance ( Num t ) => Bounded (TaggetTime tag t) where
  minBound = TaggetTime Nothing 0
  maxBound = TaggetTime Nothing 1000

instance ( Num t, Show tag, Eq tag ) => Num (TaggetTime tag t) where
  (TaggetTime Nothing a) + (TaggetTime Nothing b) = TaggetTime Nothing (a + b)
  (TaggetTime (Just tag) a) + (TaggetTime Nothing b) = TaggetTime (Just tag) (a + b)
  (TaggetTime Nothing a) + (TaggetTime (Just tag) b) = TaggetTime (Just tag) (a + b)
  (TaggetTime tag_a a) + (TaggetTime tag_b b)
    | tag_a == tag_b = TaggetTime tag_a (a + b)
    | otherwise = error $ "Not equal time tag! " ++ show tag_a ++ " " ++ show tag_b
  fromInteger = TaggetTime Nothing . fromInteger
  negate t = t{ clock=negate $ clock t }
  (*) = undefined
  abs = undefined
  signum = undefined


-- setTag Nothing (TaggetTime _ t) = TaggetTime Nothing t
-- setTag (Just tag) (TaggetTime _ t) = TaggetTime (Just tag) t
-- getTag (TaggetTime tag _) = tag


class ( Show (fb v), Eq (fb v), Ord (fb v), Vars (fb v) v, Typeable (fb v)
      ) => FBClass fb v where
  dependency :: fb v -> [(v, v)]
  insideOut :: fb v -> Bool
  insideOut _ = False
  isCritical :: fb v -> Bool
  isCritical _ = False

class WithFunctionalBlocks a v | a -> v where
  functionalBlocks :: a -> [FB v]

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




class ( Typeable v, Eq v, Ord v, Show v
      -- , ToString v
      ) => Var v
instance ( Typeable v, Eq v, Ord v, Show v
         -- , ToString v
         ) => Var v

class Vars a v | a -> v where
  variables :: a -> [v]

-- class ToString a where toString :: a -> String
-- instance ToString String where toString s = s



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
    } deriving ( Show )

instance (Time t) => Default (Process v t) where
  def = Process { tick=def
                , nextUid=def
                , steps=[]
                , relations=[]
                }



data Step v t where
  Step ::
    { uid  :: ProcessUid
    , time :: Event t
    , info :: StepInfo v
    } -> Step v t
  deriving ( Show )


data StepInfo v where
  FBStep :: FB v -> StepInfo v
  InfoStep :: String -> StepInfo v
  EffectStep :: Effect v -> StepInfo v
  InstructionStep :: ( Show (Instruction pu)
                     , Typeable (Instruction pu)
                     ) => Instruction pu -> StepInfo v
  NestedStep :: ( Eq title, Show title, Ord title
                ) => title -> StepInfo v -> StepInfo v




deriving instance ( Var v ) => Show (StepInfo v)

level (FBStep _)          = "Function block"
level (InfoStep _)        = "Info"
level (EffectStep _)      = "Effect"
level (InstructionStep _) = "Instruction"
level (NestedStep _ _)    = "Nested"



data Relation = Vertical ProcessUid ProcessUid
              deriving (Show, Eq)




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
instance Vars (Action Passive v t) v where
  variables EffectAct{..} = variables eaEffect



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
      , Typeable (Instruction pu)
      ) => PUClass ty pu v t | pu -> ty, pu -> v, pu -> t where
  bind :: FB v -> pu -> Either String pu
  options :: pu -> [Option ty v t]
  select :: pu -> Action ty v t -> pu

  process :: pu -> Process v t
  setTime :: t -> pu -> pu

class ( Typeable pu ) => Controllable pu t | pu -> t where
  data Instruction pu :: *

  data Signals pu :: *
  signal :: pu -> S -> t -> Value
  signal pu (S s) = let s' = fromMaybe (error "Wrong signal!") $ cast s
                    in signal' pu s'
  signal' :: pu -> Signals pu -> t -> Value

  proxy :: pu -> Proxy pu
  proxy _ = Proxy

class Similatable pu v x | pu -> v, pu -> x where
  varValue :: pu -> SimulationContext v x -> (v, x) -> x
  variableValue :: FB v -> pu -> SimulationContext v x -> (v, x) -> x



data S where
  S :: Typeable (Signals a) => Signals a -> S



data PU ty v t where
  PU :: ( PUClass ty pu v t
        , Typeable pu
        , Similatable pu v Int
        , Controllable pu t
        ) => pu -> PU ty v t

-- deriving instance Show (Instruction (PU Passive v t))

instance ( Var v, Time t ) => PUClass Passive (PU Passive v t) v t where
  bind fb (PU pu) = PU <$> bind fb pu
  options (PU pu) = options pu
  select (PU pu) act = PU $ select pu act
  process (PU pu) = process pu
  setTime t (PU pu) = PU $ setTime t pu


instance ( PUClass Passive (PU Passive v t) v t
         ) => Similatable (PU Passive v t) v Int where
  varValue (PU pu) cntx vi = varValue pu cntx vi
  variableValue fb (PU pu) cntx vi = variableValue fb pu cntx vi




type SimulationContext v x = M.Map (v, Int) x

