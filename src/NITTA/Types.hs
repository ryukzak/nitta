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


class ( Typeable fb
      , Eq fb
      -- , Ord fb
      -- , Var v
      ) => FBClass fb v | fb -> v where
  dependency :: fb -> [(v, v)]
  insideOut :: fb -> Bool
  insideOut _ = False
  isCritical :: fb -> Bool
  isCritical _ = False

class WithFunctionalBlocks x io v | x -> io, x -> v where
  functionalBlocks :: x -> [FB io v]





class Variables x v | x -> v where
  variables :: x -> [v]

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
    }


instance (Time t) => Default (Process v t) where
  def = Process { tick=def
                , nextUid=def
                , steps=[]
                , relations=[]
                }



data Step v t where
  Step ::
    { sKey  :: ProcessUid
    , sTime :: Event t
    , sDesc :: StepInfo v
    } -> Step v t

data StepInfo v where
  FBStep :: FB Parcel v -> StepInfo v
  InfoStep :: String -> StepInfo v
  EffectStep :: Effect v -> StepInfo v
  InstructionStep :: ( Show (Instruction pu)
                     , Typeable (Instruction pu)
                     ) => Instruction pu -> StepInfo v
  NestedStep :: ( Eq title, Show title, Ord title
                ) => title -> StepInfo v -> StepInfo v

deriving instance ( Var v, Time t ) => Show ( Process v t )
deriving instance ( Var v, Time t ) => Show ( Step v t )
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
    }
  data Action Passive v t
    = EffectAct
    { eaEffect :: Effect v
    , eaAt :: Event t
    }

deriving instance ( Var v, Time t ) => Show (Option Passive v t)
deriving instance ( Var v, Time t ) => Show (Action Passive v t)

instance ( Var v ) => Variables (Option Passive v t) v where
  variables EffectOpt{..} = variables eoEffect
instance ( Var v ) => Variables (Action Passive v t) v where
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
instance Variables (Option (Network title) v t) v where
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
  deriving ( Show, Eq, Ord )
-- deriving instance ( Variable v ) => Show (Effect v)
-- deriving instance ( Variable v ) => Eq (Effect v)
-- deriving instance ( Variable v ) => Ord (Effect v)

instance ( Var v ) => Variables (Effect v) v where
  variables (Push i) = [i]
  variables (Pull o) = o


-- (Push a) `subset` (Push b) | a == b = True
-- (Pull a) `subset` (Pull b)
--   = let as = variables a
--         bs = variables b
--     in (length $ bs L.\\ as) == length as - length bs
-- _ `subset` _ = False

(Push a) << (Push b) | a == b = True
(Pull a) << (Pull b)          = all (`elem` a) b
_        << _                 = False

(Pull a) \\\ (Pull b) = Pull (a L.\\ b)
_ \\\ _ = error "Only for Pulls"




class ( Typeable (Signals pu)
      , Typeable (Instruction pu)
      ) => PUClass ty pu v t | pu -> ty, pu -> v, pu -> t where
  bind :: FB Parcel v -> pu -> Either String pu
  options :: pu -> [Option ty v t]
  select :: pu -> Action ty v t -> pu

  process :: pu -> Process v t
  setTime :: t -> pu -> pu

class ( Typeable pu ) => Controllable pu where
  data Instruction pu :: *
  data Signals pu :: *

  -- идея конечно хороша и крассива, но на практике не реализуема, так как:
  -- - исключает параллелизм инсрукций (что особенно актуально для Net.
  -- signalValue :: Instruction pu -> Signals pu -> Value

  proxy :: pu -> Proxy pu
  proxy _ = Proxy


class ByTime pu t | pu -> t where
  signalAt :: pu -> Signals pu -> t -> Value



gsignalAt pu (S s) = let s' = fromMaybe (error "Wrong signal!") $ cast s
                     in signalAt pu s'

class ByInstruction pu where
  signalFor :: Instruction pu -> Signals pu -> Value

gsignalFor instr (S s) = let s' = fromMaybe (error "Wrong signal!") $ cast s
                         in signalFor instr s'



class Similatable pu v x | pu -> v, pu -> x where
  varValue :: pu -> SimulationContext v x -> (v, x) -> x
  variableValue :: FB Parcel v -> pu -> SimulationContext v x -> (v, x) -> x



data S where
  S :: Typeable (Signals a) => Signals a -> S



data PU ty v t where
  PU :: ( PUClass ty pu v t
        , Typeable pu
        , Similatable pu v Int
        , Controllable pu
        , ByTime pu t
        ) => pu -> PU ty v t


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




class IOTypeFamily io where
  data I io :: * -> *
  data O io :: * -> *

class ( Show (I io v), Variables (I io v) v, Eq (I io v) -- , Ord (I io v)
      , Show (O io v), Variables (O io v) v, Eq (O io v) -- , Ord (O io v)
      , Typeable io, Var v
      ) => IOType io v
instance ( Show (I io v), Variables (I io v) v, Eq (I io v) -- , Ord (I io v)
         , Show (O io v), Variables (O io v) v, Eq (O io v) -- , Ord (O io v)
         , Typeable io, Var v
         ) => IOType io v



class ( Typeable v, Eq v, Ord v, Show v ) => Var v
instance ( Typeable v, Eq v, Ord v, Show v ) => Var v

data Parcel = Parcel

instance IOTypeFamily Parcel where
  data I Parcel v = I v  -- Incoming Parcel
    deriving (Show, Eq, Ord)
  data O Parcel v = O [v]  -- Outgoing Parcel
    deriving (Show, Eq, Ord)

instance Variables (I Parcel v) v where
  variables (I v) = [v]
instance Variables (O Parcel v) v where
  variables (O v) = v





data FB box v where
  FB :: ( FBClass fb v
        , Show fb
        , Variables fb v
        , IOType io v
        ) => fb -> FB io v

instance ( IOType box v, Var v ) => FBClass (FB box v) v where
  dependency (FB fb) = dependency fb
  insideOut (FB fb) = insideOut fb
  isCritical (FB fb) = isCritical fb

deriving instance ( Show v ) => Show (FB box v)

instance Variables (FB Parcel v) v where
  variables (FB fb) = variables fb

instance Eq (FB box v) where
  FB a == FB b = Just a == cast b

instance ( Variables (FB box v) v, Var v ) => Ord (FB box v) where
  a `compare` b = variables a `compare` variables b

