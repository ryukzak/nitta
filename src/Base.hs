{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Base where
import           Control.Monad.State
import           Data.Default
import           Data.List           (find, intersect, partition)
import qualified Data.List           as L
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe, isJust)
import           Data.Typeable       (Typeable, cast, typeOf)
import           FB
import qualified FB




class ( Num t, Bounded t, Ord t ) => Time t
instance ( Num t, Bounded t, Ord t ) => Time t

class ( Typeable v, Eq v, Ord v ) => Var v
instance ( Typeable v, Eq v, Ord v ) => Var v

class PUClass dpu variant action var time key
            -- | dpu -> variant action
            where
  evaluate :: dpu variant action var time key -> FB var -> Maybe (dpu variant action var time key)
  variants :: dpu variant action var time key -> [variant var time]
  step     :: dpu variant action var time key -> action var time -> dpu variant action var time key
  process  :: dpu variant action var time key -> Process var key time


data PU variant action v t k where
  PU :: (PUClass dpu variant action v t k) => dpu variant action v t k -> PU variant action v t k

instance PUClass PU variant action var time key where
  evaluate (PU dpu) fb = PU <$> evaluate dpu fb
  variants (PU dpu) = variants dpu
  step (PU dpu) act = PU $ step dpu act
  process (PU dpu) = process dpu




data Transport mtime title var time = Transport
  { pullFrom :: title
  , pullAt   :: time
  , push     :: M.Map var (Maybe (title, mtime time))
                      -- Nothing значит либо, операция ещё не распределена,
                      -- либо DPU не может его принять.
  } deriving (Show, Eq)



-- type TransportVariant title v t = (Transport title v (TimeConstrain t))
-- type TransportAction title v t = (Transport title v (Moment t))

data Action mtime v t = Action { act :: (v, mtime t) }
-- type Variant v t = (Interaction v, TimeConstrain t)
-- type Action v t = (Interaction v, Moment t)


-- data DPU key var time where
  -- DPU :: (DPUClass dpu key var time) => dpu -> DPU key var time

-- instance DPUClass (DPU key var time) key var time where
  -- evaluate (DPU dpu) fb = DPU <$> evaluate dpu fb
  -- variants (DPU dpu) = variants dpu
  -- step (DPU dpu) act begin end = DPU $ step dpu act begin end
  -- proc (DPU dpu) = proc dpu


data TimeConstrain t
  = TimeConstrain
  { tcDuration :: t
  , tcFrom     :: t
  , tcTo       :: t
  } deriving (Show, Eq)

-- rename to Event
data Moment t
  = Moment
  { eStart    :: t
  , eDuration :: t
  } deriving (Show, Eq)






data Interaction var
  = Push var
  | Pull [var]
  deriving (Show, Eq)

(Push a) << (Push b) | a == b = True
(Pull a) << (Pull b)          = all (`elem` a) b
_        << _                 = False

(Pull a) \\ (Pull b) = Pull (a L.\\ b)


instance Vars (Interaction var) var where
  variables (Push var)  = [var]
  variables (Pull vars) = vars


data Process var key time
  = Process
    { tick      :: time
    , keySeed   :: key
    , steps     :: [Step key time var]
    , relations :: [Relation key]
    } deriving (Show)


instance (Default key, Default time) => Default (Process var key time) where
  def = Process { tick=def
                , keySeed=def
                , steps=[]
                , relations=[]
                }

data Relation key = Seq [key]
                  | Vertical key key
                  deriving (Show, Eq)

data Step key time var
  = Step
    { key  :: key
    , time :: StepTime time
    , desc :: StepInfo var
    }

instance ( Num time, Enum time
         , Show key, Show time, Show var
         ) => Show (Step key time var) where
  show Step{..} = case time of
    Event a      -> ['.'|_<-[0..a-1]] ++ "+"             ++ suf
    Interval a b -> ['.'|_<-[0..a-1]] ++ ['='|_<-[a..b]] ++ suf
    where suf = "\t\t: " ++ show key ++ " " ++ show desc



data StepTime t = Event t
                | Interval t t
                deriving (Eq, Show)


class Timeline a where
  chart :: a -> String

instance (Show key, Show var, Show time, Num time
         ) => Timeline (Step key time var) where
  chart Step{..} = concat
    [ "["
    , "'" ++ show key ++ "', "
    , "'" ++ show desc ++ "', "
    , "null, "
    , chart time
    , "]"
    ]

instance (Show t, Num t) => Timeline (StepTime t) where
  chart (Event t) = "new Date(" ++ show t ++  "), new Date(" ++ show t ++  ")"
  chart (Interval a b) = "new Date(" ++ show a ++  "), new Date(" ++ show (b + 1) ++  ")"



data Key i = Atom i
           | Composite [Key i]
           deriving (Show, Eq, Ord)

instance (Default i) => Default (Key i) where
  def = Atom def

instance Enum i => Enum (Key i) where
  toEnum i = Atom $ toEnum i
  fromEnum (Atom i) = fromEnum i


modifyProcess p state = runState state p

add desc time = do
  p@Process{ keySeed=key, .. } <- get
  put p { keySeed=succ key
        , steps=Step key time desc : steps
        }
  return key

relation r = do
  p@Process{..} <- get
  put p{ relations=r : relations }

setTime t = do
  p <- get
  put p{ tick=t }


data StepInfo var  where
  Compiler :: String -> StepInfo var
  FunctionBlock :: FB var -> StepInfo var
  Interaction :: Interaction var -> StepInfo var
  Middle :: (Show mid) => mid -> StepInfo var
  Signal :: (Show sig) => sig -> StepInfo var

instance (Show var
         ) => Show (StepInfo var) where
  show (Compiler s)       = s
  show (FunctionBlock fb) = show fb
  show (Interaction i)    = show i
  show (Middle m)         = show m
  show (Signal sig)       = "signal: " ++ show sig

