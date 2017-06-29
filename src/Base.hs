{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

module Base where
import           Control.Monad.State
import           Data.Default
import           Data.List           (find, intersect, partition)
import qualified Data.List           as L
import qualified Data.Map            as M
import           Data.Maybe          (fromMaybe, isJust)
import           FB
import qualified FB


class DPUClass dpu key var time | dpu -> key var time where
  evaluate :: dpu -> FB var -> Maybe dpu
  variants :: dpu -> [(Interaction var, TimeConstrain time)]
  step :: dpu -> Interaction var -> time -> time -> dpu
  proc :: dpu -> Process var key time

evaluate' (Just dpu) fb = evaluate




data DPU key var time where
  DPU :: (DPUClass dpu key var time) => dpu -> DPU key var time

instance DPUClass (DPU key var time) key var time where
  evaluate (DPU dpu) fb = fmap DPU $ evaluate dpu fb
  variants (DPU dpu) = variants dpu
  step (DPU dpu) act begin end = DPU $ step dpu act begin end
  proc (DPU dpu) = proc dpu


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


data Transport title var time = Transport
  { pullFrom :: title
  , pullAt   :: time
  , push     :: M.Map var (Maybe (title, time))
                      -- Nothing значит либо, операция ещё не распределена,
                      -- либо DPU не может его принять.
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
  chart (Event t) = "new Date(" ++ year t ++  "), new Date(" ++ year t ++  ")"
  chart (Interval a b) = "new Date(" ++ year a ++  "), new Date(" ++ year (b + 1) ++  ")"
-- instance (Show t) => Timeline (StepTime t) where
--   chart (Event t) = "new Date('" ++ year t ++  "-01-01'), new Date('" ++ year t ++  "-02-01')"
--   chart (Interval a b) = "new Date('" ++ year a ++  "-01-01'), new Date('" ++ year b ++  "-01-01')"

year x = show x -- zeros ++ x'
  -- where
    -- x' = show x
    -- zeros = take (4 - length x') $ repeat '0'




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
        , steps=(Step key time desc) : steps
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
  Interaction :: (Interaction var) -> StepInfo var
  Middle :: (Show mid) => mid -> StepInfo var
  Signal :: (Show sig) => sig -> StepInfo var

instance (Show var
         ) => Show (StepInfo var) where
  show (Compiler s)       = s
  show (FunctionBlock fb) = show fb
  show (Interaction i)    = show i
  show (Middle m)         = show m
  show (Signal sig)       = "signal: " ++ show sig


-- data StepInfo var mid sig
  -- = Compiler String
  -- | FunctionBlock FB
  -- | Interaction (Interaction var)
  -- | Middle mid
  -- | Signal sig

-- instance (Show var, Show mid, Show sig
         -- ) => Show (StepInfo var mid sig) where
  -- show (Compiler s)       = s
  -- show (FunctionBlock fb) = show fb
  -- show (Interaction i)    = show i
  -- show (Middle m)         = show m
  -- show (Signal sig)       = "signal: " ++ show sig
