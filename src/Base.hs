{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Base where
import           Control.Monad.State
import           Data.Default
import qualified Data.List           as L
import           FB


data Times t
  = Times
  { duration  :: t
  , available :: (t, t)
  } deriving (Show, Eq)


data Interaction var
  = Push var
  | Pull [var]
  deriving (Show, Eq)

(Push a) << (Push b) | a == b = True
(Pull a) << (Pull b)          = all (`elem` a) b
_        << _                 = False

(Pull a) \\ (Pull b) = Pull (a L.\\ b)



data Process var key time mid sig
  = Process
    { tick      :: time
    , keySeed   :: key
    , steps     :: [Step key time var mid sig]
    , relations :: [Relation key]
    } deriving (Show)

instance (Default key, Default time) => Default (Process var key time mid sig) where
  def = Process { tick=def
                , keySeed=def
                , steps=[]
                , relations=[]
                }

data Relation key = Seq [key]
                  | Vertical key key
                  deriving (Show, Eq)

data Step key time var mid sig
  = Step
    { key  :: key
    , desc :: Desc var mid sig
    , time :: StepTime time
    }

instance ( Num time, Enum time
         , Show key, Show time, Show var, Show mid, Show sig
         ) => Show (Step key time var mid sig) where
  show Step{..} = case time of
    Event a      -> ['.'|_<-[0..a-1]] ++ "+"             ++ suf
    Interval a b -> ['.'|_<-[0..a-1]] ++ ['='|_<-[a..b]] ++ suf
    where suf = "\t\t: " ++ show key ++ " " ++ show desc



data StepTime t = Event t
                | Interval t t
                deriving (Eq, Show)


class Timeline a where
  chart :: a -> String

instance (Show key, Show var, Show mid, Show sig, Show time
         , Num time
         ) => Timeline (Step key time var mid sig) where
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

a@(Atom _) +++ b@(Atom _) = Composite [a, b]
a@(Atom _) +++ (Composite b) = Composite (a : b)
(Composite a) +++ b@(Atom _) = Composite (a ++ [b])
(Composite a) +++ (Composite b) = Composite (a ++ b)


modifyProcess p state = runState state p

add desc time = do
  p@Process{ keySeed=key@(Atom seed), .. } <- get
  put p { keySeed=Atom (seed + 1)
        , steps=(Step key desc time) : steps
        }
  return key

relation r = do
  p@Process{..} <- get
  put p { relations=r : relations
        }

setTime t = do
  p <- get
  put p{ tick=t }


data Desc var mid sig
  = Compiler String
  | FunctionBlock FB
  | Interaction (Interaction var)
  | Middle mid
  | Signal sig

instance (Show var, Show mid, Show sig
         ) => Show (Desc var mid sig) where
  show (Compiler s)       = s
  show (FunctionBlock fb) = show fb
  show (Interaction i)    = show i
  show (Middle m)         = show m
  show (Signal sig)       = "signal: " ++ show sig
