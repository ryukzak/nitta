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

class ( Typeable v, Eq v, Ord v, Show v ) => Var v
instance ( Typeable v, Eq v, Ord v, Show v ) => Var v

class ( Ord k, Enum k ) => Key k
instance ( Ord k, Enum k ) => Key k



-- v - var; t - time; k - key
class PUClass dpu variant action step v t k where
  evaluate :: dpu variant action step v t k -> FB v -> Maybe (dpu variant action step v t k)
  variants :: dpu variant action step v t k -> [variant v t]
  step     :: dpu variant action step v t k -> action v t -> dpu variant action step v t k
  process  :: dpu variant action step v t k -> Process step v t k



data PU variant action step v t k where
  PU :: (PUClass dpu variant action step v t k
        ) => dpu variant action step v t k -> PU variant action step v t k

instance PUClass PU variant action step v t k where
  evaluate (PU dpu) fb = PU <$> evaluate dpu fb
  variants (PU dpu) = variants dpu
  step (PU dpu) act = PU $ step dpu act
  process (PU dpu) = process dpu



data Transport title mtime var time = Transport
  { pullFrom :: title
  , pullAt   :: mtime time
  , push     :: M.Map var (Maybe (title, mtime time))
                      -- Nothing значит либо, операция ещё не распределена,
                      -- либо DPU не может его принять.
  } deriving (Show, Eq)
type NiVariant title = Transport title TimeConstrain
type NiAction title = Transport title Event


data Interaction mtime v t = Interaction { effect :: Effect v
                                         , at     :: mtime t
                                         }
type PuVariant = Interaction TimeConstrain
type PuAction = Interaction Event

data Effect var
  = Push var
  | Pull [var]
  deriving (Show, Eq)

(Push a) << (Push b) | a == b = True
(Pull a) << (Pull b)          = all (`elem` a) b
_        << _                 = False

(Pull a) \\ (Pull b) = Pull (a L.\\ b)

instance Vars (Effect var) var where
  variables (Push var)  = [var]
  variables (Pull vars) = vars



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

instance (Default k, Default t) => Default (Process step v t k) where
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

instance (Default k) => Default (Step info v t k) where
  def = Step undefined undefined def

-- instance Enum k => Enum (Step info v t k) where
  -- toEnum i = Step (toEnum i) undefined undefined
  -- fromEnum Step{..} = fromEnum key



data NestedStep info nId nInfo v t k
  = NStep { nTime :: Event t
          , nInfo :: info v
          , nKey  :: k
          }
  | Nested { nId    :: nId
           , nested :: nInfo v t k
           , nKey   :: k
           } deriving (Show)

instance (Default k) => Default (NestedStep info nId nInfo v t k) where
  def = NStep undefined undefined def

-- instance Enum k => Enum (NestedStep info nId nInfo v t k) where
  -- toEnum i = NStep (toEnum i) undefined undefined
  -- fromEnum NStep{..}  = fromEnum nKey
  -- fromEnum Nested{..} = fromEnum nKey

class Level a where
  level :: a -> String

instance Level (PuInfo v) where
  level (Compiler s)       = "Compiler"
  level (FunctionBlock fb) = "FunctionBlock"
  level (Effect i)         = "Effect"
  level (Middle m)         = "Middle"
  level (Signal sig)       = "Signal"


data PuInfo v where
  Compiler :: String -> PuInfo v
  FunctionBlock :: FB v -> PuInfo v
  Effect :: Effect v -> PuInfo v
  Middle :: (Show mid) => mid -> PuInfo v
  Signal :: (Show sig) => sig -> PuInfo v
type PuStep = Step PuInfo

instance (Show v) => Show (PuInfo v) where
  show (Compiler s)       = s
  show (FunctionBlock fb) = show fb
  show (Effect i)         = show i
  show (Middle m)         = show m
  show (Signal sig)       = "signal: " ++ show sig

instance Level (NiInfo v) where
  level (NiCompiler s)   = "NiCompiler"
  level (NiTransport fb) = "NiTransport"
  level (NiSignal i)     = "NiSignal"




data NiInfo v where
  NiCompiler :: String -> NiInfo v
  NiTransport :: [v] -> NiInfo v
  NiSignal :: (Show sig) => sig -> NiInfo v
type NiStep title = NestedStep NiInfo title (Step PuInfo)

instance (Show v) => Show (NiInfo v) where
  show (NiCompiler s)  = s
  show (NiTransport m) = show m
  show (NiSignal sig)  = "signal: " ++ show sig




data Relation key = Seq [key]
                  | Vertical key key
                  deriving (Show, Eq)


modifyProcess p state = runState state p


add' st = do
  p@Process{ nextStep=key, .. } <- get
  put p { nextStep=succ key
        , steps=st key : steps
        }
  return key

add desc time = do
  p@Process{ nextStep=key, .. } <- get
  put p { nextStep=succ key
        , steps=Step time desc key : steps
        }
  return key

-- add' desc time = do
  -- p@Process{ nextStep=key, .. } <- get
  -- put p { nextStep=succ key
        -- , steps=Step key time desc : steps
        -- }
  -- return key

relation r = do
  p@Process{..} <- get
  put p{ relations=r : relations }

setTime t = do
  p <- get
  put p{ tick=t }
