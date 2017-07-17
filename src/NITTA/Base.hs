{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE LambdaCase                #-}
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
import           NITTA.Types
import qualified NITTA.FunctionBlocks as FB





type ProcessUid = Int

data Value = X | V Int | B Bool | Broken
instance Show Value where
  show X = "x"
  show (B True) = "1"
  show (B False) = "0"

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




class PUType t where
  data Variant t :: * -> * -> *
  data Action t :: * -> * -> *

data Passive
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

instance Vars (Variant Passive v t) v where
  variables PUVar{..} = variables vEffect
  
isPull (PUVar (Pull _) _) = True
isPull _                  = False
isPush (PUVar (Push _) _) = True
isPush _                  = False



data Network title
instance PUType (Network title) where
  data Variant (Network title) v t
    = NetworkVariant
    { vPullFrom :: title
    , vPullAt   :: TimeConstrain t
    , vPush     :: M.Map v (Maybe (title, TimeConstrain t))
    } deriving (Show)
  data Action (Network title) v t
    = NetworkAction
    { aPullFrom :: title
    , aPullAt   :: Event t
    , aPush     :: M.Map v (Maybe (title, Event t))
    } deriving (Show)




class ( Typeable (Signals pu)
      , ProcessInfo (Instruction pu v t)
      ) => PUClass pu ty v t where
  bind :: pu ty v t -> FB v -> Maybe (pu ty v t)
  variants :: pu ty v t -> [Variant ty v t]
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
        ) => pu ty v t -> PU ty v t
deriving instance Show (Instruction PU v t)
instance ( Var v, Time t ) => PUClass PU Passive v t where
  bind (PU pu) fb = PU <$> bind pu fb
  variants (PU pu) = variants pu
  step (PU pu) act = PU $ step pu act
  process (PU pu) = process pu
  data Signals PU = Signals ()
  data Instruction PU v t
  signal' = error ""

-- instance ( Var v, Time t ) => TestBench PU Passive v t where
  -- testBench (PU pu) = testBench pu
  -- fileName (PU pu) = fileName pu
  -- processFileName (PU pu) = processFileName pu


  




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




class ( PUClass pu ty v t ) => TestBench pu ty v t where
  testBench :: (pu ty v t) -> String

  fileName :: (pu ty v t) -> String
  processFileName :: (pu ty v t) -> String

  writeTestBench :: (pu ty v t) -> IO ()
  writeTestBench pu =
    writeFile (processFileName pu) $ testBench pu




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

whatsHappen t = filter (\Step{ time=Event{..} } -> eStart <= t && t < eStart + eDuration)
infoAt t = catMaybes . map (\Step{..} -> cast info) . whatsHappen t


