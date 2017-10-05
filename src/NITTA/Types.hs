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

import           Control.Lens
import           Data.Default
import qualified Data.List         as L
import qualified Data.Map          as M
import           Data.Maybe
import qualified Data.String.Utils as S
import           Data.Typeable


class HasLeftBound a b | a -> b where
  leftBound :: Lens' a b

class HasRightBound a b | a -> b where
  rightBound :: Lens' a b

class HasDur a b | a -> b where
  dur :: Lens' a b



-- | Описание классов идентификатора переменной.
class ( Typeable v, Eq v, Ord v, Show v ) => Var v
instance ( Typeable v, Eq v, Ord v, Show v ) => Var v

class Variables x v | x -> v where
  -- | Получить список переменных, связанных с экземпляром класса.
  variables :: x -> [v]



-- | Описание классов координаты во времени.
class ( Default t, Num t, Bounded t, Ord t, Show t, Typeable t, Enum t ) => Time t
instance ( Default t, Num t, Bounded t, Ord t, Show t, Typeable t, Enum t ) => Time t


-- | Тип данных для описания требований к событиям во времени. Интервал значений - замкнутый.
data TimeConstrain t
  = TimeConstrain
  { tcDuration :: t -- ^ TODO: Не понятно можно ли изменять длительность и если да - то в каких пределах.
  , tcFrom     :: t
  , tcTo       :: t
  } deriving ( Show, Eq )

instance HasLeftBound (TimeConstrain t) t where
  leftBound = lens tcFrom $ \e s -> e{ tcFrom=s }
instance HasRightBound (TimeConstrain t) t where
  rightBound = lens tcTo $ \e s -> e{ tcTo=s }
instance HasDur (TimeConstrain t) t where
  dur = lens tcDuration $ \e s -> e{ tcDuration=s }


data Event t
  = Event
  { eStart    :: t
  , eDuration :: t
  } deriving ( Show, Eq )

instance HasLeftBound (Event t) t where
  leftBound = lens eStart $ \e s -> e{ eStart=s }
instance HasDur (Event t) t where
  dur = lens eDuration $ \e s -> e{ eDuration=s }








-- |Изначально, для описания времени пользовался тип Int. Время отсчитывалось с 0, было линейным и совпадало с ячеками памяти.
-- К сожалению, этого недостаточно для описании вычислительного процесса с ветвлениями, по этому было принято решение
-- теги к описанию времени.
data TaggedTime tag t
  = TaggedTime
  { -- | Позволяет идентифицировать  ветку вычислительного процесса. Ветки образуютсяв следствии ветвления и циклов.
    tag   :: Maybe tag
  , clock :: t
  } deriving ( Typeable )

instance ( Default t ) => Default (TaggedTime tag t) where
  def = TaggedTime Nothing def

instance ( Time t, Show tag ) => Show (TaggedTime tag t) where
  show (TaggedTime tag t) = show t ++ maybe "" (("!" ++) . show) tag
instance {-# OVERLAPS #-} ( Time t ) => Show (TaggedTime String t) where
  show (TaggedTime tag t) = show t ++ maybe "" ("!" ++) tag

instance ( Eq t ) => Eq (TaggedTime tag t) where
  (TaggedTime _ a) == (TaggedTime _ b) = a == b
instance ( Ord t ) => Ord (TaggedTime tag t) where
  (TaggedTime _ a) `compare` (TaggedTime _ b) = a `compare` b

instance ( Enum t ) => Enum (TaggedTime tag t) where
  toEnum i = TaggedTime Nothing $ toEnum i
  fromEnum (TaggedTime _ i) = fromEnum i
instance ( Num t ) => Bounded (TaggedTime tag t) where
  minBound = TaggedTime Nothing 0
  maxBound = TaggedTime Nothing 1000
instance ( Num t, Show tag, Eq tag ) => Num (TaggedTime tag t) where
  (TaggedTime Nothing a) + (TaggedTime Nothing b) = TaggedTime Nothing (a + b)
  (TaggedTime (Just tag) a) + (TaggedTime Nothing b) = TaggedTime (Just tag) (a + b)
  (TaggedTime Nothing a) + (TaggedTime (Just tag) b) = TaggedTime (Just tag) (a + b)
  (TaggedTime tag_a a) + (TaggedTime tag_b b)
    | tag_a == tag_b = TaggedTime tag_a (a + b)
    | otherwise = error $ "Not equal time tag! " ++ show tag_a ++ " " ++ show tag_b
  fromInteger = TaggedTime Nothing . fromInteger
  negate t = t{ clock=negate $ clock t }
  (*) = undefined
  abs = undefined
  signum = undefined


















-- | Класс для функциональных блоков. Описывает все необходмые для работы компилятора свойства.
class ( Typeable fb
      , Eq fb
      ) => FunctionalBlock fb v | fb -> v where
  -- | Возвращает зависимости между аргументами функционального блока. Формат: (заблокированное, требуемое).
  dependency :: fb -> [(v, v)]
  -- | Необходимость "выворачивания" функций при визуализации вычислительного процесса
  -- (начинается вместе с циклом, потом преравыется и заканчивается с циклом).
  insideOut :: fb -> Bool
  insideOut _ = False
  -- | Информация для приоритизации функций в процессе диспетчеризации.
  -- TODO: необходимо обобщить.
  isCritical :: fb -> Bool
  isCritical _ = False


class WithFunctionalBlocks x io v | x -> io, x -> v where
  -- | Получить список связанных функциональных блоков.
  functionalBlocks :: x -> [FB io v]











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
instance ( Var v ) => Show (StepInfo v) where
  show (FBStep (FB fb))            = show fb
  show (InfoStep s)                = s
  show (EffectStep (Pull v))       = "↓" ++ show v
  show (EffectStep (Push v))       = "↑" ++ show v
  show (InstructionStep instr)     = show instr
  show (NestedStep title stepInfo) = show title ++ "." ++ show stepInfo


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
    } -- deriving (Show)
instance Variables (Option (Network title) v t) v where
  variables TransportOpt{..} = M.keys toPush

instance ( Show title, Time t, Var v ) => Show (Action (Network title) v t) where
  show TransportAct{..} = show taPullFrom ++ " -> " ++ S.join "; " pushs
    where
      pushs = catMaybes $ map foo $ M.assocs taPush
      foo (v, Just (title, Event{..})) = Just (show v ++ "@" ++ show title ++ " (" ++ show eStart ++ ", " ++ show (eStart + eDuration) ++ ")")
      foo _ = Nothing

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



class Simulatable pu v x | pu -> v, pu -> x where
  varValue :: pu -> SimulationContext v x -> (v, x) -> x
  variableValue :: FB Parcel v -> pu -> SimulationContext v x -> (v, x) -> x



data S where
  S :: ( Typeable (Signals pu), Ord (Signals pu), Show (Signals pu) ) => Signals pu -> S

data GenericSignals
  = Clk
  | Data
  | Value
  | DataAttr
  | ValueAttr
  deriving (Show, Eq, Ord)

-- deriving instance Show (Signals pu) => Show (Link pu)
-- deriving instance Eq (Signals pu) => Eq (Link pu)
-- deriving instance Ord (Signals pu) => Ord (Link pu)


class ( Typeable pu, Ord (Signals pu)) => Synthesis pu where
  moduleInstance :: pu -> String -> [(String, String)] -> String
  moduleName :: pu -> String
  moduleDefinition :: pu -> String


data PU ty v t where
  PU :: ( PUClass ty pu v t
        , Typeable pu
        , Simulatable pu v Int
        , Controllable pu
        , Synthesis pu
        , ByTime pu t
        , Ord (Signals pu)
        , Show (Signals pu)
        , Typeable (Signals pu)
        ) => pu -> PU ty v t


instance ( Var v, Time t ) => PUClass Passive (PU Passive v t) v t where
  bind fb (PU pu) = PU <$> bind fb pu
  options (PU pu) = options pu
  select (PU pu) act = PU $ select pu act
  process (PU pu) = process pu
  setTime t (PU pu) = PU $ setTime t pu


instance ( PUClass Passive (PU Passive v t) v t
         ) => Simulatable (PU Passive v t) v Int where
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
  FB :: ( FunctionalBlock fb v
        , Show fb
        , Variables fb v
        , IOType io v
        ) => fb -> FB io v

instance ( IOType box v, Var v ) => FunctionalBlock (FB box v) v where
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

