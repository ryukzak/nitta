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
import qualified Data.List         as L
import qualified Data.Map          as M
import           Data.Maybe
import qualified Data.String.Utils as S
import           Data.Typeable
import           Numeric.Interval  hiding (elem)



---------------------------------------------------------------------
-- * Переменные и пересылаемые данные


-- | Класс идентификатора переменной.
class ( Typeable v, Eq v, Ord v, Show v ) => Var v
instance ( Typeable v, Eq v, Ord v, Show v ) => Var v
instance {-# OVERLAPS #-} Var String

class Variables x v | x -> v where
  -- | Получить список идентификаторов связанных переменных.
  variables :: x -> [v]



-- | Семейство типов для описания входов/выходов для функциональных блоков.
-- Необходимо чтобы описание функционального блока можно было использовать для:
--
--     * логического описания вычислительного значения (выход из f подаётся на вход g и h);
--     * фактического (Parcel) описания пересылок (выход из f формирует значения а и b,
--       значение a загружается в g, значение b загружается в h).
class IOTypeFamily io where
  -- | Тип для описания загружаемого значения.
  data I io :: * -> *
  -- | Тип для описания выгружаемого значения.
  data O io :: * -> *

class ( Show (I io v), Variables (I io v) v, Eq (I io v)
      , Show (O io v), Variables (O io v) v, Eq (O io v)
      , Typeable io, Var v
      ) => IOType io v
instance ( Show (I io v), Variables (I io v) v, Eq (I io v)
         , Show (O io v), Variables (O io v) v, Eq (O io v)
         , Typeable io, Var v
         ) => IOType io v



-- | Идентификатор типа для описания физически фактических пересылаемых значений.
data Parcel = Parcel

instance IOTypeFamily Parcel where
  data I Parcel v = I v -- ^ Загружаемые значения.
    deriving (Show, Eq, Ord)
  data O Parcel v = O [v] -- ^ Выгружаемые значения.
    deriving (Show, Eq, Ord)

instance Variables (I Parcel v) v where
  variables (I v) = [v]
instance Variables (O Parcel v) v where
  variables (O v) = v



-- | Взаимодействие PU с окружением. Подразумевается, что в один момент времени может быть только
-- одно взаимодействие, при этом у PU только один канал для взаимодействия.
data Effect v
  = Push v   -- ^ Загрузка данных в PU.
  | Pull [v] -- ^ Выгрузка данных из PU.
  deriving ( Show, Eq, Ord )

instance ( Var v ) => Variables (Effect v) v where
  variables (Push i) = [i]
  variables (Pull o) = o

(Push a) << (Push b) | a == b = True
(Pull a) << (Pull b)          = all (`elem` a) b
_        << _                 = False

(Pull a) \\\ (Pull b) = Pull (a L.\\ b)
_ \\\ _ = error "Only for Pulls"



---------------------------------------------------------------------
-- * Время


-- | Класс координаты во времени.
class ( Default t, Num t, Bounded t, Ord t, Show t, Typeable t, Enum t ) => Time t
instance ( Default t, Num t, Bounded t, Ord t, Show t, Typeable t, Enum t ) => Time t
instance {-# OVERLAPS #-} Time Int
instance {-# OVERLAPS #-} ( Default t, Typeable tag, Typeable t
                          , Eq tag, Ord t, Num t, Bounded t, Enum t
                          , Show tag, Show t
                          ) => Time (TaggedTime tag t)

-- | Описание временных ограничений на активности (Ativity). Используется при описании доступных
-- опций для планирования вычислительного процесса.
data TimeConstrain t
  = TimeConstrain
  { tcAvailable :: Interval t -- ^ Замкнутый интервал, в рамках которого можно выполнить активность.
  , tcDuration  :: Interval t -- ^ Замкнутый интервал допустимой длительности активности.
  } deriving ( Show, Eq )



-- | Изначально, для описания времени использовался тип Int. Время отсчитывалось с 0, было линейным
-- и совпадало с адресами памяти команд. К сожалению, это никуда не годится в случае если:
--
--     1) В вычислительном процессе присутствуют циклы и ветвления.
--     2) Вычислитель может включаться, выключаться...
--
-- По этому было принято решение добавить тег, идентифицирующий к какой ветку развития
-- вычислительного процесса относится данная точка.
data TaggedTime tag t
  = TaggedTime
  { tag   :: Maybe tag -- ^ Идентификатор ветки вычислительного процесса.
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



---------------------------------------------------------------------
-- * Функциональные блоки


-- | Класс функциональных блоков. Описывает все необходмые для работы компилятора свойства.
-- TODO: Разбить на множество мелких классов, чтобы сократить описание функционального блока.
class ( Typeable fb
      , Eq fb
      ) => FunctionalBlock fb v | fb -> v where
  -- | Возвращает зависимости между аргументами функционального блока.
  -- Формат: (заблокированное значение, блокирующее значение).
  dependency :: fb -> [(v, v)]
  -- | Необходимость "выворачивания" функций при визуализации вычислительного процесса.
  -- Выглядит примерно так:
  -- 1. Начинается вместе с вычислительным циклом.
  -- 2. Прерывается.
  -- 3. Возобнавляется и выполняется до конца вычислительного цикла.
  insideOut :: fb -> Bool
  insideOut _ = False
  -- | Информация для приоритизации функций в процессе диспетчеризации.
  -- Критические функциональные блоки - блоки, жёстко блокирующие внутрении ресурсы PU. Такие блоки
  -- следует привязывать одними из первых, так как в противном случае требуемые ресурс может быть
  -- занят другим ФБ, а следовательно заблокировать процесс синтеза.
  -- TODO: Необходимо обобщить.
  isCritical :: fb -> Bool
  isCritical _ = False


class WithFunctionalBlocks x io v | x -> io, x -> v where
  -- | Получить список связанных функциональных блоков.
  functionalBlocks :: x -> [FB io v]



-- | Контейнер для функциональных блоков. Необходимо для формирования гетерогенных списков.
data FB box v where
  FB :: ( FunctionalBlock fb v
        , Show fb
        , Variables fb v
        , IOType io v
        ) => fb -> FB io v
deriving instance ( Show v ) => Show (FB box v)

instance ( IOType box v, Var v ) => FunctionalBlock (FB box v) v where
  dependency (FB fb) = dependency fb
  insideOut (FB fb) = insideOut fb
  isCritical (FB fb) = isCritical fb

instance Variables (FB Parcel v) v where
  variables (FB fb) = variables fb

instance Eq (FB box v) where
  FB a == FB b = Just a == cast b

instance ( Variables (FB box v) v, Var v ) => Ord (FB box v) where
  a `compare` b = variables a `compare` variables b



---------------------------------------------------------------------
-- * Описание вычислительного процесса


-- | Описание многоуровневого вычислительного процесса PU. Подход к моделированию вдохновлён
-- ISO 15926. Имеются следующие варианты использвоания:
--
--     1) Хранение многоуровневого описания вычислительного процесса отдельного PU (одного
--        структурного элемента процессора).
--     2) Формирование многоуровневого описания вычислительного процесса для отдельного PU и
--        входящих в его состав структурных элементов (Nested). К примеру: вычислительный процесс
--        сети и подключённых к ней PU. (доступно через функцию process)
data Process v t
  = Process
    { steps     :: [Step v t] -- ^ Список шагов вычислительного процесса.

    , relations :: [Relation] -- ^ Список отношений между шагами вычислительного процесса
                              --   (отношения описываются через "кортежи" из ProcessUid).
    , nextTick  :: t          -- ^ Номер первого свободного такта.
    , nextUid   :: ProcessUid -- ^ Следующий свободный идентификатор шага вычислительного процесса.
    }

instance (Time t) => Default (Process v t) where
  def = Process { steps=[], relations=[], nextTick=def, nextUid=def }

type ProcessUid = Int -- ^ Уникальный идентификатор шага вычислительного процесса.

-- | Описание шага вычислительного процесса.
data Step v t where
  Step ::
    { sKey  :: ProcessUid    -- ^ Уникальный идентификатор шага.
    , sTime :: PlaceInTime t -- ^ Описание типа и положения шага во времени.
    , sDesc :: StepInfo v    -- ^ Описание действия описываемого шага.
    } -> Step v t

-- | Описание положения события во времени и типа события:
data PlaceInTime t
  = Event t -- ^ Мгновенные события, используются главным образом для описания событий САПР.
  | Activity ( Interval t ) -- ^ Протяжённые во времени события. Используются замкнутые интервалы.
  deriving ( Show )

-- | Описание события, соответсвующего шага вычислительного процесса. Каждый вариант соответствует
-- соответствующему отдельному уровню организации вычислительного процесса.
data StepInfo v where
  -- | Решения, принятые на уровне САПР.
  CADStep :: String -> StepInfo v
  -- | Время работы над функциональным блоком функционального алгоритма.
  FBStep :: FB Parcel v -> StepInfo v
  -- | Описание взаимодействий отдельных PU (загрузка/выгрузка данных). Указывается с точки зрения
  -- управления PU, а не с точки зрения загрузки шины.
  EffectStep :: Effect v -> StepInfo v
  -- | Описание инструкций, выполняемых конкретным рассматриваемым PU. Список инструкций
  -- определяется PU.
  InstructionStep :: ( Show (Instruction pu)
                     , Typeable (Instruction pu)
                     ) => Instruction pu -> StepInfo v

  -- | Используется для описания вычислительного процесса вложенных структурных элементов.
  -- Как правило не хранится в структурах данных, а генерируется автоматически по требованию при
  -- помощи опроса вложенных структурных элементов.
  NestedStep :: ( Eq title, Show title, Ord title
                ) => title -> StepInfo v -> StepInfo v

deriving instance ( Var v, Time t ) => Show ( Process v t )
deriving instance ( Var v, Time t ) => Show ( Step v t )
instance ( Var v ) => Show (StepInfo v) where
  show (CADStep s)                 = s
  show (FBStep (FB fb))            = show fb
  show (EffectStep (Pull v))       = "V" ++ show v
  show (EffectStep (Push v))       = "A" ++ show v
  show (InstructionStep instr)     = show instr
  show (NestedStep title stepInfo) = show title ++ "." ++ show stepInfo


-- | Получить строку с название уровня указанного шага вычислительного процесса.
level (CADStep _)         = "CAD"
level (FBStep _)          = "Function block"
level (EffectStep _)      = "Effect"
level (InstructionStep _) = "Instruction"
level (NestedStep _ _)    = "Nested"


-- | Описание отношений между шагами вычисительного процесса.
data Relation
  -- | Отношение между шагами вычислительного процесса разных уровней, в котором второй шаг получен
  -- путём трансляции/детализации первого шага.
  = Vertical ProcessUid ProcessUid
  deriving (Show, Eq)



---------------------------------------------------------------------
-- * Вычислительные блоки (PU)


-- | Тип PU с точки зрения принципов организации вычислительного процесса определяется теми
-- операциями, посредством которых САПР определяет развитие его вычислительного процесса.
-- При этом один PU может реализовывать несколько вариантов, например: организация вычилсительного
-- процесса множества PU объединённых в сеть с точки зрения пересылки данных и сточки зрения
-- внешнего взаимодействия с сетью как с одним PU.
class PUType t where
  -- | Вариант развития вычислительного процесса. Может допускать внутрении свободы действий:
  --
  --     * выбор конкретных моментов времени в предоставленных вариантах;
  --     * выбор отдельных деталей (к примеру: когда одно значение надо забрать в два пункта
  --       назначения, это можно сделать за один раз, так и за два).
  data Option t :: * -> * -> *
  data Action t :: * -> * -> *



-- | Тип организации вычислительного процесса через пассивные операции чтения (Pull) данных с
-- входного регистра и записи (Push) данных на выходной регистр. В один момент времени может быть
-- произведена только одна операция (вероятно, это искусственное ограничение, навязанное
-- архитектурой NL3). PU не может самостоятельно принимать решение относительно своих
-- взаимодействий с окружающим миром, он искючительно выполняет сказанные ему операции.
data Passive

instance PUType Passive where
  data Option Passive v t
    = EffectOpt
    { eoEffect :: Effect v -- ^ Чтение данных из входного регистра PU или запись данных в него.
    , eoAt :: TimeConstrain t -- ^ Временные ограничения на операцию.
    }
  data Action Passive v t
    = EffectAct
    { eaEffect :: Effect v -- ^ Выбранная операция для взаимодействия с окружающим миром.
    , eaAt :: Interval t -- ^ Положение операции во времени.
    }

deriving instance ( Var v, Time t ) => Show (Option Passive v t)
deriving instance ( Var v, Time t ) => Show (Action Passive v t)

instance ( Var v ) => Variables (Option Passive v t) v where
  variables EffectOpt{..} = variables eoEffect
instance ( Var v ) => Variables (Action Passive v t) v where
  variables EffectAct{..} = variables eaEffect



-- | Тип организации вычилсительного процесса через организацию пересылки данных между PU. При это
-- возможна параллельная пересылка одного значения в несколько потребителей.
data Network title

instance PUType (Network title) where
  data Option (Network title) v t
    = TransportOpt
    { toPullFrom :: title -- ^ Источник пересылки.
    , toPullAt   :: TimeConstrain t -- ^ Временные ограничения на операцию.
    -- | Словарь, описывающий все необходимые пункты назначения для пересылаемого значения.
    -- Допустима ситация, когда пункт назначения не может принять значение, в таком случае для
    -- негоне указываются временные ограничения.
    --
    -- Примечание: почему title оказался под Maybe? Потому что мы можем, банально, не знать в каком
    -- PU находится требуемый функциональный блок, так как он может быть ещё непривязан к PU.
    , toPush     :: M.Map v (Maybe (title, TimeConstrain t))
    } deriving (Show)
  data Action (Network title) v t
    = TransportAct
    { taPullFrom :: title -- ^ Источник пересылки.
    , taPullAt   :: Interval t -- ^ Положение операции во времени.
    -- | Словарь, описывающий пункты назначения для пересылаемого значения.
    , taPush     :: M.Map v (Maybe (title, Interval t))
    }
instance Variables (Option (Network title) v t) v where
  variables TransportOpt{..} = M.keys toPush

instance ( Show title, Time t, Var v ) => Show (Action (Network title) v t) where
  show TransportAct{..} = show taPullFrom ++ "#[" ++ show taPullAt ++ "] -> " ++ S.join "; " pushs
    where
      pushs = catMaybes $ map foo $ M.assocs taPush
      foo (v, Just (title, event)) = Just (show v ++ "@" ++ show title ++ "#[" ++ show event ++ "]")
      foo _ = Nothing



-- | Базовые функции для организации вычислительного процесса вычислительного блока.
--
-- Идеологически, планирование вычислительного процесса производится следующим образом:
--
--    1) Вычислительному блоку назначаются испоняемые им функции.
--    2) Блок опрашивается на предмет возможных вариантов развития вычислительного процесса.
--    3) Выбранный вариант развития вычислительного процесса моделируется в рамках вычислительного
--       блока. Его модельное время продвигается вперёд, в описании вычислительного процесса
--       дополняется записями относительно сделанных шагов вычислительного процесса.
--    4) Повторение, пока список возможных вариантов не станет пустым.
class ( Typeable (Signals pu)
      , Typeable (Instruction pu)
      ) => PUClass ty pu v t | pu -> ty, pu -> v, pu -> t where
  -- | Назначить исполнение функционального блока вычислительному узлу.
  bind :: FB Parcel v -> pu -> Either String pu
  -- | Получить описания всех вариантов развития вычислительного процесса, доступных для
  -- вычислительного блока в его текущем состоянии.
  options :: pu -> [Option ty v t]
  -- | Выбор и моделирование конкретного варианта развития вычислительного процесса.
  select :: pu -> Action ty v t -> pu

  -- | Запрос описания вычилсительного процесса с возможностью включения описания вычислительного
  -- процесс вложенных структурных элементов.
  process :: pu -> Process v t
  -- | Установать модельное время вычислительного блока.
  -- FIXME: А зачем оно нужно?
  setTime :: t -> pu -> pu



-- | Контейнер для вычислительных узлов (PU). Необходимо для формирования гетерогенных списков.
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
  variableValue fb (PU pu) cntx vi = variableValue fb pu cntx vi



---------------------------------------------------------------------
-- * Сигналы и инструкции


class ( Typeable pu ) => Controllable pu where
  data Instruction pu :: *
  data Signals pu :: *

  -- идея конечно хороша и крассива, но на практике не реализуема, так как:
  -- - исключает параллелизм инсрукций (что особенно актуально для Net.
  -- signalValue :: Instruction pu -> Signals pu -> Value

  proxy :: pu -> Proxy pu
  proxy _ = Proxy



data S where
  S :: ( Typeable (Signals pu), Ord (Signals pu), Show (Signals pu) ) => Signals pu -> S



class ByTime pu t | pu -> t where
  signalAt :: pu -> Signals pu -> t -> Value



gsignalAt pu (S s) = let s' = fromMaybe (error "Wrong signal!") $ cast s
                     in signalAt pu s'

class ByInstruction pu where
  signalFor :: Instruction pu -> Signals pu -> Value

gsignalFor instr (S s) = let s' = fromMaybe (error "Wrong signal!") $ cast s
                         in signalFor instr s'





data Value = X | B Bool | Broken

instance Show Value where
  show X         = "x"
  show (B True)  = "1"
  show (B False) = "0"
  show Broken    = "B"

X +++ v = v
v +++ X = v
_ +++ _ = Broken






---------------------------------------------------------------------
-- * Синтез и тестирование вычислительных блоков


type SimulationContext v x = M.Map (v, Int) x


-- | Класс предназначенный для симуляции вычислительного процесса. Может использоваться как просто
-- для моделирования, так и для генерации TestBench-а. Работает только с уже спланированным
-- вычислительным процессом (с одной стороны это искуственное ограничение первых версий, с другой
-- стороны - это позволяет учитывать внутренее состояние вычислительного блока, что может быть
-- полезным при работе со значеними по умолчанию).
class Simulatable pu v x | pu -> v, pu -> x where
  variableValue :: FB Parcel v -- ^ Функциональный блок, оперируйщий интересующим значением.
                -> pu -- ^ Вычислительный блок.
                -> SimulationContext v x -- ^ Контекст вычислительного процесса, содержащий уже
                                         -- известные значения переменных.
                -> (v, x) -- ^ Описание интересующего значения, где v - идентификатор, а x - номер
                          -- вычислительного цикла. Совпадение его по типу с результатом -
                          -- - совпадение (FIXME).
                -> x      -- ^ Значение на шине данных, выставляемое вычислительным блоком по результату.



class ( Typeable pu, Ord (Signals pu)) => Synthesis pu where
  moduleInstance :: pu -> String -> [(String, String)] -> String
  moduleName :: pu -> String
  moduleDefinition :: pu -> String
