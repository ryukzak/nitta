{-# LANGUAGE DeriveGeneric          #-}
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

module NITTA.Types.Base where

import           Data.Default
import qualified Data.List        as L
import qualified Data.Map         as M
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics
import           NITTA.Types.Poly
import           Numeric.Interval hiding (elem)



---------------------------------------------------------------------
-- * Переменные и пересылаемые данные


-- | Класс идентификатора переменной.
--
-- TODO: Повидимому именно тут заложена самая страшная мина текущей реализации, а именно -
-- отсутствие типизации. При этом в настоящий момент совершенно не понятно как и главное где
-- учитывать типизацию данных ходящих по шине.
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
  data I io :: *
  -- | Тип для описания выгружаемого значения.
  data O io :: *

class ( Show (I io), Variables (I io) v, Eq (I io)
       , Show (O io), Variables (O io) v, Eq (O io)
       , Typeable io, Var v
       ) => IOType io v | io -> v
instance ( Show (I io), Variables (I io) v, Eq (I io)
         , Show (O io), Variables (O io) v, Eq (O io)
         , Typeable io, Var v
         ) => IOType io v



-- | Идентификатор типа для описания физически фактических пересылаемых значений.
newtype Parcel v = Parcel v

instance Var v => IOTypeFamily (Parcel v) where
  data I (Parcel v) = I v -- ^ Загружаемые значения.
    deriving (Show, Eq, Ord)
  data O (Parcel v) = O [v] -- ^ Выгружаемые значения.
    deriving (Show, Eq, Ord)

instance Variables (I (Parcel v)) v where
  variables (I v) = [v]
instance Variables (O (Parcel v)) v where
  variables (O v) = v



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
  } deriving ( Eq )
instance ( Show t ) => Show (TimeConstrain t) where
  show TimeConstrain{..} = show tcAvailable ++ "/P" ++ show tcDuration ++ ""


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
  } deriving ( Typeable, Generic )

class ( Eq tag, Show tag, Typeable tag ) => Tag tag
instance ( Eq tag, Show tag, Typeable tag ) => Tag tag

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


-- | Семейство типов для описания системы функций вычислительного блока.
class FunctionalSet pu where
  -- | Тип для представляния системы команд.
  data FSet pu :: *

instance ( WithFunctionalBlocks (FSet pu) (FB (Parcel v) v) ) => Variables (FSet pu) v where
  variables fbs = concatMap variables $ functionalBlocks fbs



class ToFSet pu v | pu -> v where
  -- | Преобразование гетерогенного функционального блока в представление системы функций
  -- вычислительного блока.
  toFSet :: FB (Parcel v) v -> Either String (FSet pu)

-- | Преобразование из представления системы функций вычислительного блока в гетерогенный
-- функциональный блок.
fromFSet f = head $ functionalBlocks f



-- | Класс функциональных блоков. Описывает все необходмые для работы компилятора свойства.
-- TODO: Разбить на множество мелких классов, чтобы сократить описание функционального блока.
class ( Typeable fb
      , Eq fb
      ) => FunctionalBlock fb v | fb -> v where
  inputs :: fb -> [v]
  inputs _ = []
  outputs :: fb -> [v]
  outputs _ = []
  -- | Возвращает зависимости между аргументами функционального блока.
  -- Формат: (заблокированное значение, блокирующее значение).
  dependency :: fb -> [(v, v)]
  dependency _ = []
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
  --
  -- TODO: на самом деле это не правильно, так как критичность на самом деле определяется связкой
  -- fb + pu. К примеру - использование Loop для Accum.
  isCritical :: fb -> Bool
  isCritical _ = False



class WithFunctionalBlocks x fb | x -> fb where
  -- | Получить список связанных функциональных блоков.
  functionalBlocks :: x -> [fb]



data Cntx v x
  = Cntx { cntxVars    :: M.Map v [x]
         , cntxInputs  :: M.Map v [x]
         , cntxOutputs :: M.Map v [x]
         , cntxFram    :: M.Map (Int, v) [x]
         }
  deriving (Show)

instance Default (Cntx v x) where
  def = Cntx M.empty M.empty M.empty M.empty


class FunctionSimulation fb v x | fb -> v where
  simulate :: Cntx v x -> fb -> Maybe (Cntx v x)



-- | Контейнер для функциональных блоков. Необходимо для формирования гетерогенных списков.
data FB io v where
  FB :: ( FunctionalBlock fb v
        , Show fb
        , Variables fb v
        , IOType io v
        , FunctionSimulation fb v Int
        ) => fb -> FB io v
deriving instance ( Show v ) => Show (FB io v)

instance ( IOType box v, Var v ) => FunctionalBlock (FB box v) v where
  dependency (FB fb) = dependency fb
  insideOut (FB fb) = insideOut fb
  isCritical (FB fb) = isCritical fb
  inputs (FB fb) = inputs fb
  outputs (FB fb) = outputs fb

instance Variables (FB io v) v where
  variables (FB fb) = variables fb

instance Eq (FB io v) where
  FB a == FB b = Just a == cast b

instance ( Variables (FB io v) v, Ord v ) => Ord (FB io v) where
  a `compare` b = variables a `compare` variables b

instance {-# OVERLAPS #-} FunctionalBlock fb v => Variables fb v where
  variables fb = inputs fb ++ outputs fb

instance FunctionSimulation (FB (Parcel v) v) v Int where
  simulate cntx (FB fb) = simulate cntx fb


---------------------------------------------------------------------
-- * Описание вычислительного процесса


-- | Описание многоуровневого вычислительного процесса PU. Подход к моделированию вдохновлён
-- ISO 15926. Имеются следующие варианты использования:
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

instance ( Default t ) => Default (Process v t) where
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
  FBStep :: FB (Parcel v) v -> StepInfo v
  -- | Описание использования вычислительного блока с точки зрения передачи данных.
  EndpointStep :: EndpointType v -> StepInfo v
  -- | Описание инструкций, выполняемых вычислительным блоком. Список доступных инструкций
  -- определяется типом вычислительного блока.
  InstructionStep :: ( Show (Instruction pu)
                     , Typeable (Instruction pu)
                     ) => Instruction pu -> StepInfo v

  -- | Используется для описания вычислительного процесса вложенных структурных элементов.
  -- Как правило не хранится в структурах данных, а генерируется автоматически по требованию при
  -- помощи опроса вложенных структурных элементов.
  NestedStep :: ( Eq title, Show title, Ord title
                ) => title -> StepInfo v -> StepInfo v

deriving instance ( Show v, Show t ) => Show ( Process v t )
deriving instance ( Show v, Show t ) => Show ( Step v t )
instance ( Show v ) => Show (StepInfo v) where
  show (CADStep s)                 = s
  show (FBStep (FB fb))            = show fb
  show (EndpointStep eff)          = show eff
  show (InstructionStep instr)     = show instr
  show (NestedStep title stepInfo) = show title ++ "." ++ show stepInfo


-- | Получить строку с название уровня указанного шага вычислительного процесса.
level (CADStep _)         = "CAD"
level (FBStep _)          = "Function block"
level (EndpointStep _)    = "Endpoint"
level (InstructionStep _) = "Instruction"
level (NestedStep _ _)    = "Nested"


-- | Описание отношений между шагами вычисительного процесса.
data Relation
  -- | Отношение между шагами вычислительного процесса разных уровней, в котором второй шаг получен
  -- путём трансляции/детализации первого шага.
  = Vertical ProcessUid ProcessUid
  deriving (Show, Eq)



---------------------------------------------------------------------
-- * Варианты и решения
--
-- Именно в рамках этих терминов должна риализовываться основная логика процесса синтеза (по
-- крайней мере программного обеспечения). Используется следующее соглашение: Тип идентифицирующий
-- варианты и решения заканчивается суфиксом DT, вариант - O, решение - D.

-- | Решение в области привязки функционального блока к вычислительному. Определяется только для
-- вычислительных блоков, организующих работу со множеством вложенных блоков, адресуемым по title.
data BindingDT title v
binding = Proxy :: Proxy BindingDT

instance DecisionType (BindingDT title v) where
  data Option (BindingDT title v) = BindingO (FB (Parcel v) v) title deriving ( Generic )
  data Decision (BindingDT title v) = BindingD (FB (Parcel v) v) title deriving ( Generic )


-- | Взаимодействие PU с окружением. Подразумевается, что в один момент времени может быть только
-- одно взаимодействие, при этом у PU только один канал для взаимодействия, что в общем то
-- ограничение. В перспективе должно быть расширено для работы с конвейра.
data EndpointType v
  = Source [v] -- ^ Выгрузка данных из PU.
  | Target v   -- ^ Загрузка данных в PU.
  deriving ( Show, Eq, Ord )

(Target a) << (Target b) | a == b = True
(Source a) << (Source b)          = all (`elem` a) b
_        << _                 = False

(Source a) \\\ (Source b) = Source (a L.\\ b)
a \\\ b = error $ "Can't get sub endpoint for " ++ show a ++ " " ++ show b

instance Variables (EndpointType v) v where
  variables (Source vs) = vs
  variables (Target v)  = [v]



-- | Решение об спользовании вычислительных блоков в роли источника или пункта назначения данных. В
--  один момент времени может быть произведена только одна операция (вероятно, это искусственное
--  ограничение, навязанное архитектурой NL3). PU не может самостоятельно принимать решение
--  относительно своих взаимодействий с окружающим миром, он искючительно выполняет сказанные ему
--  операции.
data EndpointDT v t
endpointDT = Proxy :: Proxy EndpointDT

instance DecisionType (EndpointDT v t) where
  data Option (EndpointDT v t)
    = EndpointO
    { epoType :: EndpointType v -- ^ Чтение данных из входного регистра PU или запись данных в него.
    , epoAt :: TimeConstrain t -- ^ Временные ограничения на операцию.
    }
  data Decision (EndpointDT v t)
    = EndpointD
    { epdType :: EndpointType v -- ^ Выбранная операция для взаимодействия с окружающим миром.
    , epdAt :: Interval t -- ^ Положение операции во времени.
    }

instance Variables (Option (EndpointDT v t)) v where
  variables EndpointO{..} = variables epoType
instance Variables (Decision (EndpointDT v t)) v where
  variables EndpointD{..} = variables epdType
instance ( Show v, Show t ) => Show (Option (EndpointDT v t)) where
  show EndpointO{..} = "option: " ++ show epoType ++ "@(" ++ show epoAt ++ ")"
instance ( Show v, Show t ) => Show (Decision (EndpointDT v t)) where
  show EndpointD{..} = "decision: " ++ show epdType ++ "@(" ++ show epdAt ++ ")"



---------------------------------------------------------------------
-- * Вычислительные блоки (PU)


-- | Описание вычислительного блока. Используется в совокупности с Decision по интересующим группам
-- вопросов.
--
-- Идеологически, планирование вычислительного процесса производится следующим образом:
--
--    1) Вычислительному блоку назначаются испоняемые им функции.
--    2) Блок опрашивается на предмет возможных вариантов развития вычислительного процесса.
--    3) Выбранный вариант развития вычислительного процесса моделируется в рамках вычислительного
--       блока. Его модельное время продвигается вперёд, в описании вычислительного процесса
--       дополняется записями относительно сделанных шагов вычислительного процесса.
--    4) Повторение, пока список возможных вариантов не станет пустым.
class ProcessUnit pu v t | pu -> v t where
  -- | Назначить исполнение функционального блока вычислительному узлу.
  bind :: FB (Parcel v) v -> pu -> Either String pu
  -- | Запрос описания вычилсительного процесса с возможностью включения описания вычислительного
  -- процесс вложенных структурных элементов.
  --
  -- Результат вычисления данной функции не должен редактироваться и возкращаться на место!
  process :: pu -> Process v t
  -- | Установить модельное время вычислительного блока.
  --
  -- TODO: Необходимо преобразовать в setTimeTag.
  --
  -- История вопроса:
  -- Изначально, данный метод был добавлен для работы в ращеплённом времени, но он: 1) недостаточен,
  -- 2) может быть реализован в рамках алгоритма компиляции.
  --
  -- В тоже время, setTime нужен не только для того, чтобы ограничить время, но и для того, что бы
  -- установить тег времени.
  --
  -- Вероятно, хорошим вариантом является жёсткое отслеживание времени и как следствие - явная
  -- изменение его тега / значения. Логично ожидать что данная операция будет применяться ко сему
  -- дереву вычислителя.
  setTime :: t -> pu -> pu

  -- TODO: Добавить метод skip, для того что бы вычислительный блок мог пропускать отдельный
  -- переменные (необходимо для ветвления вычислительного процесса).


---------------------------------------------------------------------
-- * Сигналы и инструкции


-- | Управляемые вычислительные блоки.
class Controllable pu where
  -- | Инструкции, описывающие работу вычислительного блока. Набор инструкций зависит от типа
  -- вычислительного блока. Для "оконечных" вычислительных блоков инструкция в прикладном виде
  -- описывает операцию, выполняемую им и одназначно транслируемую микрокод. Для сети -
  -- инструкция может описывать пересылку данных между блоками обраобтки (возможно более одной
  -- за такт).
  data Instruction pu :: *
  -- | Микрокод, управляющий вычислительным блоком. Описывает в человеческом виде все сигналы,
  -- управляющие вычислительным блоком.
  data Microcode pu :: *


-- | Метод, необходимый для управляемых блоков обработки данных. Позволяет узнать микрокоманду
-- для конкретного такта вычислительного процесса.
class ByTime pu t | pu -> t where
  microcodeAt :: pu -> t -> Microcode pu


class UnambiguouslyDecode pu where
  -- | Конвертация инструкции в микрокод.
  decodeInstruction :: Instruction pu -> Microcode pu



-- | Значение сигнальной линии.
data Value
  -- | Значение не определено.
  = X
  -- | Значение сигнальной линии установлено в логическое значение.
  | B Bool
  -- | Была сделана попытка установить сигнальную линию несколькими источниками, что привело к
  -- колизии и битому значению.
  | Broken

instance Default Value where
  def = X

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

-- | Контекст модельного вычислительного процесса представленный словарём переменных и их значений.
-- В качестве ключа используется имя переменной и индекс. Это необходимо для того, что бы
-- моделировать вычислительный процесс на разных циклах. При этом не очень ясно, модет ли работать
-- данная конструкция в TaggedTime в принципе.

-- | Класс предназначенный для симуляции вычислительного процесса. Может использоваться как просто
-- для моделирования, так и для генерации TestBench-а. Работает только с уже спланированным
-- вычислительным процессом (с одной стороны это искуственное ограничение первых версий, с другой
-- стороны - это позволяет учитывать внутренее состояние вычислительного блока, что может быть
-- полезным при работе со значеними по умолчанию).
class Simulatable pu v x | pu -> v x where
  simulateOn :: Cntx v x -- ^ Контекст вычислительного процесса, содержащий уже
                         -- известные значения переменных.
             -> pu -- ^ Вычислительный блок.
             -> FB (Parcel v) v -- ^ Функциональный блок, оперируйщий интересующим значением.
             -> Maybe (Cntx v x)



-- | Реализация вычислительного блока и его конфигурации (процессора и программоного обеспечения).
data Implementation
  -- | Рекомендуемое имя файла для модуля и текст модуля.
  = Immidiate String String
  -- | Библиотечный элемент, возвращается имя файла.
  | FromLibrary String
  -- | Рекомендуемое имя каталога, в котором должен быть проект данного модуля со всеми вложениями.
  | Project String [Implementation]
  | Empty



-- | Генерация аппаратной и программной составляющей процессора.
class DefinitionSynthesis pu where
  -- | Имя модуля.
  moduleName :: pu -> String
  -- | Реализация аппаратной составляющей вычислительного блока.
  hardware :: pu -> Implementation
  -- | Реализация программной составляющей составляющей вычислительного блока.
  software :: pu -> Implementation

