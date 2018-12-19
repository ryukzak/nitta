{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Types.Base
Description :
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Types.Base
    ( module NITTA.Types.Base
    , module NITTA.Types.Time
    ) where

import           Data.Default
import           Data.List
import qualified Data.Map          as M
import           Data.Proxy
import qualified Data.Set          as S
import qualified Data.String.Utils as S
import           Data.Typeable
import           GHC.Generics
import           NITTA.Types.Poly
import           NITTA.Types.Time
import           Numeric.Interval



---------------------------------------------------------------------
-- * Переменные и пересылаемые данные


-- |Класс идентификатора переменной.

-- TODO: Видимо именно тут заложена самая страшная мина текущей реализации, а именно - отсутствие
-- типизации. При этом в настоящий момент совершенно не понятно как и главное где учитывать
-- типизацию данных ходящих по шине.
type Var v = ( Typeable v, Ord v, Show v )

class Variables a v | a -> v where
    -- |Получить список идентификаторов связанных переменных.
    variables :: a -> S.Set v



-- |Группа типов данных для описания параметров функций
data I v = I v -- ^Загружаемые значения.
    deriving (Show, Eq, Ord)
data O v = O (S.Set v) -- ^Выгружаемые значения.
    deriving (Show, Eq, Ord)
data X x = X x -- ^Выгружаемые значения.
    deriving (Show, Eq)

instance Variables (I v) v where
    variables (I v) = S.singleton v
instance Variables (O v) v where
    variables (O v) = v



---------------------------------------------------------------------
-- * Функциональные блоки

-- |Класс функциональных блоков. Описывает все необходмые для работы компилятора свойства.
class Function f v | f -> v where
    inputs :: f -> S.Set v
    inputs _ = S.empty
    outputs :: f -> S.Set v
    outputs _ = S.empty
    -- |Необходимость "выворачивания" функций при визуализации вычислительного процесса. Выглядит
    -- примерно так:
    --
    -- 1. Начинается вместе с вычислительным циклом.
    -- 2. Прерывается.
    -- 3. Возобнавляется и выполняется до конца вычислительного цикла.
    insideOut :: f -> Bool
    insideOut _ = False
    -- |Информация для приоритизации функций в процессе диспетчеризации. Критические функциональные
    -- блоки - блоки, жёстко блокирующие внутрении ресурсы PU. Такие блоки следует привязывать одними
    -- из первых, так как в противном случае требуемые ресурс может быть занят другим ФБ, а
    -- следовательно заблокировать процесс синтеза.
    --
    -- Примечание: на самом деле это не правильно, так как критичность на самом деле определяется
    -- связкой f + pu. К примеру - использование Loop для Accum.
    isCritical :: f -> Bool
    isCritical _ = False


data Lock v
    = Lock
        { locked :: v
        , lockBy :: v
        }
    deriving ( Show )

class Locks x v | x -> v where
    -- |Возвращает зависимости между аргументами функционального блока. Формат: (заблокированное
    -- значение, блокирующее значение).
    locks :: x -> [Lock v]



class WithFunctions a f | a -> f where
    -- |Получить список связанных функциональных блоков.
    functions :: a -> [f]



data Cntx v x
    = Cntx
        { cntxVars    :: M.Map v [x]
        , cntxInputs  :: M.Map v [x]
        , cntxOutputs :: M.Map v [x]
        , cntxFram    :: M.Map (Int, v) [x]
        }

-- FIXME: Incorrect output if cntxInput has different amount of data.
instance ( Show v, Show x ) => Show (Cntx v x) where
    show Cntx{ cntxVars, cntxInputs, cntxOutputs }
        = let
            dt = concat
                [ map (\(v, xs) -> reverse $ map ( filter (/= '"') . (("q." ++ show v ++ ":") ++) . show ) xs) $ M.assocs cntxInputs
                , map (\(v, xs) -> reverse $ map ( filter (/= '"') . ((show v ++ ":") ++) . show ) xs) $ M.assocs cntxOutputs
                , map (\(v, xs) -> reverse $ map ( filter (/= '"') . ((show v ++ ":") ++) . show ) xs) $ M.assocs cntxVars
                ]
        in S.join "\n" $ map (S.join "\t") $ transpose dt

instance Default (Cntx v x) where
    def = Cntx M.empty M.empty M.empty M.empty


class FunctionSimulation f v x | f -> v x where
    simulate :: Cntx v x -> f -> Maybe (Cntx v x)



-- |Контейнер для функциональных блоков. Необходимо для формирования гетерогенных списков.
data F v x where
    F ::
        ( Ord v
        , Function f v
        , Locks f v
        , Show f
        , FunctionSimulation f v x
        , Typeable f, Typeable v, Typeable x
        ) => f -> F v x
instance Show (F v x) where
    show (F f) = S.replace "\"" "" $ show f

instance Function (F v x) v where
    insideOut (F f) = insideOut f
    isCritical (F f) = isCritical f
    inputs (F f) = inputs f
    outputs (F f) = outputs f

instance Locks (F v x) v where
    locks (F f) = locks f

instance Variables (F v x) v where
    variables (F f) = inputs f `S.union` outputs f

instance Eq (F v x) where
    F a == F b = show a == show b

instance Ord (F v x) where
    (F a) `compare` (F b) = show a `compare` show b

instance FunctionSimulation (F v x) v x where
    simulate cntx (F f) = simulate cntx f


---------------------------------------------------------------------
-- * Описание вычислительного процесса


-- |Описание многоуровневого вычислительного процесса PU. Подход к моделированию вдохновлён ISO
-- 15926. Процесс описывается относительно вычислительных блоков. При наличии вложенных блоков -
--        структура сохраняется.
data Process v x t
    = Process
        { steps     :: [Step v x t] -- ^Список шагов вычислительного процесса.
        , relations :: [Relation] -- ^Список отношений между шагами вычислительного процесса
                                  -- (отношения описываются через "кортежи" из ProcessUid).
        , nextTick  :: t          -- ^Номер первого свободного такта.
        , nextUid   :: ProcessUid -- ^Следующий свободный идентификатор шага вычислительного процесса.
        }
    deriving ( Show )

instance ( Default t ) => Default (Process v x t) where
    def = Process { steps=[], relations=[], nextTick=def, nextUid=def }


type ProcessUid = Int -- ^Уникальный идентификатор шага вычислительного процесса.

-- |Описание шага вычислительного процесса.
data Step v x t
    = Step
        { sKey  :: ProcessUid    -- ^Уникальный идентификатор шага.
        , sTime :: PlaceInTime t -- ^Описание типа и положения шага во времени.
        , sDesc :: StepInfo v x t -- ^Описание действия описываемого шага.
        }
    deriving ( Show )

-- |Описание положения события во времени и типа события:
data PlaceInTime t
    = Event t -- ^Мгновенные события, используются главным образом для описания событий САПР.
    | Activity ( Interval t ) -- ^Протяжённые во времени события. Используются замкнутые интервалы.
    deriving ( Show )

-- |Описание события, соответсвующего шага вычислительного процесса. Каждый вариант соответствует
-- соответствующему отдельному уровню организации вычислительного процесса.
data StepInfo v x t where
    -- |Решения, принятые на уровне САПР.
    CADStep :: String -> StepInfo v x t
    -- |Время работы над функциональным блоком функционального алгоритма.
    FStep :: F v x -> StepInfo v x t
    -- |Описание использования вычислительного блока с точки зрения передачи данных.
    EndpointRoleStep :: EndpointRole v -> StepInfo v x t
    -- |Описание инструкций, выполняемых вычислительным блоком. Список доступных инструкций
    -- определяется типом вычислительного блока.
    InstructionStep :: 
        ( Show (Instruction pu)
        , Typeable (Instruction pu)
        ) => Instruction pu -> StepInfo v x t
    -- |Используется для описания вычислительного процесса вложенных структурных элементов. Как
    -- правило не хранится в структурах данных, а генерируется автоматически по требованию при
    -- помощи опроса вложенных структурных элементов.
    NestedStep :: 
        ( Eq title, Show title, Ord title
        ) => 
            { nTitle :: title
            , nStep :: Step v x t
            } -> StepInfo v x t

descent Step{ sDesc=NestedStep _ step } = descent step
descent step                            = step

instance ( Show (Step v x t), Show v ) => Show (StepInfo v x t) where
    show (CADStep s)                 = s
    show (FStep (F f))               = show f
    show (EndpointRoleStep eff)      = show eff
    show (InstructionStep instr)     = show instr
    show NestedStep{ nTitle, nStep } = show nTitle ++ "." ++ show nStep


-- |Получить строку с название уровня указанного шага вычислительного процесса.
level CADStep{}           = "CAD"
level FStep{}             = "Function"
level EndpointRoleStep{}  = "Endpoint"
level InstructionStep{}   = "Instruction"
level (NestedStep _ step) = level $ sDesc $ descent step

showPU si = S.replace "\"" "" $ S.join "." $ showPU' si
    where
        showPU' (NestedStep title Step{ sDesc }) = show title : showPU' sDesc
        showPU' _                                = []

-- |Описание отношений между шагами вычисительного процесса.
data Relation
    -- |Отношение между шагами вычислительного процесса разных уровней, в котором второй шаг получен
    -- путём трансляции/детализации первого шага.
    = Vertical ProcessUid ProcessUid
    deriving (Show, Eq)



---------------------------------------------------------------------
-- * Варианты и решения
--
-- Именно в рамках этих терминов должна риализовываться основная логика процесса синтеза (по
-- крайней мере программного обеспечения). Используется следующее соглашение: Тип идентифицирующий
-- варианты и решения заканчивается суфиксом DT, вариант - O, решение - D.

-- |Решение в области привязки функционального блока к вычислительному. Определяется только для
-- вычислительных блоков, организующих работу со множеством вложенных блоков, адресуемым по title.
data BindingDT title v x
binding = Proxy :: Proxy BindingDT

instance DecisionType (BindingDT title v x) where
    data Option (BindingDT title v x) = BindingO (F v x) title deriving ( Generic )
    data Decision (BindingDT title v x) = BindingD (F v x) title deriving ( Generic )


-- |Взаимодействие PU с окружением. Подразумевается, что в один момент времени может быть только
-- одно взаимодействие, при этом у PU только один канал для взаимодействия, что в общем то
-- ограничение. В перспективе должно быть расширено для работы с конвейра.

-- TODO: В настоящий момен Source определяет множество вариантов выгрузки переменной. Это
-- неправильно и требует комплексной переработки.
data EndpointRole v
    = Source (S.Set v) -- ^ Выгрузка данных из PU.
    | Target v   -- ^ Загрузка данных в PU.
    deriving ( Show, Eq, Ord )

(Target a) << (Target b) | a == b = True
(Source a) << (Source b)          = all (`S.member` a) b
_        << _                     = False

(Source a) `sourceDifference` (Source b) = Source $ S.difference a b
a `sourceDifference` b = error $ "Can't get sub endpoint for " ++ show a ++ " " ++ show b

(\\\) a b = sourceDifference a b

instance Variables (EndpointRole v) v where
    variables (Source vs) = vs
    variables (Target v)  = S.singleton v



-- |Решение об использовании вычислительных блоков в роли источника или пункта назначения для
-- данных. В один момент времени может быть произведена только одна операция (вероятно, это
-- искусственное ограничение, навязанное архитектурой NL3). PU не может самостоятельно принимать
-- решение относительно своих взаимодействий с окружающим миром, он искючительно выполняет сказанные
-- ему операции.
data EndpointDT v t
endpointDT = Proxy :: Proxy EndpointDT

instance DecisionType (EndpointDT v t) where
    data Option (EndpointDT v t)
        = EndpointO
        { epoRole :: EndpointRole v -- ^ Чтение данных из входного регистра PU или запись данных в него.
        , epoAt :: TimeConstrain t -- ^ Временные ограничения на операцию.
        }
    data Decision (EndpointDT v t)
        = EndpointD
        { epdRole :: EndpointRole v -- ^ Выбранная операция для взаимодействия с окружающим миром.
        , epdAt :: Interval t -- ^ Положение операции во времени.
        }

instance Variables (Option (EndpointDT v t)) v where
    variables EndpointO{ epoRole } = variables epoRole
instance Variables (Decision (EndpointDT v t)) v where
    variables EndpointD{ epdRole } = variables epdRole
instance ( Show v, Show t, Eq t, Bounded t ) => Show (Option (EndpointDT v t)) where
    show EndpointO{ epoRole, epoAt } = "?" ++ show epoRole ++ "@(" ++ show epoAt ++ ")"
instance ( Show v, Show t, Eq t, Bounded t ) => Show (Decision (EndpointDT v t)) where
    show EndpointD{ epdRole, epdAt } = "!" ++ show epdRole ++ "@(" ++ show epdAt ++ ")"



---------------------------------------------------------------------
-- * Вычислительные блоки (PU)


-- |Описание вычислительного блока. Используется в совокупности с Decision по интересующим группам
-- вопросов.
--
-- Идеологически, планирование вычислительного процесса производится следующим образом:
--
-- 1. Вычислительному блоку назначаются испоняемые им функции.
-- 2. Блок опрашивается на предмет возможных вариантов развития вычислительного процесса.
-- 3. Выбранный вариант развития вычислительного процесса моделируется в рамках вычислительного
--    блока. Его модельное время продвигается вперёд, в описании вычислительного процесса
--    дополняется записями относительно сделанных шагов вычислительного процесса.
-- 4. Повторение, пока список возможных вариантов не станет пустым.
class ProcessUnit pu v x t | pu -> v x t where
    -- |Назначить исполнение функционального блока вычислительному узлу.
    tryBind :: F v x -> pu -> Either String pu
    -- |Запрос описания вычилсительного процесса с возможностью включения описания вычислительного
    -- процесс вложенных структурных элементов.
    --
    -- Результат вычисления данной функции не должен редактироваться и возкращаться на место!
    process :: pu -> Process v x t
    -- |Установить модельное время вычислительного блока.
    --
    -- История вопроса: Изначально, данный метод был добавлен для работы в ращеплённом времени, но он:
    --
    -- 1. недостаточен,
    -- 2. может быть реализован в рамках алгоритма компиляции.
    --
    -- В тоже время, setTime нужен не только для того, чтобы ограничить время, но и для того, что бы установить тег
    -- времени.
    --
    -- Вероятно, хорошим вариантом является жёсткое отслеживание времени и как следствие - явная изменение его тега /
    -- значения. Логично ожидать что данная операция будет применяться ко сему дереву вычислителя.

    -- TODO: Необходимо преобразовать в setTimeTag. Добавить метод skip, для того что бы вычислительный блок мог
    -- пропускать отдельный переменные (необходимо для ветвления вычислительного процесса).
    setTime :: t -> pu -> pu


class WithX pu x | pu -> x where
    proxyX :: pu -> Proxy x
    proxyX _ = Proxy


bind fb pu = case tryBind fb pu of
    Right pu' -> pu'
    Left err  -> error $ "Can't bind F to PU: " ++ err

allowToProcess fb pu
    | Right _ <- tryBind fb pu = True
    | otherwise = False


---------------------------------------------------------------------
-- * Сигналы и инструкции

-- |Класс управляемых вычислительных блоков.
--
-- Определяет низкоуровневые интерфейсы управления вычислительным блоком. Описываются два уровня
-- организации вычислительного процесса: на уровне инструкций вычислительного блока и на уровне
-- микрокоманд вычислительного блока.
class Controllable pu where
    -- |Инструкции вычислительного блока описывают его поведения с точки зрения разработчика. Если
    -- вычислительный блок не выполняет никаких операций (находится в состоянии ожидания), то его
    -- поведение не описывается никакой инструкцией.
    data Instruction pu :: *

    -- |Структура микрокода управляющего поведением вычислительного блока.
    -- Может быть использован непосредственно для управления аппаратной частью
    -- вычислительного блока, так как содержит непосредственно значения управляющих
    -- сигналов.
    data Microcode pu :: *


-- |Метод, необходимый для управляемых блоков обработки данных. Позволяет узнать микрокоманду
-- для конкретного такта вычислительного процесса.
class ByTime pu t | pu -> t where
    microcodeAt :: pu -> t -> Microcode pu


-- |Декодирование инструкции в микрокод.
--
-- Не может быть реализована для инструкций сетей, так как: (1) инструкция для сетей описывает
-- несколько тактов, а значит должна возвращать последовательность значений микрокода; (2)
-- несколько инструкций может выполняться одновременно.
class UnambiguouslyDecode pu where
    decodeInstruction :: Instruction pu -> Microcode pu


-- |Значение сигнальной линии.
data SignalValue
    -- |Значение не определено.
    = Undef
    -- |Значение сигнальной линии установлено в логическое значение.
    | Bool Bool
    -- |Была сделана попытка установить сигнальную линию несколькими источниками, что привело к
    -- колизии и битому значению.
    | Broken
    deriving ( Eq )

instance Default SignalValue where
    def = Undef

instance Show SignalValue where
    show Undef        = "x"
    show (Bool True)  = "1"
    show (Bool False) = "0"
    show Broken       = "B"

Undef +++ v = v
v +++ Undef = v
_ +++ _ = Broken




---------------------------------------------------------------------
-- * Синтез и тестирование вычислительных блоков

-- |Контекст модельного вычислительного процесса представленный словарём переменных и их значений.
-- В качестве ключа используется имя переменной и индекс. Это необходимо для того, что бы
-- моделировать вычислительный процесс на разных циклах. При этом не очень ясно, модет ли работать
-- данная конструкция в TaggedTime в принципе.

-- |Класс предназначенный для симуляции вычислительного процесса. Может использоваться как просто
-- для моделирования, так и для генерации TestBench-а. Работает только с уже спланированным
-- вычислительным процессом (с одной стороны это искуственное ограничение первых версий, с другой
-- стороны - это позволяет учитывать внутренее состояние вычислительного блока, что может быть
-- полезным при работе со значеними по умолчанию).
class Simulatable pu v x | pu -> v x where
    simulateOn
        :: Cntx v x -- ^Контекст вычислительного процесса, содержащий уже
                    -- известные значения переменных.
            -> pu -- ^Вычислительный блок.
            -> F v x -- ^Функциональный блок, оперируйщий интересующим значением.
            -> Maybe (Cntx v x)
