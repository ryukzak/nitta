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
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures -fno-warn-orphans #-}

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
    , module NITTA.Types.Function
    , module NITTA.Types.Time
    ) where

import           Data.Default
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import           Data.Proxy
import qualified Data.Set             as S
import qualified Data.String.Utils    as S
import           Data.Typeable
import           GHC.Generics
import           NITTA.Types.Function
import           NITTA.Types.Poly
import           NITTA.Types.Time
import           Numeric.Interval


---------------------------------------------------------------------
-- * Функциональные блоки

class WithFunctions a f | a -> f where
    -- |Получить список связанных функциональных блоков.
    functions :: a -> [f]



---------------------------------------------------------------------
-- *Computational process


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

instance ( Ord v ) => Patch (Step v x t) (Diff v) where
    patch diff step@Step{ sDesc } = step{ sDesc=patch diff sDesc }


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
        ( Show title, Ord title
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

instance ( Ord v ) => Patch (StepInfo v x t) (Diff v) where
    patch diff (FStep f)                = FStep $ patch diff f
    patch diff (EndpointRoleStep ep)    = EndpointRoleStep $ patch diff ep
    patch diff (NestedStep title nStep) = NestedStep title $ patch diff nStep
    patch _    i                        = i


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
    deriving ( Eq, Ord )

instance {-# OVERLAPPABLE #-} ( Show v ) => Show (EndpointRole v) where
    show (Source vs) = "Source " ++ S.join "," (map show $ S.elems vs)
    show (Target v) = "Target " ++ show v

instance {-# OVERLAPS #-} Show (EndpointRole String) where
    show (Source vs) = "Source " ++ S.join "," (S.elems vs)
    show (Target v) = "Target " ++ v

instance ( Ord v ) => Patch (EndpointRole v) (Diff v) where
    patch Diff{ diffI } (Target v) = Target $ fromMaybe v $ diffI M.!? v
    patch Diff{ diffO } (Source vs)
        = Source $ S.fromList $ map (\v -> fromMaybe v $ diffO M.!? v) $ S.elems vs

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

instance ( Ord v ) => Patch (Option (EndpointDT v t)) (Diff v) where
    patch diff ep@EndpointO{ epoRole } = ep{ epoRole=patch diff epoRole }
instance ( Ord v ) => Patch (Decision (EndpointDT v t)) (Diff v) where
    patch diff ep@EndpointD{ epdRole } = ep{ epdRole=patch diff epdRole }



data RefactorDT v
refactorOptions m = options (Proxy :: Proxy RefactorDT) m
refactorDecision m d = decision (Proxy :: Proxy RefactorDT) m d

instance DecisionType (RefactorDT v) where
    data Option (RefactorDT v)
        -- |Example:
        --
        -- >>> f1 :: (...) -> (a)
        -- f2 :: (a, ...) -> (...)
        -- f1 and f2 process on same processor
        -- In this case, we have deadlock, witch can be fixed by insetion of register between functions:
        -- f1 :: (...) -> (a)
        -- reg :: a -> buf_a
        -- f2 :: (buf_a, ...) -> (...)
        = InsertOutRegisterO v
        deriving ( Generic, Show, Eq, Ord )
    data Decision (RefactorDT v)
        = InsertOutRegisterD v v
        deriving ( Generic, Show )



---------------------------------------------------------------------
-- * Processor & Process Unit (PU)


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
