{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : NITTA.Model.ProcessorUnits.Types
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Types
    ( UnitTag
    , ProcessorUnit(..), Controllable(..), UnambiguouslyDecode(..)
    , Process(..), ProcessStepID, Step(..), StepInfo(..), Relation(..)
    , descent, whatsHappen, extractInstructionAt
    , bind, allowToProcess
    , Connected(..), SignalTag(..), SignalValue(..), (+++)
    , IOConnected(..), InputPortTag(..), OutputPortTag(..), InoutPortTag(..)
    , ByTime(..)
    ) where

import           Data.Default
import           Data.Ix
import           Data.Kind
import qualified Data.List as L
import           Data.Maybe
import qualified Data.String.Utils as S
import           Data.Typeable
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems
import           NITTA.Model.Types
import           NITTA.Utils.CodeFormat
import           Numeric.Interval
import qualified Numeric.Interval as I
import           Text.InterpolatedString.Perl6 ( qc )


-- |Typeclass alias for processor unit tag.
type UnitTag tag = ( Typeable tag, Ord tag, Show tag )


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
class ( VarValTime v x t ) => ProcessorUnit u v x t | u -> v x t where
    -- |Назначить исполнение функционального блока вычислительному узлу.
    tryBind :: F v x -> u -> Either String u
    -- |Запрос описания вычилсительного процесса с возможностью включения описания вычислительного
    -- процесс вложенных структурных элементов.
    --
    -- Результат вычисления данной функции не должен редактироваться и возкращаться на место!
    process :: u -> Process v x t


bind fb pu = case tryBind fb pu of
    Right pu' -> pu'
    Left err  -> error $ "Can't bind F to PU: " ++ err


allowToProcess fb pu
    | Right _ <- tryBind fb pu = True
    | otherwise = False


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
        , nextUid   :: ProcessStepID -- ^Следующий свободный идентификатор шага вычислительного процесса.
        }

instance (VarValTime v x t) => Show (Process v x t) where
    show p = codeBlock [qc|
        Process
            steps     =
                {inline $ listShow $ steps p  }
            relations =
                {inline $ listShow $ relations p  }
            nextTick  = { show ( nextTick p ) }
            nextUid   = { show ( nextUid p ) }
        |]
        where
            listShow list = unlines $ map (\(i, value) -> [qc|{i}) {value}|]) $ zip [0::Integer ..] list

instance ( Default t ) => Default (Process v x t) where
    def = Process { steps=[], relations=[], nextTick=def, nextUid=def }

instance ( Ord t ) => WithFunctions (Process v x t) (F v x) where
    functions Process{ steps } = mapMaybe get $ L.sortOn (I.inf . sTime) steps
        where
            get Step{ sDesc } | FStep f <- descent sDesc = Just f
            get _             = Nothing


-- |Unique ID of a process step. Uniquity presented only inside PU.
type ProcessStepID = Int

-- |Описание шага вычислительного процесса.
data Step v x t
    = Step
        { sKey  :: ProcessStepID    -- ^Уникальный идентификатор шага.
        , sTime :: Interval t -- ^Описание типа и положения шага во времени.
        , sDesc :: StepInfo v x t -- ^Описание действия описываемого шага.
        }
    deriving (Show)


instance ( Ord v ) => Patch (Step v x t) (Changeset v) where
    patch diff step@Step{ sDesc } = step{ sDesc=patch diff sDesc }


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
        ( UnitTag tag
        ) =>
            { nTitle :: tag
            , nStep :: Step v x t
            } -> StepInfo v x t

descent (NestedStep _ step) = descent $ sDesc step
descent desc                = desc

instance ( Show (Step v x t), Show v ) => Show (StepInfo v x t) where
    show (CADStep s)                 = s
    show (FStep F{ fun })            = show fun
    show (EndpointRoleStep eff)      = show eff
    show (InstructionStep instr)     = show instr
    show NestedStep{ nTitle, nStep } = S.replace "\"" "" ("Nested " ++ show nTitle ++ ": " ++ show nStep)

instance ( Ord v ) => Patch (StepInfo v x t) (Changeset v) where
    patch diff (FStep f)              = FStep $ patch diff f
    patch diff (EndpointRoleStep ep)  = EndpointRoleStep $ patch diff ep
    patch diff (NestedStep tag nStep) = NestedStep tag $ patch diff nStep
    patch _    i                      = i


-- |Описание отношений между шагами вычисительного процесса.
data Relation
    -- |Отношение между шагами вычислительного процесса разных уровней, в котором второй шаг получен
    -- путём трансляции/детализации первого шага.
    = Vertical ProcessStepID ProcessStepID
    deriving ( Show, Eq )


---------------------------------------------------------------------
-- * Сигналы и инструкции

-- |Type class for controllable units. Defines two level of a unit behaviour representation (see
-- ahead).
class Controllable pu where
    -- |Instruction describe unit behaviour on each mUnit cycle. If instruction not defined for
    -- some cycles - it should be interpreted as NOP. In future, Instruction should be extracted, because
    data Instruction pu :: Type

    -- |Microcode desctibe controll signals on each mUnit cycle (without exclusion).
    data Microcode pu :: Type

    -- |Map microcode to unit signal ports.
    mapMicrocodeToPorts :: Microcode pu -> Ports pu -> [(SignalTag, SignalValue)]

    -- |Get list of signals from Ports pu
    portsToSignals :: Ports pu -> [SignalTag]

    -- |Get Ports from list of signals
    signalsToPorts :: [SignalTag] -> pu -> Ports pu



-- |Type class of processor units with control ports.
class Connected pu where
    -- |A processor unit control ports (signals, flags).
    data Ports pu :: Type

-- |Type class of processor units with IO ports.
class IOConnected pu where
    data IOPorts pu :: Type
    -- |External input ports, which go outside of NITTA mUnit.
    inputPorts :: IOPorts pu -> [ InputPortTag ]
    inputPorts _ = []
    -- |External output ports, which go outside of NITTA mUnit.
    outputPorts :: IOPorts pu -> [ OutputPortTag ]
    outputPorts _ = []
    -- |External output ports, which go outside of NITTA mUnit.
    inoutPorts :: IOPorts pu -> [ InoutPortTag ]
    inoutPorts _ = []

newtype SignalTag = SignalTag { signalTag :: Int} deriving ( Show, Eq, Ord, Ix )
newtype InputPortTag = InputPortTag{ inputPortTag :: String } deriving ( Show, Eq, Ord )
newtype OutputPortTag = OutputPortTag{ outputPortTag :: String } deriving ( Show, Eq, Ord )
newtype InoutPortTag = InoutPortTag{ inoutPortTag :: String } deriving ( Show, Eq, Ord )



-- |Метод, необходимый для управляемых блоков обработки данных. Позволяет узнать микрокоманду
-- для конкретного такта вычислительного процесса.
class ByTime pu t | pu -> t where
    microcodeAt :: pu -> t -> Microcode pu

instance ( Show (Instruction pu)
         , Default (Microcode pu)
         , ProcessorUnit pu v x t
         , UnambiguouslyDecode pu
         , Time t
         , Typeable pu
         ) => ByTime pu t where

    microcodeAt pu t = case extractInstructionAt pu t of
        []  -> def
        [i] -> decodeInstruction i
        is  -> error $ "instruction collision at " ++ show t ++ " tick: " ++ show is ++ show (process pu)


whatsHappen t Process{ steps } = filter (atSameTime t . sTime) steps

extractInstructionAt pu t = mapMaybe (inst pu) $ whatsHappen t $ process pu
    where
        inst :: ( Typeable (Instruction pu) ) => pu -> Step v x t -> Maybe (Instruction pu)
        inst _ Step{ sDesc=InstructionStep instr } = cast instr
        inst _ _                                   = Nothing


atSameTime a ti = a `member` ti


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
