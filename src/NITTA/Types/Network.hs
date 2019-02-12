{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Types.Network
Description :
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Types.Network where

import           Data.Ix
import qualified Data.Map         as M
import           Data.Typeable
import           GHC.Generics     (Generic)
import           NITTA.Types.Base
import           NITTA.Types.Poly
import           Numeric.Interval hiding (elem)


-- |Контейнер для вычислительных узлов (PU). Необходимо для формирования гетерогенных списков.
data PU v x t where
    PU :: 
        ( ByTime pu t
        , Connected pu
        , DecisionProblem (EndpointDT v t)
               EndpointDT  pu
        , ProcessUnit pu v x t
        , Show (Instruction pu)
        , Simulatable pu v x
        , Typeable pu
        , UnambiguouslyDecode pu
        , TargetSystemComponent pu
        , IOTest pu v x
        , Typeable x
        , Show x
        , Num x
        , Locks pu v
        ) => 
            { unit :: pu
            , links :: PUPorts pu
            , systemEnv :: Enviroment
            } -> PU v x t

instance DecisionProblem (EndpointDT v t)
              EndpointDT (PU v x t)
         where
    options proxy PU{ unit } = options proxy unit
    decision proxy PU{ unit, links, systemEnv } d
        = PU{ unit=decision proxy unit d, links, systemEnv }

instance ProcessUnit (PU v x t) v x t where
    tryBind fb PU{ unit, links, systemEnv }
        = case tryBind fb unit of
            Right unit' -> Right PU { unit=unit', links, systemEnv }
            Left err    -> Left err
    process PU{ unit } = process unit
    setTime t PU{ unit, links, systemEnv }
        = PU{ unit=setTime t unit, links, systemEnv }

instance Locks (PU v x t) v where
    locks PU{ unit } = locks unit

instance Simulatable (PU v x t) v x where
    simulateOn cntx PU{ unit } fb = simulateOn cntx unit fb

instance TargetSystemComponent (PU v x t) where
    moduleName name PU{ unit } = moduleName name unit
    hardware name PU{ unit } = hardware name unit
    software name PU{ unit } = software name unit
    hardwareInstance name pu = hardwareInstance name pu

instance IOTest (PU v x t) v x where
    componentTestEnviroment name PU{ unit, systemEnv, links } _systemEnv _links cntxs
        = componentTestEnviroment name unit systemEnv links cntxs


castPU :: ( Typeable pu ) => PU v x t -> Maybe pu
castPU PU{ unit } = cast unit


class Connected pu where
    -- |Линии специфичные для подключения вычислительного блока к рабочему окружению: управляющие
    -- сигналы, флаги, подключения к внешнему миру.
    data PUPorts pu :: *
    -- |Отображение микрокода на сигнальные линии. Необходимо для "сведения" микрокоманд отдельных
    -- вычислительных блоков в микрокоманды сети.
    transmitToLink :: Microcode pu -> PUPorts pu -> [(Signal, SignalValue)]
    -- |External input ports, which go outside of NITTA processor.
    externalInputPorts :: PUPorts pu -> [InputPort]
    externalInputPorts _ = []
    -- |External output ports, which go outside of NITTA processor.
    externalOutputPorts :: PUPorts pu -> [OutputPort]
    externalOutputPorts _ = []



-- |Решения относительно пересылки данных между вычислитльными узлами (реализация прикладного
-- DataFlow).
data DataFlowDT title v t
dataFlowDT = Proxy :: Proxy DataFlowDT

type Source title t = (title, t)
type Target title v t = M.Map v (Maybe (title, t))

instance DecisionType (DataFlowDT title v t) where
    data Option (DataFlowDT title v t)
        = DataFlowO
        { dfoSource     :: Source title (TimeConstrain t) -- ^Источник пересылки.
        -- |Словарь, описывающий все необходимые пункты назначения для пересылаемого значения.
        -- Допустима ситация, когда пункт назначения не может принять значение, в таком случае для
        -- негоне указываются временные ограничения.
        --
        -- Примечание: почему title оказался под Maybe? Потому что мы можем, банально, не знать в
        -- каком PU находится требуемый функциональный блок, так как он может быть ещё непривязан к
        -- PU.
        , dfoTargets    :: Target title v (TimeConstrain t)
        } deriving ( Show, Generic )
    data Decision (DataFlowDT title v t)
        = DataFlowD
        { dfdSource     :: Source title (Interval t) -- ^Источник пересылки.
        -- |Словарь, описывающий пункты назначения для пересылаемого значения.
        , dfdTargets    :: Target title v (Interval t)
        } deriving ( Show, Generic )


-- |Реализация компонента системы или её фрагмента (HW или SW).

-- TODO: Рассмотреть возможность отказа от Empty в пользу оборачивания в Maybe.
data Implementation
    -- |Непосредственно реализация компонента.
    = Immidiate { impFileName, impText :: String }
    -- |Библиотечный элемент, приведённый в указанном файле.
    | FromLibrary { impFileName :: String }
    -- |Релизация описывается совокупностью файлов располагаемых в указанном каталоге относительно
    -- рабочей папки.
    | Aggregate { impPath :: Maybe String, subComponents :: [ Implementation ] }
    -- |Реализация не требуется (к примеру: для многих вычислительных блоков ПО отсутствует).
    | Empty

-- |Класс для кодогенерации для встраивания вычислительного блока в процессор.
class TargetSystemComponent pu where
    -- |Наименование аппаратного модуля, соответствующего вычислительному блоку.
    moduleName :: String -> pu -> String

    -- |Программное обеспечение для вычислительного блока. Под ПО вычислительного блока понимается
    -- настройка вычислительного блока, которая может меняться при изменении прикладного алгоритма
    -- без повторного синтеза аппаратуры. Наличие ПО не является обязательным.
    software :: String -> pu -> Implementation

    -- |Аппаратное обеспечение вычислительного блока. Это может быть ссылка на библиотеку,
    -- сгенерированный файл либо их совокупность.
    --
    -- В связи с тем, что в проекте используется несколько целевых платформ (на момент написания это
    -- Icarus Verilog для тестирования и Quartus для синтеза), для отдельных вычислительных блоков
    -- может осуществляться конфигурировние.
    hardware :: String -> pu -> Implementation

    -- |Генерация фрагмента исходного кода для создания экземпляра вычислительного блока в рамках
    -- вычислительной платформы NITTA.
    hardwareInstance :: String -> pu -> Enviroment -> PUPorts pu -> String


class IOTest pu v x | pu -> v x where
    -- |Для автоматизированного тестирования компонент со внешними портами ввода/вывода необходимо
    -- специализированное тестовое окружение, имитирующее ввод/вывод.
    componentTestEnviroment :: String -> pu -> Enviroment -> PUPorts pu -> [Cntx v x] -> String
    componentTestEnviroment _title _pu _env _ports _cntxs = ""


-- |Описание подключения сигнальных шин управления.
newtype Signal = Signal Int deriving ( Show, Eq, Ord, Ix )
-- |Описание подключения шин ввода вывода.
newtype InputPort = InputPort String deriving ( Show, Eq, Ord )
newtype OutputPort = OutputPort String deriving ( Show, Eq, Ord )
data Parameter
    = InlineParam String
    | IntParam Int
    deriving ( Eq, Ord )
instance Show Parameter where
    show (IntParam i)    = show i
    show (InlineParam s) = s


data NetEnv
    = NetEnv
        { parameterAttrWidth :: Parameter
        , dataIn, attrIn     :: String
        , dataOut, attrOut   :: String
        , signal             :: Signal -> String -- ^Функция позволяющая подставить индекс на шину управления.
        }

-- |Подключения к сети.
data Enviroment
    = Enviroment
        { signalClk   :: String
        , signalRst   :: String
        , signalCycle :: String -- ^Сигнал о начале вычислительного цикла.
        , inputPort   :: InputPort -> String
        , outputPort  :: OutputPort -> String
        , net         :: NetEnv
        }
