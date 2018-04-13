{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Types.Network where

import           Data.Default
import           Data.Ix
import qualified Data.Map         as M
import           Data.Typeable
import           GHC.Generics     (Generic)
import           NITTA.Types.Base
import           NITTA.Types.Poly
import           Numeric.Interval hiding (elem)


-- | Контейнер для вычислительных узлов (PU). Необходимо для формирования гетерогенных списков.
data PU v x t where
  PU :: ( ByTime pu t
        , Connected pu
        , DecisionProblem (EndpointDT v t)
               EndpointDT  pu
        , Default (Instruction pu)
        , ProcessUnit pu (Parcel v x) t
        , Show (Instruction pu)
        , Simulatable pu v x
        , Synthesis pu
        , Typeable pu
        , UnambiguouslyDecode pu
        , Typeable x
        , Show x
        , Num x
        ) => { unit :: pu
             , links :: PUPorts pu
             , systemEnv :: Enviroment
             } -> PU v x t

instance ( Var v, Time t
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (PU v x t)
         where
  options proxy PU{..} = options proxy unit
  decision proxy PU{ unit, links, systemEnv } d
    = PU{ unit=decision proxy unit d, links, systemEnv }

instance ProcessUnit (PU v x t) (Parcel v x) t where
  bind fb PU{ unit, links, systemEnv }
    = case bind fb unit of
      Right unit' -> Right PU { unit=unit', links, systemEnv }
      Left err    -> Left err
  process PU{ unit } = process unit
  setTime t PU{ unit, links, systemEnv }
    = PU{ unit=setTime t unit, links, systemEnv }

instance Simulatable (PU v x t) v x where
  simulateOn cntx PU{..} fb = simulateOn cntx unit fb

instance DefinitionSynthesis (PU v x t) where
  moduleName name PU{..} = moduleName name unit
  hardware name PU{..} = hardware name unit
  software name PU{..} = software name unit

castPU :: ( ByTime pu t
          , Connected pu
          , DecisionProblem (EndpointDT v t)
                 EndpointDT  pu
          , Default (Instruction pu)
          , DefinitionSynthesis pu
          , ProcessUnit pu v t
          , Show (Instruction pu)
          , Simulatable pu v x
          , Synthesis pu
          , Typeable pu
          , UnambiguouslyDecode pu
          , Typeable x
          , Show x
          , Num x
          ) => PU v x t -> Maybe pu
castPU PU{..} = cast unit


class Connected pu where
  -- | Линии специфичные для подключения вычислительного блока к рабочему окружению: управляющие
  -- сигналы, флаги, подключения к внешнему миру.
  data PUPorts pu :: *
  -- | Отображение микрокода на сигнальные линии. Необходимо для "сведения" микрокоманд отдельных
  -- вычислительных блоков в микрокоманды сети.
  transmitToLink :: Microcode pu -> PUPorts pu -> [(Signal, Value)]



-- | Решения относительно пересылки данных между вычислитльными узлами (реализация прикладного
-- DataFlow).
data DataFlowDT title v t
dataFlowDT = Proxy :: Proxy DataFlowDT

type Source title t = (title, t)
type Target title v t = M.Map v (Maybe (title, t))

instance DecisionType (DataFlowDT title v t) where
  data Option (DataFlowDT title v t)
    = DataFlowO
    { dfoSource     :: Source title (TimeConstrain t) -- ^ Источник пересылки.
    -- | Словарь, описывающий все необходимые пункты назначения для пересылаемого значения.
    -- Допустима ситация, когда пункт назначения не может принять значение, в таком случае для
    -- негоне указываются временные ограничения.
    --
    -- Примечание: почему title оказался под Maybe? Потому что мы можем, банально, не знать в каком
    -- PU находится требуемый функциональный блок, так как он может быть ещё непривязан к PU.
    , dfoTargets    :: Target title v (TimeConstrain t)
    } deriving ( Show, Generic )
  data Decision (DataFlowDT title v t)
    = DataFlowD
    { dfdSource     :: Source title (Interval t) -- ^ Источник пересылки.
    -- | Словарь, описывающий пункты назначения для пересылаемого значения.
    , dfdTargets    :: Target title v (Interval t)
    } deriving ( Show, Generic )


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
  moduleName :: String -> pu -> String
  -- | Реализация аппаратной составляющей вычислительного блока.
  hardware :: String -> pu -> Implementation
  -- | Реализация программной составляющей составляющей вычислительного блока.
  software :: String -> pu -> Implementation


class ( DefinitionSynthesis pu ) => Synthesis pu where
  -- | Объявление экземпляра модуля. Используется для генерации процессоров и testbench-ей.
  --
  -- Конфигурирование вычислительного блока осуществляется через подаваемы на вход словарь. В
  -- настоящий момент данная функция не является типо-безопастной и не отличается runtime
  -- проверками, что конечно никуда не годится.
  hardwareInstance :: String -> pu -> Enviroment -> PUPorts pu -> String

  testBenchEnviroment :: String -> pu -> Enviroment -> PUPorts pu -> String
  testBenchEnviroment _ _ _ _ = ""


-- | Описание подключения сигнальных шин управления.
newtype Signal = Signal Int deriving ( Show, Eq, Ord, Ix )
-- | Описание подключения шин ввода вывода.
newtype InputPort = InputPort String deriving ( Show, Eq, Ord )
newtype OutputPort = OutputPort String deriving ( Show, Eq, Ord )
data Parameter 
  = InlineParam String
  | IntParam Int
  deriving ( Eq, Ord )
instance Show Parameter where
  show (IntParam i) = show i
  show (InlineParam s) = s
  

data NetEnv
  = NetEnv
    { parameterDataWidth :: Parameter
    , parameterAttrWidth :: Parameter
    , dataIn, attrIn     :: String
    , dataOut, attrOut   :: String
    , signal             :: Signal -> String -- ^ Функция позволяющая подставить индекс на шину управления.
    }

-- | Подключения к сети.
data Enviroment
  = Enviroment
    { signalClk   :: String
    , signalRst   :: String
    , signalCycle :: String -- ^ Сигнал о начале вычислительного цикла.
    , inputPort   :: InputPort -> String
    , outputPort  :: OutputPort -> String
    , net         :: NetEnv
    }
