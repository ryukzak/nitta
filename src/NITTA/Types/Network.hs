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
import qualified Data.Map         as M
import           Data.Typeable
import           GHC.Generics     (Generic)
import           NITTA.Types.Base
import           NITTA.Types.Poly
import           Numeric.Interval hiding (elem)


-- | Контейнер для вычислительных узлов (PU). Необходимо для формирования гетерогенных списков.
data PU i v x t where
  PU :: ( ByTime pu t
        , Connected pu i
        , DecisionProblem (EndpointDT v t)
               EndpointDT  pu
        , Default (Instruction pu)
        , ProcessUnit pu (Parcel v x) t
        , Show (Instruction pu)
        , Simulatable pu v x
        , Synthesis pu i
        , Typeable i
        , Typeable pu
        , UnambiguouslyDecode pu
        , Typeable x
        , Show x
        , Num x
        ) => { unit :: pu
             , links :: Link pu i
             , networkLink :: NetworkLink i
             } -> PU i v x t

instance ( Var v, Time t
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (PU i v x t)
         where
  options proxy PU{..} = options proxy unit
  decision proxy PU{ unit, links, networkLink } d
    = PU{ unit=decision proxy unit d, links, networkLink }

instance ProcessUnit (PU i v x t) (Parcel v x) t where
  bind fb PU{ unit, links, networkLink }
    = case bind fb unit of
      Right unit' -> Right PU { unit=unit', links, networkLink }
      Left err    -> Left err
  process PU{ unit } = process unit
  setTime t PU{ unit, links, networkLink }
    = PU{ unit=setTime t unit, links, networkLink }

instance Simulatable (PU i v x t) v x where
  simulateOn cntx PU{..} fb = simulateOn cntx unit fb

instance DefinitionSynthesis (PU i v x t) where
  moduleName PU{..} = moduleName unit
  hardware PU{..} = hardware unit
  software PU{..} = software unit

castPU :: ( ByTime pu t
          , Connected pu i
          , DecisionProblem (EndpointDT v t)
                 EndpointDT  pu
          , Default (Instruction pu)
          , DefinitionSynthesis pu
          , ProcessUnit pu v t
          , Show (Instruction pu)
          , Simulatable pu v x
          , Synthesis pu i
          , Typeable i
          , Typeable pu
          , UnambiguouslyDecode pu
          , Typeable x
          , Show x
          , Num x
          ) => PU i v x t -> Maybe pu
castPU PU{..} = cast unit


class Connected pu i where
  -- | Линии специфичные для подключения вычислительного блока к рабочему окружению: управляющие
  -- сигналы, флаги, подключения к внешнему миру.
  data Link pu i :: *
  -- | Отображение микрокода на сигнальные линии. Необходимо для "сведения" микрокоманд отдельных
  -- вычислительных блоков в микрокоманды сети.
  transmitToLink :: Microcode pu -> Link pu i -> [(i, Value)]



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


class ( DefinitionSynthesis pu ) => Synthesis pu i where
  -- | Объявление экземпляра модуля. Используется для генерации процессоров и testbench-ей.
  --
  -- Конфигурирование вычислительного блока осуществляется через подаваемы на вход словарь. В
  -- настоящий момент данная функция не является типо-безопастной и не отличается runtime
  -- проверками, что конечно никуда не годится.
  hardwareInstance :: pu -> String -> NetworkLink i -> Link pu i -> String


-- | Подключения к сети.
data NetworkLink i
  = NetworkLink
    { clk, rst :: i
    , dataWidth :: i, attrWidth :: i
    , dataIn, attrIn :: i
    , dataOut, attrOut :: i
    , cycleStart :: i -- ^ Сигнал о начале вычислительного цикла.
    , controlBus :: i -> i -- ^ Функция позволяющая подставить индекс на шину управления.
    }


data LinkId = Index Int
            | Name String
            deriving ( Show, Eq, Ord )

link (Index i) = show i
link (Name n)  = n
