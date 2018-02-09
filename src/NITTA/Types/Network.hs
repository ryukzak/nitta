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

module NITTA.Types.Network where

import           Data.Typeable
import           NITTA.Types.Base
import           NITTA.Types.Poly


-- | Контейнер для вычислительных узлов (PU). Необходимо для формирования гетерогенных списков.
data PU v t where
  PU :: ( Typeable pu
        , Show (Signal pu)
        , ProcessUnit pu v t
        , ByTime pu t
        , Synthesis pu
        , Simulatable pu v Int
        , DecisionProblem (EndpointDT v t)
               EndpointDT  pu
        ) => { unit :: pu
             } -> PU v t
setUnit PU{..} unit' = PU{ unit=unit' } 

instance ( Var v, Time t
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (PU v t)
         where
  options proxy PU{..} = options proxy unit
  decision proxy pu@PU{..} act = setUnit pu $ decision proxy unit act

instance ProcessUnit (PU v t) v t where
  bind fb pu@PU{..} = setUnit pu <$> bind fb unit
  process PU{..} = process unit
  setTime t pu@PU{..} = setUnit pu $ setTime t unit

instance Simulatable (PU v t) v Int where
  simulateOn cntx PU{..} fb = simulateOn cntx unit fb

instance Synthesis (PU v t) where
  name PU{..} = name unit
  hardwareInstance PU{..} = hardwareInstance unit
  hardware PU{..} = hardware unit
  software PU{..} = software unit

castPU :: ( Typeable pu
          , Show (Signal pu)
          , ProcessUnit pu v t
          , ByTime pu t
          , Synthesis pu
          , Simulatable pu v Int
          , DecisionProblem (EndpointDT v t)
                 EndpointDT  pu
          ) => PU v t -> Maybe pu
castPU PU{..} = cast unit


-- | Решения относительно пересылки данных между вычислитльными узлами (реализация прикладного
-- DataFlow).
data DataFlowDT title v t
dataFlowDT = Proxy :: Proxy DataFlowDT

instance DecisionType (DataFlowDT title v t) where
  data Option (DataFlowDT title v t)
    = DataFlowO
    { dfoSource     :: (title, TimeConstrain t) -- ^ Источник пересылки.
    -- | Словарь, описывающий все необходимые пункты назначения для пересылаемого значения.
    -- Допустима ситация, когда пункт назначения не может принять значение, в таком случае для
    -- негоне указываются временные ограничения.
    --
    -- Примечание: почему title оказался под Maybe? Потому что мы можем, банально, не знать в каком
    -- PU находится требуемый функциональный блок, так как он может быть ещё непривязан к PU.
    , dfoTargets    :: M.Map v (Maybe (title, TimeConstrain t))
    } deriving ( Show )
  data Decision (DataFlowDT title v t)
    = DataFlowD
    { dfdSource     :: (title, Interval t) -- ^ Источник пересылки.
    -- | Словарь, описывающий пункты назначения для пересылаемого значения.
    , dfdTargets    :: M.Map v (Maybe (title, Interval t))
    } deriving ( Show )
