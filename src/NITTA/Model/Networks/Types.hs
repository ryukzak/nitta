{-# LANGUAGE ConstraintKinds        #-}
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
Module      : NITTA.Model.Networks.Types
Description : Types for processor unit network description.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Networks.Types
    ( PU(..)
    ) where

import qualified Data.Map                         as M
import qualified Data.Set                         as S
import           Data.Typeable
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.Problems.Types
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Parts.TestBench


-- |Existential container for a processor unit .
data PU v x t where
    PU :: 
        ( ByTime pu t
        , Connected pu
        , DecisionProblem (EndpointDT v t)
               EndpointDT  pu
        , ProcessorUnit pu v x t
        , Show (Instruction pu)
        , Simulatable pu v x
        , Typeable pu
        , UnambiguouslyDecode pu
        , TargetSystemComponent pu
        , Controllable pu
        , IOTestBench pu v x
        , Locks pu v
        ) => 
            { diff :: Diff v
            , unit :: pu
            , ports :: Ports pu
            , systemEnv :: TargetEnvironment
            } -> PU v x t

instance ( Ord v ) =>
        DecisionProblem (EndpointDT v t)
             EndpointDT (PU v x t)
         where
    options proxy PU{ diff, unit }
        = map (patch diff) $ options proxy unit
    decision proxy PU{ diff, unit, ports, systemEnv } d
        = PU
            { diff
            , unit=decision proxy unit $ patch (reverseDiff diff) d
            , ports
            , systemEnv
            }

instance ( VarValTime v x t ) => ProcessorUnit (PU v x t) v x t where
    tryBind fb PU{ diff, unit, ports, systemEnv }
        = case tryBind fb unit of
            Right unit' -> Right PU { diff, unit=unit', ports, systemEnv }
            Left err    -> Left err
    process PU{ diff, unit } = let
            p = process unit
        in p{ steps=map (patch diff) $ steps p }
    setTime t PU{ diff, unit, ports, systemEnv }
        = PU{ diff, unit=setTime t unit, ports, systemEnv }

instance ( Ord v ) => Patch (PU v x t) (I v, I v) where
    patch (I v, I v') pu@PU{ diff=diff@Diff{ diffI } } = pu{ diff=diff{ diffI=M.insert v v' diffI }}

instance ( Ord v ) => Patch (PU v x t) (O v, O v) where
    patch (O vs, O vs') pu@PU{ diff=diff@Diff{ diffO } }
        = pu{ diff=diff{ diffO=foldl (\s (v, v') -> M.insert v v' s) diffO $ zip (S.elems vs) (S.elems vs')  }}

instance ( Var v ) => Locks (PU v x t) v where
    locks PU{ unit } = locks unit

instance Simulatable (PU v x t) v x where
    simulateOn cntx PU{ unit } fb = simulateOn cntx unit fb

instance TargetSystemComponent (PU v x t) where
    moduleName name PU{ unit } = moduleName name unit
    hardware name PU{ unit } = hardware name unit
    software name PU{ unit } = software name unit
    hardwareInstance name pu = hardwareInstance name pu

instance IOTestBench (PU v x t) v x where
    testEnvironmentInitFlag tag PU{ unit } = testEnvironmentInitFlag tag unit

    testEnvironment tag PU{ unit, systemEnv, ports } _systemEnv _links cntxs
        = testEnvironment tag unit systemEnv ports cntxs
