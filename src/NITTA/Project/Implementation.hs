{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Project.Implementation
Description : Types for target system implementation description.
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Implementation (
    UnitEnv (..),
    envInputPorts,
    envOutputPorts,
    envInOutPorts,
    Implementation (..),
    Parameter (..),
    TargetSystemComponent (..),
) where

import NITTA.Model.ProcessorUnits.Types

-- |Element of target system implementation
data Implementation
    = -- |Immediate implementation
      Immediate {impFileName, impText :: String}
    | -- |Fetch implementation from library
      FromLibrary {impFileName :: String}
    | -- |Aggregation of many implementation parts in separate paths
      Aggregate {impPath :: Maybe String, subComponents :: [Implementation]}
    | -- |Nothing
      Empty

-- |Type class for target components. Target -- a target system project or a testbench.
class TargetSystemComponent pu where
    -- |Name of the structural hardware module or Verilog module name (network or process unit)
    moduleName :: String -> pu -> String

    -- |Software and other specification which depends on application algorithm
    software :: String -> pu -> Implementation

    -- |Hardware which depends on microarchitecture description and requires synthesis.
    hardware :: String -> pu -> Implementation

    -- |Generate code for making an instance of the hardware module
    hardwareInstance :: String -> pu -> UnitEnv pu -> String

data Parameter
    = InlineParam String
    | IntParam Int
    deriving (Eq, Ord)

instance Show Parameter where
    show (IntParam i) = show i
    show (InlineParam s) = s

{- |Resolve uEnv element to verilog source code. E.g. `dataIn` into
`data_bus`, `dataOut` into `accum_data_out`.
-}
data UnitEnv m = UnitEnv
    { -- |clock signal
      sigClk :: String
    , -- |reset signal
      sigRst :: String
    , -- |posedge on computation cycle begin
      sigCycleBegin :: String
    , -- |positive on computation cycle
      sigInCycle :: String
    , -- |posedge on computation cycle end
      sigCycleEnd :: String
    , ctrlPorts :: Maybe (Ports m)
    , ioPorts :: Maybe (IOPorts m)
    , valueIn, valueOut :: Maybe (String, String)
    }

envInputPorts UnitEnv{ioPorts} = concatMap inputPorts ioPorts
envOutputPorts UnitEnv{ioPorts} = concatMap outputPorts ioPorts
envInOutPorts UnitEnv{ioPorts} = concatMap inoutPorts ioPorts
