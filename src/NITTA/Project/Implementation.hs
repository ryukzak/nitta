{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Project.Implementation
Description : Types for target system implementation description.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Implementation (
    TargetEnvironment (..),
    Implementation (..),
    UnitEnv (..),
    Parameter (..),
    TargetSystemComponent (..),
) where

import qualified Data.Text as T
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
    moduleName n pu = T.unpack $ moduleNameT (T.pack n) pu

    moduleNameT :: T.Text -> pu -> T.Text
    moduleNameT n pu = T.pack $ moduleName (T.unpack n) pu

    -- |Software and other specification which depends on application algorithm
    software :: String -> pu -> Implementation

    -- |Hardware which depends on microarchitecture description and requires synthesis.
    hardware :: String -> pu -> Implementation

    -- |Generate code for making an instance of the hardware module
    hardwareInstance :: String -> pu -> TargetEnvironment -> Ports pu -> IOPorts pu -> String
    hardwareInstance n pu env p iop = T.unpack $ hardwareInstanceT (T.pack n) pu env p iop

    hardwareInstanceT :: T.Text -> pu -> TargetEnvironment -> Ports pu -> IOPorts pu -> T.Text
    hardwareInstanceT n pu env p iop = T.pack $ hardwareInstance (T.unpack n) pu env p iop

data Parameter
    = InlineParam String
    | IntParam Int
    deriving (Eq, Ord)

instance Show Parameter where
    show (IntParam i) = show i
    show (InlineParam s) = s

-- |Target mUnit environment, including IO ports, clk, rst and cycle signals.
data TargetEnvironment = TargetEnvironment
    { -- |clock
      signalClk :: String
    , -- |reset
      signalRst :: String
    , -- |posedge on computation cycle begin
      signalCycleBegin :: String
    , -- |positive on computation cycle
      signalInCycle :: String
    , -- |posedge on computation cycle end
      signalCycleEnd :: String
    , inputPort :: InputPortTag -> String
    , outputPort :: OutputPortTag -> String
    , inoutPort :: InoutPortTag -> String
    , unitEnv :: UnitEnv -- unit specific environment
    }

data UnitEnv
    = -- |Environment of process unit.
      ProcessUnitEnv
        { -- |bus name
          dataIn, attrIn :: String
        , -- |bus name
          dataOut, attrOut :: String
        }
    | -- |Environment of network.
      NetworkEnv
