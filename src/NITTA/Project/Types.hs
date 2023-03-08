{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Project.Types
Description : Types for a target project description and generation
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Types (
    Project (..),
    defProjectTemplates,
    TargetSystemComponent (..),
    Implementation (..),
    UnitEnv (..),
    envInputPorts,
    envOutputPorts,
    envInOutPorts,
) where

import Data.Default
import Data.Set qualified as S
import Data.Text qualified as T
import NITTA.Intermediate.Types
import NITTA.Intermediate.Value ()
import NITTA.Model.ProcessorUnits.Types
import NITTA.Utils

{- | Target project for different purpose (testing, target system, etc). Should
be writable to disk.
-}

-- FIXME: collision between target project name and output directory. Maybe
-- pName or pTargetProjectPath should be maybe? Or both?
data Project m v x = Project
    { pName :: T.Text
    -- ^ target project name
    , pLibPath :: FilePath
    -- ^ IP-core library path
    , pTargetProjectPath :: FilePath
    -- ^ output path for target project
    , pAbsTargetProjectPath :: FilePath
    -- ^ absolute output path for target project
    , pInProjectNittaPath :: FilePath
    -- ^ relative to the project path output path for NITTA processor inside target project
    , pAbsNittaPath :: FilePath
    -- ^ absolute output path for NITTA processor inside target project
    , pUnit :: m
    -- ^ 'mUnit' model (a mUnit unit for testbench or network for complete NITTA mUnit)
    , pUnitEnv :: UnitEnv m
    , pTestCntx :: Cntx v x
    -- ^ testbench context with input values
    , pTemplates :: [FilePath]
    -- ^ Target platform templates
    }

defProjectTemplates :: [FilePath]
defProjectTemplates =
    [ "templates/Icarus"
    , "templates/DE0-Nano"
    ]

instance (Default x) => DefaultX (Project m v x) x

-- | Type class for target components. Target -- a target system project or a testbench.
class TargetSystemComponent pu where
    -- | Name of the structural hardware module or Verilog module name (network or process unit)
    moduleName :: T.Text -> pu -> T.Text

    -- | Software and other specification which depends on application algorithm
    software :: T.Text -> pu -> Implementation

    -- | Hardware which depends on microarchitecture description and requires synthesis.
    hardware :: T.Text -> pu -> Implementation

    -- | Generate code for making an instance of the hardware module
    hardwareInstance :: T.Text -> pu -> UnitEnv pu -> Verilog

-- | Element of target system implementation
data Implementation
    = -- | Immediate implementation in the from of Ginger template (@nitta.paths.nest@ + 'projectContext')
      Immediate {impFileName :: FilePath, impText :: T.Text}
    | -- | Fetch implementation from library
      FromLibrary {impFileName :: FilePath}
    | -- | Aggregation of many implementation parts in separate paths
      Aggregate {impPath :: Maybe FilePath, subComponents :: [Implementation]}
    | -- | Nothing
      Empty

{- | Resolve uEnv element to verilog source code. E.g. `dataIn` into
`data_bus`, `dataOut` into `accum_data_out`.
-}
data UnitEnv m = UnitEnv
    { sigClk :: T.Text
    -- ^ clock signal
    , sigRst :: T.Text
    -- ^ reset signal
    , sigCycleBegin :: T.Text
    -- ^ posedge on computation cycle begin
    , sigInCycle :: T.Text
    -- ^ positive on computation cycle
    , sigCycleEnd :: T.Text
    -- ^ posedge on computation cycle end
    , ctrlPorts :: Maybe (Ports m)
    , ioPorts :: Maybe (IOPorts m)
    , valueIn, valueOut :: Maybe (T.Text, T.Text)
    }

deriving instance (Show (Ports m), Show (IOPorts m)) => Show (UnitEnv m)

instance Default (UnitEnv m) where
    def =
        UnitEnv
            { sigClk = "clk"
            , sigRst = "rst"
            , sigCycleBegin = "flag_cycle_begin"
            , sigInCycle = "flag_in_cycle"
            , sigCycleEnd = "flag_cycle_end"
            , ctrlPorts = Nothing
            , ioPorts = Nothing
            , valueIn = Nothing
            , valueOut = Nothing
            }

envInputPorts UnitEnv{ioPorts = Just ports} = inputPorts ports
envInputPorts UnitEnv{ioPorts = Nothing} = S.empty

envOutputPorts UnitEnv{ioPorts = Just ports} = outputPorts ports
envOutputPorts UnitEnv{ioPorts = Nothing} = S.empty

envInOutPorts UnitEnv{ioPorts = Just ports} = inoutPorts ports
envInOutPorts UnitEnv{ioPorts = Nothing} = S.empty
