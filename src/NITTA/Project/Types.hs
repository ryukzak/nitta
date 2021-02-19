{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

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
    writeImplementation,
    copyLibraryFiles,
    UnitEnv (..),
    envInputPorts,
    envOutputPorts,
    envInOutPorts,
) where

import Data.Default
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import NITTA.Intermediate.Types
import NITTA.Intermediate.Value ()
import NITTA.Model.ProcessorUnits.Types
import System.Directory
import System.FilePath.Posix (joinPath, takeDirectory, (</>))

{- |Target project for different purpose (testing, target system, etc). Should
be writable to disk.
-}

-- FIXME: collision between target project name and output directory. Maybe
-- pName or pTargetProjectPath should be maybe? Or both?
data Project m v x = Project
    { -- |target project name
      pName :: T.Text
    , -- |IP-core library directory
      pLibPath :: FilePath
    , -- |output directory for target project
      pTargetProjectPath :: FilePath
    , -- |output directory for NITTA processor inside target project
      pNittaPath :: FilePath
    , -- |'mUnit' model (a mUnit unit for testbench or network for complete NITTA mUnit)
      pUnit :: m
    , pUnitEnv :: UnitEnv m
    , -- |testbench context with input values
      pTestCntx :: Cntx v x
    , -- |Target platform templates
      pTemplates :: [FilePath]
    }

defProjectTemplates :: [FilePath]
defProjectTemplates =
    [ "templates/Icarus"
    , "templates/DE0-Nano"
    ]

instance (Default x) => DefaultX (Project m v x) x

-- |Type class for target components. Target -- a target system project or a testbench.
class TargetSystemComponent pu where
    -- |Name of the structural hardware module or Verilog module name (network or process unit)
    moduleName :: T.Text -> pu -> T.Text

    -- |Software and other specification which depends on application algorithm
    software :: T.Text -> pu -> Implementation

    -- |Hardware which depends on microarchitecture description and requires synthesis.
    hardware :: T.Text -> pu -> Implementation

    -- |Generate code for making an instance of the hardware module
    hardwareInstance :: T.Text -> pu -> UnitEnv pu -> T.Text

-- |Element of target system implementation
data Implementation
    = -- |Immediate implementation
      Immediate {impFileName :: T.Text, impText :: T.Text}
    | -- |Fetch implementation from library
      FromLibrary {impFileName :: T.Text}
    | -- |Aggregation of many implementation parts in separate paths
      Aggregate {impPath :: Maybe T.Text, subComponents :: [Implementation]}
    | -- |Nothing
      Empty

{- |Write 'Implementation' to the file system.

The $PATH$ placeholder is used for correct addressing between nested files. For
example, the PATH contains two files f1 and f2, and f1 imports f2 into itself.
To do this, you often need to specify its address relative to the working
directory, which is done by inserting this address in place of the $PATH$
placeholder.
-}
writeImplementation prjPath nittaPath = writeImpl nittaPath
    where
        writeImpl p (Immediate fn src) =
            T.writeFile (joinPath [prjPath, p, T.unpack fn]) $ T.replace "$PATH$" (T.pack p) src
        writeImpl p (Aggregate p' subInstances) = do
            let path = joinPath $ maybe [p] (\x -> [p, x]) $ fmap T.unpack p'
            createDirectoryIfMissing True $ joinPath [prjPath, path]
            mapM_ (writeImpl path) subInstances
        writeImpl _ (FromLibrary _) = return ()
        writeImpl _ Empty = return ()

-- |Copy library files to target path.
copyLibraryFiles prj = mapM_ (copyLibraryFile prj) $ libraryFiles prj
    where
        copyLibraryFile Project{pTargetProjectPath, pNittaPath, pLibPath} file = do
            let fullNittaPath = pTargetProjectPath </> pNittaPath
            source <- makeAbsolute $ joinPath [pLibPath, T.unpack file]
            target <- makeAbsolute $ joinPath [fullNittaPath, "lib", T.unpack file]
            createDirectoryIfMissing True $ takeDirectory target
            copyFile source target

        libraryFiles Project{pName, pUnit} =
            L.nub $ concatMap (args "") [hardware pName pUnit]
            where
                args p (Aggregate (Just p') subInstances) = concatMap (args $ joinPath [p, T.unpack p']) subInstances
                args p (Aggregate Nothing subInstances) = concatMap (args p) subInstances
                args _ (FromLibrary fn) = [fn]
                args _ _ = []

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

envInputPorts UnitEnv{ioPorts} = concatMap inputPorts ioPorts
envOutputPorts UnitEnv{ioPorts} = concatMap outputPorts ioPorts
envInOutPorts UnitEnv{ioPorts} = concatMap inoutPorts ioPorts
