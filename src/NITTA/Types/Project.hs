{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Types.Project
Description : Types for describe target system project and test benchs
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Types.Project
    ( -- *Project
      Project(..)
    , writeProjectForTest
    , writeWholeProject
      -- *Project part types
    , ProjectPart(..)
    , TestBench(..)
    , TargetSystem(..)
    , QuartusProject(..)
    , IcarusMakefile(..)
      -- *Testbench
    , Testable(..), TestbenchReport(..)
    , testBenchTopModule
      -- *Utils
    , projectFiles
    ) where

import qualified Data.List             as L
import qualified Data.String.Utils     as S
import qualified Data.Text             as T
import           GHC.Generics          (Generic)
import           NITTA.Types.Base
import           NITTA.Types.Network
import           System.FilePath.Posix (joinPath)


-- |Target project for different purpose (testing, target system, etc). Should
-- be writable to disk.

-- FIXME: collision between target project name and output directory. Maybe
-- pName or pPath should be maybe? Or both?
data Project m v x
    = Project
        { pName     :: String -- ^target project name
        , pLibPath  :: String -- ^'mUnit' unit library directory
        , pPath     :: String -- ^output directory
        , pUnit     :: m      -- ^'mUnit' model (a mUnit unit for testbench or network for complete NITTA mUnit)
        , pTestCntx :: Maybe (Cntx v x) -- ^testbench context with input values
        -- FIXME: Should be a real Either Cntx, with functional simulation data.
        } deriving ( Show )



-- |Target system project contain multiple parts for different applications. Usually, for any
-- specific purpose you need several of them.
class ProjectPart pt m where
    writePart :: pt -> m -> IO ()


-- |Write project with @TargetSystem@, @TestBench@ and @IcarusMakefile@ parts.
writeProjectForTest prj = do
    writePart TargetSystem prj
    writePart TestBench prj
    writePart IcarusMakefile prj

-- |Write project with all available parts.
writeWholeProject prj = do
    writePart TargetSystem prj
    writePart TestBench prj
    writePart QuartusProject prj
    writePart IcarusMakefile prj



-- |Target system as a set of Verilog files and software dumps.
data TargetSystem = TargetSystem

-- |Quartus project files.
data QuartusProject = QuartusProject

-- |Test bench for a target system or a mUnit unit.
data TestBench = TestBench

-- |Makefile for running testbench by Icarus Verilog.
data IcarusMakefile = IcarusMakefile



-- |Type class for all testable parts of a target system.
class Testable m v x | m -> v x where
    testBenchImplementation :: Project m v x -> Implementation

data TestbenchReport
    = TestbenchReport
        { tbStatus         :: Bool
        , tbPath           :: String
        , tbFiles          :: [String]
        , tbFunctions      :: [String]
        , tbCompilerDump   :: String
        , tbSimulationDump :: String
        }
    deriving ( Generic, Show )


-- |Generate list of project files (including testbench).
projectFiles prj@Project{ pName, pUnit }
    = L.nub $ concatMap (addPath "") [ hardware pName pUnit, testBenchImplementation prj ]
    where
        addPath p (Aggregate (Just p') subInstances) = concatMap (addPath $ joinPath [p, p']) subInstances
        addPath p (Aggregate Nothing subInstances) = concatMap (addPath $ joinPath [p]) subInstances
        addPath p (Immediate fn _) = [ joinPath [ p, fn ] ]
        addPath _ (FromLibrary fn) = [ joinPath [ "lib", T.unpack $ L.last $ T.split (=='/') (T.pack fn) ] ]
        addPath _ Empty = []

-- |Get name of testbench top module.
testBenchTopModule prj = S.replace ".v" "" $ last $ projectFiles prj
