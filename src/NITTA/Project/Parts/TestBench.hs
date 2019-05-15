{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Project.Parts.TestBench
Description : Generation a test bench for the target system.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Parts.TestBench
    ( TestBench(..)
    , Testable(..), IOTestBench(..), TestbenchReport(..)
    , testBenchTopModuleName
    , projectFiles
    ) where

import qualified Data.List                        as L
import qualified Data.String.Utils                as S
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import           NITTA.Intermediate.Types
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Parts.Utils
import           NITTA.Project.Types
import           System.Directory                 (createDirectoryIfMissing)
import           System.FilePath.Posix            (joinPath)


data TestBench = TestBench

instance ( Testable (m v x t) v x
        ) => ProjectPart TestBench (Project (m v x t) v x) where
    writePart TestBench prj@Project{ pPath } = do
        createDirectoryIfMissing True pPath
        writeImplementation pPath $ testBenchImplementation prj


-- |Type class for all testable parts of a target system.
class Testable m v x | m -> v x where
    testBenchImplementation :: Project m v x -> Implementation


-- |Processor units with input/output ports should be tested by generation
-- external input ports signals and checking output port signals.
class IOTestBench pu v x | pu -> v x where
    componentTestEnvironment :: String -> pu -> TargetEnvironment -> Ports pu -> Cntx v x -> String
    componentTestEnvironment _title _pu _env _ports _cntx = ""


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


-- |Generate list of project verilog files (including testbench).
projectFiles prj@Project{ pName, pUnit }
    = L.nub $ concatMap (addPath "") [ hardware pName pUnit, testBenchImplementation prj ]
    where
        addPath p (Aggregate (Just p') subInstances) = concatMap (addPath $ joinPath [p, p']) subInstances
        addPath p (Aggregate Nothing subInstances) = concatMap (addPath $ joinPath [p]) subInstances
        addPath p (Immediate fn _) = [ joinPath [ p, fn ] ]
        addPath _ (FromLibrary fn) = [ joinPath [ "lib", T.unpack $ L.last $ T.split (=='/') (T.pack fn) ] ]
        addPath _ Empty = []


-- |Get name of testbench top module.
testBenchTopModuleName prj = S.replace ".v" "" $ last $ projectFiles prj
