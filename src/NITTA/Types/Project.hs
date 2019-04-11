{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Types.Project
Description : Types for describe target system
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Types.Project where

import qualified Data.List             as L
import qualified Data.String.Utils     as S
import qualified Data.Text             as T
import           GHC.Generics          (Generic)
import           NITTA.Types.Base
import           NITTA.Types.Network
import           System.FilePath.Posix (joinPath)



-- |Проект вычислителя NITTA.
data Project pu v x
    = Project
        { projectName     :: String -- ^Наименование проекта.
        , libraryPath     :: String -- ^Директория библиотеки с вычислительными блоками.
        , projectPath     :: String -- ^Директория проекта, куда будут размещены его файлы.
        , processorModel  :: pu     -- ^Модель вычислительного блока.
        , testCntx        :: Maybe (Cntx v x) -- ^Контекст для генерации test bench.
        , targetPlatforms :: [TargetPlatform]
        } deriving ( Show )

-- |Target platform specific files.
data TargetPlatform
    = Makefile -- ^ Makefile
    | DE0Nano -- ^ Modelsim and Quartus
    deriving ( Show )



-- |Данный класс позволяет для реализующих его вычислительных блоков сгенировать test bench.
class TestBench pu v x | pu -> v x where
    testBenchDescription :: Project pu v x -> Implementation

data TestBenchSetup pu
    = TestBenchSetup
        { tbcSignals       :: [String]
        , tbcPorts         :: PUPorts pu
        , tbcSignalConnect :: Signal -> String
        , tbcCtrl          :: Microcode pu -> String
        , tbDataBusWidth   :: Int
        }

data TestBenchReport
    = TestBenchReport
        { tbStatus         :: Bool
        , tbPath           :: String
        , tbFiles          :: [String]
        , tbFunctions      :: [String]
        , tbCompilerDump   :: String
        , tbSimulationDump :: String
        }
    deriving ( Generic )



-- * Utils

projectFiles prj@Project{ projectName, processorModel }
    = let
        files = L.nub $ concatMap (args "") [ hardware projectName processorModel, testBenchDescription prj ]
        tb = S.replace ".v" "" $ last files
    in (tb, files)
    where
        args p (Aggregate (Just p') subInstances) = concatMap (args $ joinPath [p, p']) subInstances
        args p (Aggregate Nothing subInstances) = concatMap (args $ joinPath [p]) subInstances
        args p (Immidiate fn _) = [ joinPath [ p, fn ] ]
        args _ (FromLibrary fn) = [ joinPath [ "lib", T.unpack $ L.last $ T.split (=='/') (T.pack fn) ] ]
        args _ Empty = []

