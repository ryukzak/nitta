{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-cse #-}

{-|
Module      : NITTA.Utils.Test
Description : Testing utility
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Utils.Test
    ( mkModelWithOneNetwork
    , Test(..), runTest
    ) where

import           Control.Monad                 (when)
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Except
import           Data.Default                  as D
import qualified Data.Map                      as M
import           Data.Text                     (Text)
import           NITTA.BusNetwork              (BusNetwork, Title)
import           NITTA.DataFlow
import           NITTA.Frontend
import           NITTA.Project
import           NITTA.SynthesisMethod
import           NITTA.Types
import           NITTA.Types.Project
import           NITTA.Types.Synthesis         (Node (..), mkNodeIO)
import           System.FilePath               (joinPath)
import           Text.InterpolatedString.Perl6 (qc)


data Test title v x t = Test
    { testProjectName   :: String
    , microarchitecture :: BusNetwork title v x t
    , sourceCode        :: Maybe Text
    , alg               :: [F v x]
    , receiveValues     :: [(v, [x])]
    , verbose           :: Bool
    , synthesisMethod   :: Node title v x t -> IO (Node title v x t)
    , platforms         :: [TargetPlatform]
    }

instance ( Title title, Var v, Semigroup v, Val x, Time t ) => Default (Test title v x t) where
    def = Test
        { testProjectName=undefined
        , microarchitecture=undefined
        , sourceCode=Nothing
        , alg=undefined
        , receiveValues=def
        , verbose=False
        , synthesisMethod=simpleSynthesisIO
        , platforms=[ Makefile, DE0Nano ]
        }

runTest Test{ testProjectName, microarchitecture, sourceCode, alg, receiveValues, synthesisMethod, verbose } = runExceptT $ do
    alg' <- case sourceCode of
        Just src -> do
            when verbose $ lift $ putStrLn "lua transpiler"
            let tmp = lua2functions src
            when verbose $ lift $ putStrLn "lua transpiler - ok"
            return tmp
        Nothing -> return alg

    when verbose $ lift $ putStrLn "synthesis process"
    synthesisResult <- lift $ mkNodeIO (mkModelWithOneNetwork microarchitecture alg') >>= synthesisMethod
    let isComplete = isSchedulingComplete $ nModel synthesisResult
    when (verbose && isComplete) $ lift $ putStrLn "synthesis process - ok"
    when (verbose && not isComplete) $ lift $ putStrLn "synthesis process - fail"
    when (not isComplete) $ throwE ([qc|> test { testProjectName } synthesis fail|] :: String)

    let path = joinPath ["hdl", "gen", testProjectName]
    let prj = Project
            { projectName=testProjectName
            , libraryPath="../.."
            , projectPath=path
            , processorModel=processor $ nModel synthesisResult
            , testCntx=Just D.def{ cntxInputs=M.fromList receiveValues }
            , targetPlatforms=[ Makefile, DE0Nano ]
            }
    when verbose $ lift $ putStrLn $ "write target project (" ++ path ++ ")"
    lift $ writeProject prj
    when verbose $ lift $ putStrLn $ "write target project (" ++ path ++ ") - ok"

    when verbose $ lift $ putStrLn "run testbench"
    report@TestBenchReport{ tbStatus, tbCompilerDump, tbSimulationDump } <- lift $ runTestBench prj
    when verbose $ case tbStatus of
        True  -> lift $ putStrLn "run testbench - ok"
        False -> lift $ do
            putStrLn "run testbench - fail"
            putStrLn "-----------------------------------------------------------"
            putStrLn "testbench compiler dump:"
            putStrLn tbCompilerDump
            putStrLn "-----------------------------------------------------------"
            putStrLn "testbench simulation dump:"
            putStrLn tbSimulationDump
    return report



-- |Make a model of NITTA process with one network and a specific algorithm. All functions are
-- already bound to the network.
mkModelWithOneNetwork arch alg = Frame
    { processor=foldl (flip bind) arch alg
    , dfg=DFG $ map DFGNode alg
    }
