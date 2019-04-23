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
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Utils.Test
    ( test
    , testLua
    , testWithInput
    , testLuaWithInput
    , mkModelWithOneNetwork
    , Test(..), runTest
    ) where

import           Control.Monad                 (unless, when)
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Except
import           Data.Default                  as D
import qualified Data.Map                      as M
import           NITTA.DataFlow
import           NITTA.Frontend
import           NITTA.Project
import           NITTA.SynthesisMethod
import           NITTA.Types
import           NITTA.BusNetwork (BusNetwork)
import           NITTA.Types.Project
import           NITTA.Types.Synthesis         (Node (..), mkNodeIO)
import           System.FilePath               (joinPath)
import           Text.InterpolatedString.Perl6 (qc)


data Test title v x t = Test
    { testName :: String
    , microarchitecture :: BusNetwork title v x t
    , alg :: [F v x]
    , receiveValue :: [(v, [x])]
    , verbose :: Bool
    , platforms :: [TargetPlatform]
    }

instance Default (Test title v x t) where
    def = Test
        { testName=undefined
        , microarchitecture=undefined
        , alg=undefined
        , receiveValue=def
        , verbose=False 
        , platforms=[ Makefile, DE0Nano ]
        }

runTest Test{ testName, microarchitecture, alg, receiveValue, verbose } = runExceptT $ do
    when verbose $ lift $ putStrLn "synthesis process"
    synthesisResult <- lift $ mkNodeIO (mkModelWithOneNetwork microarchitecture alg) >>= simpleSynthesisIO
    let isComplete = isSchedulingComplete $ nModel synthesisResult
    when (verbose && isComplete) $ lift $ putStrLn "synthesis process - ok"
    when (verbose && not isComplete) $ lift $ putStrLn "synthesis process - fail"
    when (not isComplete) $ throwE ([qc|> test { testName } synthesis fail|] :: String)

    let path = joinPath ["hdl", "gen", testName]
    let prj = Project
            { projectName=testName
            , libraryPath="../.."
            , projectPath=path
            , processorModel=processor $ nModel synthesisResult
            , testCntx=Just D.def{ cntxInputs=M.fromList receiveValue }
            , targetPlatforms=[ Makefile, DE0Nano ]
            }
    when verbose $ lift $ putStrLn $ "write target project (" ++ path ++ ")"
    lift $ writeProject prj
    when verbose $ lift $ putStrLn $ "write target project (" ++ path ++ ") - ok"

    when verbose $ lift $ putStrLn "run testbench"
    report@TestBenchReport{ tbStatus } <- lift $ runTestBench prj
    when verbose $ lift $ putStrLn $ case tbStatus of
        True -> "run testbench - ok"
        False -> "run testbench - fail"
    return report

test name = testWithInput name []
testLua name ma = testWithInput name [] ma . lua2functions
testLuaWithInput name is ma = testWithInput name is ma . lua2functions

testWithInput name cntx ma alg = runExceptT $ do
    node <- lift $ simpleSynthesisIO =<< mkNodeIO (mkModelWithOneNetwork ma alg)
    unless (isSchedulingComplete $ nModel node)
        $ throwE [qc|> test { name } not isSchedulingComplete|]

    let prj = Project
            { projectName=name
            , libraryPath="../.."
            , projectPath=joinPath ["hdl", "gen", name]
            , processorModel=processor $ nModel node
            , testCntx=Just D.def{ cntxInputs=M.fromList cntx }
            , targetPlatforms=[ Makefile, DE0Nano ]
            }
    TestBenchReport{ tbStatus } <- lift $ writeAndRunTestBench prj
    unless tbStatus $ throwE ([qc|> test { name } - Fail|] :: String)

    return ([qc|> test { name } - Success|] :: String)


-- |Make a model of NITTA process with one network and a specific algorithm. All functions are
-- already bound to the network.
mkModelWithOneNetwork arch alg = Frame
    { processor=foldl (flip bind) arch alg
    , dfg=DFG $ map DFGNode alg
    }
