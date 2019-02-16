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
    ) where

import           Control.Monad                 (unless)
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Except
import           Data.Default                  as D
import qualified Data.Map                      as M
import           NITTA.DataFlow
import           NITTA.Frontend
import           NITTA.Project
import           NITTA.SynthesisMethod
import           NITTA.Types
import           NITTA.Types.Project
import           NITTA.Types.Synthesis         (Node (..), mkNodeIO)
import           System.FilePath               (joinPath)
import           Text.InterpolatedString.Perl6 (qc)


test name = testWithInput name []
testLua name ma = testWithInput name [] ma . lua2functions
testLuaWithInput name is ma = testWithInput name is ma . lua2functions

testWithInput name cntx ma alg = runExceptT $ do
    node <- lift $ simpleSynthesisIO =<< mkNodeIO mempty (mkModelWithOneNetwork ma alg)
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
