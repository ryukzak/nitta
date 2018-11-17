{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-cse #-}

{-|
Module      : Test
Description : Utils for testing.
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Utils.Test
    ( test
    , testLua
    , demo
    , testWithInput
    ) where

import           Control.Monad                 (unless)
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Except
import           Data.Default                  as D
import qualified Data.Map                      as M
import           NITTA.Compiler
import           NITTA.DataFlow
import           NITTA.Frontend
import           NITTA.Project
import           NITTA.Types
import           NITTA.Types.Project
import           System.FilePath               (joinPath)
import           Text.InterpolatedString.Perl6 (qc)


test name = testWithInput name []
testLua name ma = testWithInput name [] ma . lua2functions

testWithInput :: _ -> _ -> _ -> _ -> IO (Either String String)
testWithInput name cntx ma alg = runExceptT $ do
    let model@Frame{ processor } = simpleSynthesis $ mkModelWithOneNetwork ma alg
    let isComplete = isSchedulingComplete model
    unless isComplete $ throwE [qc|> test { name } not isSchedulingComplete|]

    let prj = Project
            { projectName=name
            , libraryPath="../.."
            , projectPath=joinPath ["hdl", "gen", name]
            , processorModel=processor
            , testCntx=Just D.def{ cntxInputs=M.fromList cntx }
            , targetPlatforms=[ Makefile, DE0Nano ]
            }
    TestBenchReport{ tbStatus } <- lift $ writeAndRunTestBench prj
    unless tbStatus $ throwE [qc|> test { name } - Fail|]

    return [qc|> test { name } - Success|]

demo prj@Project{ projectPath, processorModel } = do
    let prj' = prj{ processorModel=processor $ simpleSynthesis processorModel }
    writeProject prj'
    putStrLn $ "Demo project in " ++ projectPath
