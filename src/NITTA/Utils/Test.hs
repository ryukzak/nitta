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
module NITTA.Utils.Test where

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
import           System.FilePath               (joinPath)
import           Text.InterpolatedString.Perl6 (qc)

test name ma alg = testWithInput name [] ma alg
testLua name ma vt lua = testWithInput name [] ma $ lua2functions vt lua

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
            }
    TestBenchReport{ tbStatus } <- lift $ writeAndRunTestBench prj
    unless tbStatus $ throwE [qc|> test { name } - Fail|]

    return [qc|> test { name } - Success|]
