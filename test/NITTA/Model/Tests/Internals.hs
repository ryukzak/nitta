{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      : NITTA.Model.Tests.Internals
Description : Internals utils for model tests
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Tests.Internals (
    externalTestCntr,
    incrCounter,
    runTargetSynthesisWithUniqName,
) where

import Control.Concurrent.STM.TVar
import Data.Atomics.Counter (incrCounter, newCounter)
import Data.Map as M
import GHC.Conc
import NITTA.Synthesis
import System.IO.Unsafe (unsafePerformIO)

externalTestCntr = unsafePerformIO $ newCounter 0
{-# NOINLINE externalTestCntr #-}

externalTestNamesMap = unsafePerformIO $ newTVarIO M.empty
{-# NOINLINE externalTestNamesMap #-}

uniqTestPath :: FilePath -> IO FilePath
uniqTestPath filePath =
    atomically $ do
        m <- readTVar externalTestNamesMap
        case M.lookup filePath m of
            Nothing -> do
                writeTVar externalTestNamesMap $ M.insert filePath (0 :: Int) m
                return filePath
            Just count -> do
                writeTVar externalTestNamesMap $ M.insert filePath ((count + 1) :: Int) m
                return (filePath <> "_" <> show (count + 1))

runTargetSynthesisWithUniqName t@TargetSynthesis{tName} = do
    name <- uniqTestPath tName
    runTargetSynthesis t{tName = name}
