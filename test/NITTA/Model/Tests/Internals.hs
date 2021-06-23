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

import Data.Atomics.Counter (incrCounter, newCounter)
import NITTA.Synthesis
import System.IO.Unsafe (unsafePerformIO)
import GHC.Conc
import Control.Concurrent.STM.Map as SMap


externalTestCntr = unsafePerformIO $ newCounter 0
{-# NOINLINE externalTestCntr #-}

{-# NOINLINE externalTestNamesMap #-}
externalTestNamesMap = unsafePerformIO $ atomically SMap.empty

uniqTestPath :: FilePath -> IO FilePath
uniqTestPath filePath = do
    countEither <- atomically $ SMap.lookup filePath externalTestNamesMap
    case countEither of
        Nothing    -> atomically $ notExist filePath 
        Just count -> atomically $ exist filePath count 

    where
        notExist :: FilePath -> STM FilePath
        notExist f = do
            SMap.insert f (0 :: Int) externalTestNamesMap
            return f
        exist :: FilePath -> Int -> STM FilePath
        exist f count = do
            SMap.insert f (count + 1) externalTestNamesMap
            return (f <> "_" <> show (count + 1))

runTargetSynthesisWithUniqName t@TargetSynthesis{tName} = do
    name <- uniqTestPath tName
    runTargetSynthesis t{tName = name}