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
import Data.Map as M
import GHC.Conc as Conc
import NITTA.Synthesis
import System.IO.Unsafe (unsafePerformIO)

externalTestCntr = unsafePerformIO $ newCounter 0
{-# NOINLINE externalTestCntr #-}

{-# NOINLINE externalTestNamesMap #-}
externalTestNamesMap = unsafePerformIO $ newTVarIO M.empty

uniqTestPath :: FilePath -> IO FilePath
uniqTestPath filePath = do
    m <- readTVarIO externalTestNamesMap
    case M.lookup filePath m of
        Nothing -> notExist filePath m
        Just count -> exist filePath (count + 1) m
    where
        notExist :: FilePath -> Map FilePath Int -> IO FilePath
        notExist f m = do
            _ <- return $ M.insert f (0 :: Int) m
            return f
        exist :: FilePath -> Int -> Map FilePath Int -> IO FilePath
        exist f newCount m = do
            _ <- return $ M.insert f newCount m
            return (f <> "_" <> show newCount)

runTargetSynthesisWithUniqName t@TargetSynthesis{tName} = do
    name <- uniqTestPath tName
    runTargetSynthesis t{tName = name}
