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
    runTargetSynthesisWithUniqName,
    uniqTestPath,
) where

import Control.Concurrent.STM.TVar
import qualified Data.Map as M
import GHC.Conc
import NITTA.Synthesis
import System.IO.Unsafe (unsafePerformIO)

externalTestNamesMap = unsafePerformIO $ newTVarIO M.empty
{-# NOINLINE externalTestNamesMap #-}

uniqTestPath :: FilePath -> IO FilePath
uniqTestPath name =
    atomically $ do
        m <- readTVar externalTestNamesMap
        case M.lookup name m of
            Nothing -> do
                writeTVar externalTestNamesMap $ M.insert name (0 :: Int) m
                return name
            Just count -> do
                writeTVar externalTestNamesMap $ M.adjust (+ 1) name m
                return (name <> "_" <> show (count + 1))

runTargetSynthesisWithUniqName t@TargetSynthesis{tName} = do
    name <- uniqTestPath tName
    runTargetSynthesis t{tName = name}
