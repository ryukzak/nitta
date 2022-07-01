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
import Data.Map qualified as M
import GHC.Conc
import NITTA.Synthesis
import System.IO.Unsafe (unsafePerformIO)

globalNameRegistry = unsafePerformIO $ newTVarIO M.empty
{-# NOINLINE globalNameRegistry #-}

uniqTestPath :: FilePath -> IO FilePath
uniqTestPath name =
    atomically $ do
        nameRegistry <- readTVar globalNameRegistry
        case M.lookup name nameRegistry of
            Nothing -> do
                writeTVar globalNameRegistry $ M.insert name (0 :: Int) nameRegistry
                return name
            Just count -> do
                writeTVar globalNameRegistry $ M.adjust (+ 1) name nameRegistry
                return (name <> "_" <> show (count + 1))

runTargetSynthesisWithUniqName t@TargetSynthesis{tName} = do
    name <- uniqTestPath tName
    runTargetSynthesis t{tName = name}
