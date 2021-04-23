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

-- TODO: replace by function like: uniqTestPath :: FilePath -> IO FilePath

-- |Dirty hack to avoid collision with parallel QuickCheck.
externalTestCntr = unsafePerformIO $ newCounter 0
{-# NOINLINE externalTestCntr #-}

runTargetSynthesisWithUniqName t@TargetSynthesis{tName} = do
    i <- incrCounter 1 externalTestCntr
    runTargetSynthesis t{tName = tName <> "_" <> show i}
