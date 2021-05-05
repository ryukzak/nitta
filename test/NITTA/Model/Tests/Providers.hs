{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -fno-warn-redundant-constraints #-}
{-# OPTIONS -fno-warn-partial-type-signatures #-}

{- |
Module      : NITTA.Model.Tests.Providers
Description : Utils for processor unit testing
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Tests.Providers (
    nittaCoSimTestCase,
    algTestCase,
    module NITTA.Intermediate.Functions,
    module NITTA.Model.Tests.Microarchitecture,
) where

import Control.Monad (void)
import Data.CallStack
import Data.Default
import qualified Data.String.Utils as S
import NITTA.Intermediate.DataFlow
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Tests.Internals
import NITTA.Model.Tests.Microarchitecture
import NITTA.Project
import NITTA.Synthesis
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

-- |Execute co-simulation test for the specific microarchitecture and algorithm
nittaCoSimTestCase ::
    ( HasCallStack
    , Val x
    , Integral x
    ) =>
    String ->
    BusNetwork String String x Int ->
    [F String x] ->
    TestTree
nittaCoSimTestCase n tMicroArch alg =
    testCase n $ do
        reportE <-
            runTargetSynthesisWithUniqName
                def
                    { tName = S.replace " " "_" n
                    , tMicroArch
                    , tDFG = fsToDataFlowGraph alg
                    }
        case reportE of
            Right report@TestbenchReport{tbStatus} ->
                assertBool ("report with bad status:\n" <> show report) tbStatus
            Left err -> assertFailure $ "can't get report: " <> err

algTestCase n tMicroArch alg =
    testCase n $
        void $
            runTargetSynthesisWithUniqName
                (def :: TargetSynthesis _ _ _ Int)
                    { tName = n
                    , tMicroArch
                    , tDFG = fsToDataFlowGraph alg
                    }
