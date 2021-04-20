{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -fno-warn-redundant-constraints #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Tests.Utils
Description : Utils for processor unit testing
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Tests.TestCaseTemplates (
    puCoSimTestCase,
    nittaCoSimTestCase,
    finitePUSynthesisProp,
    puCoSimProp,
    module NITTA.Model.ProcessorUnits.Tests.Utils,
) where

import Control.Monad
import Data.Atomics.Counter (incrCounter)
import Data.CallStack
import Data.Data
import Data.Default
import qualified Data.String.Utils as S
import qualified Data.Text as T
import NITTA.Intermediate.DataFlow
import NITTA.Intermediate.Functions ()
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Networks.Types
import NITTA.Model.Problems hiding (Bind, BreakLoop)
import NITTA.Model.ProcessorUnits.Tests.DSL
import NITTA.Model.ProcessorUnits.Tests.Utils
import NITTA.Model.TargetSystem ()
import NITTA.Model.Tests.Microarchitecture
import NITTA.Project
import qualified NITTA.Project as P
import NITTA.Synthesis
import NITTA.Utils
import System.Directory
import System.FilePath.Posix (joinPath)
import Test.QuickCheck.Monadic
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import Test.Tasty.QuickCheck (testProperty)

-- *Test cases

-- |Execute co-simulation test for the specific process unit
puCoSimTestCase ::
    ( HasCallStack
    , PUClasses (pu String x Int) String x Int
    , WithFunctions (pu String x Int) (F String x)
    , P.Testable (pu String x Int) String x
    , DefaultX (pu String x Int) x
    , Typeable pu
    ) =>
    String ->
    pu String x Int ->
    [(String, x)] ->
    [F String x] ->
    TestTree
puCoSimTestCase name u cntxCycle alg =
    puUnitTestCase name u $ do
        assignsNaive alg cntxCycle
        assertNaiveCoSimulation

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
        report <-
            runTargetSynthesisWithUniqName
                def
                    { tName = S.replace " " "_" n
                    , tMicroArch
                    , tDFG = fsToDataFlowGraph alg
                    }
        case report of
            Right report' -> assertBool "report with bad status" $ tbStatus report'
            Left err -> assertFailure $ "can't get report: " ++ err

-- *Properties

-- |Is unit synthesis process complete (by function and variables).
finitePUSynthesisProp name pu0 fsGen =
    testProperty name $ do
        (pu, fs) <- processAlgOnEndpointGen pu0 fsGen
        return $
            isProcessComplete pu fs
                && null (endpointOptions pu)

{- |A computational process of functional (Haskell) and logical (Verilog)
simulation should be identical for any correct algorithm.
-}
puCoSimProp name pu0 fsGen =
    testProperty name $ do
        (pu, fs) <- processAlgOnEndpointGen pu0 fsGen
        pTestCntx <- initialCycleCntxGen fs
        return $
            monadicIO $
                run $ do
                    unless (isProcessComplete pu fs) $
                        error $ "process is not complete: " <> incompleteProcessMsg pu fs
                    i <- incrCounter 1 externalTestCntr
                    wd <- getCurrentDirectory
                    let pTargetProjectPath = joinPath [wd, "gen", toModuleName name <> "_" <> show i]
                        prj =
                            Project
                                { pName = T.pack $ toModuleName name
                                , pLibPath = "hdl"
                                , pTargetProjectPath
                                , pNittaPath = "."
                                , pUnit = pu
                                , pUnitEnv = def
                                , pTestCntx
                                , pTemplates = ["templates/Icarus"]
                                }
                    writeProject prj
                    res <- runTestbench prj
                    unless (tbStatus res) $ error $ "Fail CoSim in: " <> pTargetProjectPath