{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -fno-warn-redundant-constraints #-}
{-# OPTIONS -fno-warn-dodgy-exports #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Tests.Providers
Description : Utils for processor unit testing
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Tests.Providers (
    puCoSimTestCase,
    finitePUSynthesisProp,
    puCoSimProp,
    module NITTA.Model.ProcessorUnits,
    module NITTA.Intermediate.Functions,
    module NITTA.Intermediate.Types,
    module NITTA.Intermediate.Tests.Functions,
    module NITTA.Model.ProcessorUnits.Tests.DSL,
    module NITTA.Model.ProcessorUnits.Tests.Utils,
) where

import Control.Monad
import Data.CallStack
import Data.Data
import Data.Default
import qualified Data.Text as T
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Tests.Functions ()
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Types
import NITTA.Model.Problems hiding (Bind, BreakLoop)
import NITTA.Model.ProcessorUnits
import NITTA.Model.ProcessorUnits.Tests.DSL
import NITTA.Model.ProcessorUnits.Tests.Utils
import NITTA.Model.Tests.Internals
import NITTA.Project
import qualified NITTA.Project as P
import NITTA.Utils
import System.Directory
import System.FilePath.Posix
import Test.QuickCheck.Monadic
import Test.Tasty (TestTree)
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
        decideNaiveSynthesis
        assertCoSimulation

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
                    pwd <- getCurrentDirectory
                    let pTargetProjectPath = "gen" </> (toModuleName name <> "_" <> show i)
                        pInProjectNittaPath = "."
                        prj =
                            Project
                                { pName = T.pack $ toModuleName name
                                , pLibPath = "hdl"
                                , pTargetProjectPath
                                , pAbsTargetProjectPath = pwd </> pTargetProjectPath
                                , pInProjectNittaPath
                                , pAbsNittaPath = pwd </> pInProjectNittaPath </> pTargetProjectPath
                                , pUnit = pu
                                , pUnitEnv = def
                                , pTestCntx
                                , pTemplates = ["templates/Icarus"]
                                }
                    writeProject prj
                    res <- runTestbench prj
                    unless (tbStatus res) $ error $ "Fail CoSim in: " <> pTargetProjectPath
