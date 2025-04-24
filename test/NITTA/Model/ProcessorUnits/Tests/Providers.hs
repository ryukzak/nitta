{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

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
    puCoSimPropWithContext,
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
import Data.Text qualified as T
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Tests.Functions ()
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Types
import NITTA.Model.Problems hiding (Bind, BreakLoop)
import NITTA.Model.ProcessIntegrity
import NITTA.Model.ProcessorUnits
import NITTA.Model.ProcessorUnits.Tests.DSL
import NITTA.Model.ProcessorUnits.Tests.Utils
import NITTA.Model.Tests.Internals
import NITTA.Project
import NITTA.Project qualified as P
import NITTA.Utils
import System.Directory
import System.FilePath.Posix
import Test.QuickCheck.Monadic
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)

-- * Test cases

-- | Execute co-simulation test for the specific process unit
puCoSimTestCase ::
    ( HasCallStack
    , PUClasses (pu v x Int) v x Int
    , WithFunctions (pu v x Int) (F v x)
    , P.Testable (pu v x Int) v x
    , DefaultX (pu v x Int) x
    , Typeable pu
    , Var v
    ) =>
    String ->
    pu v x Int ->
    [(v, x)] ->
    [F v x] ->
    TestTree
puCoSimTestCase name u cntxCycle alg =
    unitTestCase name u $ do
        mapM_ (\f -> assignNaive cntxCycle f) alg
        decideNaiveSynthesis
        assertSynthesisDone
        assertPUCoSimulation

-- * Properties

-- | Is unit synthesis process complete (by function and variables).
finitePUSynthesisProp name pu0 fsGen =
    testProperty (toModuleName name) $ do
        (pu, fs) <- processAlgOnEndpointGen pu0 fsGen
        case checkProcessIntegrity pu of
            Left msg -> error msg
            Right _ ->
                return $
                    isProcessComplete pu fs
                        && null (endpointOptions pu)

{- | A computational process of functional (Haskell) and logical (Verilog)
simulation should be identical for any correct algorithm.
-}
puCoSimProp name pu0 fsGen =
    testProperty (toModuleName name) $ do
        (pu, fs) <- processAlgOnEndpointGen pu0 fsGen
        pTestCntx <- initialCycleCntxGen fs
        return $
            monadicIO $
                run $ do
                    uniqueName <- uniqTestPath (toModuleName name)
                    unless (isProcessComplete pu fs) $
                        error $
                            "process is not complete: " <> incompleteProcessMsg pu fs
                    case checkProcessIntegrity pu of
                        Left e -> error e
                        Right _ -> return ()
                    pwd <- getCurrentDirectory
                    let pTargetProjectPath = "gen" </> toModuleName uniqueName
                        pInProjectNittaPath = "."
                        prj =
                            Project
                                { pName = T.pack $ toModuleName uniqueName
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
                    report <- runTestbench prj
                    unless (tbStatus report) $ error $ "Fail CoSim in: " <> pTargetProjectPath

puCoSimPropWithContext name pu0 fsGen =
    testProperty (toModuleName name) $ do
        (fs, cntx) <- fsGen
        cntx' <- initialCycleCntxGen' fs cntx
        (pu, _) <- processAlgOnEndpointGen pu0 (return fs)
        return $
            monadicIO $
                run $ do
                    unless (isProcessComplete pu fs) $
                        error $
                            "Process incomplete: " <> incompleteProcessMsg pu fs
                    case checkProcessIntegrity pu of
                        Left e -> error e
                        Right _ -> return ()
                    uniqueName <- uniqTestPath (toModuleName name)
                    pwd <- getCurrentDirectory
                    let prj =
                            Project
                                { pName = T.pack uniqueName
                                , pLibPath = "hdl"
                                , pTargetProjectPath = "gen" </> uniqueName
                                , pAbsTargetProjectPath = pwd </> "gen" </> uniqueName
                                , pInProjectNittaPath = "."
                                , pAbsNittaPath = pwd </> "gen" </> uniqueName
                                , pUnit = pu
                                , pUnitEnv = def
                                , pTestCntx = cntx'
                                , pTemplates = ["templates/Icarus"]
                                }
                    writeProject prj
                    report <- runTestbench prj
                    unless (tbStatus report) $ error $ "CoSim failed: gen/" <> uniqueName
