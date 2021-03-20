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
module NITTA.Model.ProcessorUnits.Tests.Utils (
    puCoSimTestCase,
    nittaCoSimTestCase,
    finitePUSynthesisProp,
    puCoSimProp,
    algGen,
) where

import Control.Monad
import Data.Atomics.Counter (incrCounter)
import Data.CallStack
import Data.Default
import Data.List (delete)
import qualified Data.Map.Strict as M
import Data.Set (elems, empty, fromList, intersection, union)
import qualified Data.String.Utils as S
import qualified Data.Text as T
import qualified Debug.Trace as DebugTrace
import NITTA.Intermediate.DataFlow
import NITTA.Intermediate.Functions ()
import NITTA.Intermediate.Simulation
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Networks.Types
import NITTA.Model.Problems hiding (Bind, BreakLoop)
import NITTA.Model.ProcessorUnits
import NITTA.Model.ProcessorUnits.Tests.IntegrityCheck
import NITTA.Model.TargetSystem ()
import NITTA.Model.Tests.Microarchitecture
import NITTA.Project
import qualified NITTA.Project as P
import NITTA.Synthesis
import NITTA.Utils
import System.Directory
import System.FilePath.Posix (joinPath)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?))
import Test.Tasty.QuickCheck (testProperty)

-- *Test cases

-- |Execute co-simulation test for the specific process unit
puCoSimTestCase ::
    ( HasCallStack
    , PUClasses (pu String x Int) String x Int
    , WithFunctions (pu String x Int) (F String x)
    , P.Testable (pu String x Int) String x
    , DefaultX (pu String x Int) x
    ) =>
    String ->
    pu String x Int ->
    [(String, x)] ->
    [F String x] ->
    TestTree
puCoSimTestCase name u cntxCycle alg =
    testCase name $ do
        wd <- getCurrentDirectory
        let mname = toModuleName name
            pTargetProjectPath = joinPath [wd, "gen", mname]
            prj =
                Project
                    { pName = T.pack mname
                    , pLibPath = "hdl"
                    , pTargetProjectPath
                    , pNittaPath = "."
                    , pUnit = naiveSynthesis alg u
                    , pUnitEnv = def
                    , pTestCntx = simulateAlg 5 (CycleCntx $ M.fromList cntxCycle) [] alg
                    , pTemplates = ["templates/Icarus"]
                    }
        writeProject prj
        (tbStatus <$> runTestbench prj) @? (name <> " in " <> pTargetProjectPath)

{- |Bind all functions to processor unit and synthesis process with endpoint
decisions.
-}
naiveSynthesis alg u0 = naiveSynthesis' $ foldl (flip bind) u0 alg
    where
        naiveSynthesis' u
            | opt : _ <- endpointOptions u =
                naiveSynthesis' $ endpointDecision u $ endpointOptionToDecision opt
            | otherwise = u

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
            Right report' -> assertBool "report with bad status" $ tbStatus $ DebugTrace.traceShow report' report'
            Left err -> assertFailure $ "can't get report: " ++ err

-- *Properties

-- |Is unit synthesis process complete (by function and variables).
finitePUSynthesisProp name pu0 fsGen =
    testProperty name $ do
        (pu, fs) <- processAlgOnEndpointGen pu0 fsGen
        return $
            isProcessComplete pu fs
                && null (endpointOptions pu)
                && checkIntegrity pu fs

isProcessComplete pu fs = unionsMap variables fs == processedVars pu

incompleteProcessMsg pu fs =
    "expected: " <> show (elems $ unionsMap variables fs)
        <> " actual: "
        <> show (elems $ processedVars pu)

processedVars pu = unionsMap variables $ getEndpoints $ process pu

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
                    unless (isProcessComplete pu fs && checkIntegrity pu fs) $
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

algGen fsGen = fmap avoidDupVariables $ listOf1 $ oneof fsGen
    where
        avoidDupVariables alg =
            snd $
                foldl
                    ( \(takenVs, fs) f ->
                        let vs = variables f
                         in if null (vs `intersection` takenVs)
                                then (vs `union` takenVs, f : fs)
                                else (takenVs, fs)
                    )
                    (empty, [])
                    alg

initialCycleCntxGen fs = do
    let vs = elems $ unionsMap inputs fs
    xs <- infiniteListOf arbitrary
    let vxs = M.fromList $ zip vs xs
        cntx0 = simulateAlg 5 (CycleCntx vxs) [] fs
    return cntx0

{- |Automatic synthesis evaluation process with random decisions. If we can't bind
function to PU then we skip it.
-}
processAlgOnEndpointGen pu0 algGen' = do
    alg <- algGen'
    algSynthesisGen alg [] pu0

-- FIXME: support new synthesis/refactor style
data PUSynthesisTask r f e = BreakLoop r | Bind f | Transport e

algSynthesisGen fRemain fPassed pu = select tasksList
    where
        tasksList =
            concat
                [ map BreakLoop $ breakLoopOptions pu
                , map Bind fRemain
                , map Transport $ endpointOptions pu
                ]

        select [] = return (pu, fPassed)
        select tasks = taskPattern =<< elements tasks

        taskPattern (BreakLoop r) = algSynthesisGen fRemain fPassed $ breakLoopDecision pu r
        taskPattern (Bind f) = case tryBind f pu of
            (Right pu') -> algSynthesisGen fRemain' (f : fPassed) pu'
            (Left _err) -> algSynthesisGen fRemain' fPassed pu
            where
                fRemain' = delete f fRemain
        taskPattern (Transport e) = do
            d <- endpointOptionToDecision <$> endpointGen e
            let pu' = endpointDecision pu d
            algSynthesisGen fRemain fPassed pu'

        endpointGen option@EndpointSt{epRole = Source vs} = do
            vs' <- suchThat (sublistOf $ elems vs) (not . null)
            return option{epRole = Source $ fromList vs'}
        endpointGen o = return o
