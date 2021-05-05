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
    puCoSim,
    naiveSynthesis,
    isProcessComplete,
    incompleteProcessMsg,
    algGen,
    initialCycleCntxGen,
    processAlgOnEndpointGen,
    algSynthesisGen,
) where

import Data.CallStack
import Data.Default
import Data.List (delete)
import qualified Data.Map.Strict as M
import Data.Set (elems, empty, fromList, intersection, union)
import qualified Data.Text as T
import NITTA.Intermediate.Functions ()
import NITTA.Intermediate.Simulation
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Types
import NITTA.Model.Problems hiding (Bind, BreakLoop)
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem ()
import NITTA.Project
import qualified NITTA.Project as P
import NITTA.Utils
import System.Directory
import System.FilePath.Posix
import Test.QuickCheck

{- |Execute co-simulation test for the specific process unit
with or without "naive synthesis".
-}
puCoSim ::
    ( HasCallStack
    , PUClasses pu String x Int
    , WithFunctions pu (F String x)
    , P.Testable pu String x
    , DefaultX pu x
    ) =>
    String ->
    pu ->
    [(String, x)] ->
    [F String x] ->
    Bool ->
    IO (TestbenchReport String x)
puCoSim name u cntxCycle alg needBind = do
    pwd <- getCurrentDirectory
    let mname = toModuleName name
        pTargetProjectPath = "gen" </> mname
        pInProjectNittaPath = "."
        prj =
            Project
                { pName = T.pack mname
                , pLibPath = "hdl"
                , pTargetProjectPath
                , pAbsTargetProjectPath = pwd </> pTargetProjectPath
                , pInProjectNittaPath
                , pAbsNittaPath = pwd </> pInProjectNittaPath </> pTargetProjectPath
                , pUnit =
                    if needBind
                        then naiveSynthesis alg u
                        else u
                , pUnitEnv = def
                , pTestCntx = simulateAlg 5 (CycleCntx $ M.fromList cntxCycle) [] alg
                , pTemplates = ["templates/Icarus"]
                }
    writeProject prj
    runTestbench prj

{- |Bind all functions to processor unit and synthesis process with endpoint
decisions.
-}
naiveSynthesis alg u0 = naiveSynthesis' $ foldl (flip bind) u0 alg
    where
        naiveSynthesis' u
            | opt : _ <- endpointOptions u =
                naiveSynthesis' $ endpointDecision u $ endpointOptionToDecision opt
            | otherwise = u

isProcessComplete pu fs = unionsMap variables fs == processedVars pu

incompleteProcessMsg pu fs =
    "expected: " <> show (elems $ unionsMap variables fs)
        <> " actual: "
        <> show (elems $ processedVars pu)

processedVars pu = unionsMap variables $ getEndpoints $ process pu

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
