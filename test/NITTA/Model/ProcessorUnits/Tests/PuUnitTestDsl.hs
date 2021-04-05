{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
Module      : NITTA.Model.MultiplierDsl
Description : Provides functions to make decisions in PU
Copyright   : (c) Artyom Kostyuchik, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Tests.PuUnitTestDsl (
    UnitTestState (..),
    puUnitTestCase,
    bindFunc,
    doDecision,
    doFstDecision,
    beTargetAt,
    beSourceAt,
    doDecisionWithTarget,
    doDecisionWithSource,
    assertBindFullness,
    assertCoSimulation,
    assertSynthesisDone,
    traceProcess,
    traceFunctions,
) where

import Control.Monad.Identity
import Control.Monad.State.Lazy
import Data.CallStack
import Data.Maybe
import qualified Data.Set as S
import qualified Debug.Trace
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Model.ProcessorUnits.Tests.Utils
import NITTA.Project
import NITTA.Utils
import Numeric.Interval.NonEmpty hiding (elem)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

puUnitTestCase ::
    HasCallStack =>
    String ->
    pu ->
    StateT (UnitTestState pu v x) IO () ->
    TestTree
puUnitTestCase name pu alg = testCase name $ do
    !_ <- evalMultiplier pu alg -- ! probably not work
    assertBool "test failed" True

data UnitTestState pu v x = UnitTestState
    { unit :: pu
    , functs :: [F v x]
    }
    deriving (Show)

evalMultiplier st alg = evalStateT alg (UnitTestState st [])

bindFunc f = do
    st@UnitTestState{unit, functs} <- get
    case tryBind f unit of
        Right unit_ -> put st{unit = unit_, functs = f : functs}
        Left err -> lift $ assertFailure err

doDecision endpSt = do
    st@UnitTestState{unit} <- get
    let isAvailable = isEpOptionAvailable endpSt unit
    if isAvailable
        then put st{unit = endpointDecision unit endpSt}
        else lift $ assertFailure $ "Such option isn't available: " <> show endpSt <> "; from list: " <> show (endpointOptions unit)

isEpOptionAvailable (EndpointSt v interv) pu =
    let compEpRoles = case v of
            (Target _) -> v `elem` map epRole (endpointOptions pu)
            (Source s) -> S.isSubsetOf s $ unionsMap (variables . epRole) $ endpointOptions pu
        compIntervs = singleton (nextTick $ process pu) <=! interv
     in compEpRoles && compIntervs

doFstDecision = do
    fDes <- getFstDecision
    doDecision fDes

doDecisionWithTarget t = do
    fDes <- getFstDecision
    doDecision $ EndpointSt (Target t) $ epAt fDes

doDecisionWithSource ss = do
    fDes <- getFstDecision
    doDecision $ EndpointSt (Source $ S.fromList ss) $ epAt fDes

getFstDecision = do
    UnitTestState{unit} <- get
    if isJust $ checkFstDecision unit
        then return $ getFstDecision' unit
        else lift $ assertFailure "Failed at fstDecision, there is no decisions left!"
    where
        checkFstDecision unit = listToMaybe $ endpointOptions unit
        getFstDecision' unit = endpointOptionToDecision $ head $ endpointOptions unit

beTargetAt a b t = EndpointSt (Target t) (a ... b)
beSourceAt a b ss = EndpointSt (Source $ S.fromList ss) (a ... b)

assertBindFullness = do
    UnitTestState{unit, functs} <- get
    unless (isFullyBinded unit functs) $
        lift $ assertFailure $ "Function is not binded to process! expected: " <> show functs <> "; actual: " <> show (functions unit)

isFullyBinded ::
    ( WithFunctions a c
    , Function c k1
    , Label c
    , Function b k1
    , Label b
    , Ord k1
    ) =>
    a ->
    [b] ->
    Bool
isFullyBinded pu fs =
    let fu = functions pu
        outs = S.fromList $ map outputs fu
        inps = S.fromList $ map inputs fu
        sign = S.fromList $ map label fu
     in not (null fu)
            && outs == S.fromList (map outputs fs)
            && inps == S.fromList (map inputs fs)
            && sign == S.fromList (map label fs)

assertSynthesisDone =
    do
        UnitTestState{unit, functs} <- get
        unless (isProcessComplete unit functs && null (endpointOptions unit)) $
            lift $ assertFailure "Process is not complete"

assertCoSimulation cntxCycle = do
    assertSynthesisDone
    UnitTestState{unit, functs} <- get
    res <- lift $ puCoSim "test_multiplier_in_edsl" unit cntxCycle functs False
    unless (tbStatus res) $
        lift $ assertFailure "Simulation failed"

traceProcess = do
    UnitTestState{unit} <- get
    return $ Debug.Trace.traceShow unit unit

traceFunctions = do
    UnitTestState{functs} <- get
    return $ Debug.Trace.traceShow functs functs
