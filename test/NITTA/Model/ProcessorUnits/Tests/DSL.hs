{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Tests.DSL
Description : Provides functions to test PU, by making syntesis decisions
Copyright   : (c) Artyom Kostyuchik, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Tests.DSL (
    puUnitTestCase,
    assign,
    setValue,
    setValues,
    decide,
    decideAt,
    consume,
    provide,
    assertBindFullness,
    assertCoSimulation,
    assertSynthesisDone,
    tracePU,
    tracePUSub,
    traceFunctions,
    traceEndpoints,
) where

import Control.Monad.Identity
import Control.Monad.State.Lazy
import Data.CallStack
import Data.List (find)
import qualified Data.Set as S
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
    !_ <- evalUnitTestState name pu alg -- ! probably do not always work
    assertBool "test failed" True

data UnitTestState pu v x = UnitTestState
    { testName :: String
    , unit :: pu
    , functs :: [F v x]
    , -- | Initial values for coSimulation
      cntxCycle :: [(String, x)]
    }
    deriving (Show)

evalUnitTestState name st alg = evalStateT alg (UnitTestState name st [] [])

assign f = do
    st@UnitTestState{unit, functs} <- get
    case tryBind f unit of
        Right unit_ -> put st{unit = unit_, functs = f : functs}
        Left err -> lift $ assertFailure err

setValue var val = do
    pu@UnitTestState{cntxCycle} <- get
    -- TODO What if variable already present?
    put pu{cntxCycle = (var, val) : cntxCycle}

setValues = mapM_ (uncurry setValue)

-- | Make synthesis decision with provided Endpoint Role and automatically assigned time
decide role = do
    des <- epAt <$> getDecisionSpecific role
    doDecision $ EndpointSt role des

decideAt a b role = doDecision $ EndpointSt role (a ... b)

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

consume = Target

provide = Source . S.fromList

getDecisionSpecific role = do
    let s = variables role
    des <- getDecisionsFromEp
    case find (\case EndpointSt{epRole} | S.isSubsetOf s $ variables epRole -> True; _ -> False) des of
        Just v -> return $ endpointOptionToDecision v
        Nothing -> lift $ assertFailure $ "Can't provide decision with variable: " <> show role

getDecisionsFromEp = do
    UnitTestState{unit} <- get
    case endpointOptions unit of
        [] -> lift $ assertFailure "Failed during decision making: there is no decisions left!"
        opts -> return opts

assertBindFullness = do
    UnitTestState{unit, functs} <- get
    isOk <- lift $ isFullyBinded unit functs
    unless isOk $
        lift $ assertFailure $ "Function is not binded to process! expected: " <> show functs <> "; actual: " <> show (functions unit)

isFullyBinded ::
    ( WithFunctions a c
    , Function c k1
    , Label c
    , Function b k1
    , Label b
    , Ord k1
    , Show k1
    ) =>
    a ->
    [b] ->
    IO Bool
isFullyBinded pu fs = do
    assertBool ("Outputs not equal, expected: " <> show fOuts <> "; actual: " <> show outs) $ outs == fOuts
    assertBool ("Inputs not equal, expected: " <> show fInps <> "; actual: " <> show inps) $ inps == fInps
    assertBool ("Signs not equal, expected: " <> show fSign <> "; actual: " <> show sign) $ sign == fSign
    return $ not $ null fu
    where
        fu = functions pu
        outs = S.fromList $ map outputs fu
        inps = S.fromList $ map inputs fu
        sign = S.fromList $ map label fu
        fOuts = S.fromList $ map outputs fs
        fInps = S.fromList $ map inputs fs
        fSign = S.fromList $ map label fs

assertSynthesisDone =
    do
        UnitTestState{unit, functs} <- get
        unless (isProcessComplete unit functs && null (endpointOptions unit)) $
            lift $ assertFailure "Process is not complete"

assertCoSimulation = do
    assertSynthesisDone
    UnitTestState{unit, functs, testName, cntxCycle} <- get
    res <- lift $ puCoSim testName unit cntxCycle functs False
    unless (tbStatus res) $
        lift $ assertFailure "Simulation failed"

tracePU = tracePUSub id

tracePUSub f = do
    UnitTestState{unit} <- get
    lift $ putStrLn $ "\nPU: " <> show (f unit)

traceFunctions = do
    UnitTestState{functs} <- get
    lift $ putStrLn $ "\nFunctions: " <> show functs

traceEndpoints = do
    UnitTestState{unit} <- get
    lift $ do
        putStrLn "Endpoints:"
        mapM_ (\ep -> putStrLn $ "- " <> show ep) $ endpointOptions unit
    return ()
