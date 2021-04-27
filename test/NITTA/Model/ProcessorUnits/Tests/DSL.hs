{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
    assigns,
    assignNaive,
    assignsNaive,
    setValue,
    setValues,

    -- *Process Unit Control
    decide,
    decideAt,
    consume,
    provide,
    breakLoop,
    decideNaiveSynthesis,

    -- *Asserts
    assertBindFullness,
    assertCoSimulation,
    assertSynthesisDone,

    -- *Trace
    tracePU,
    traceFunctions,
    traceEndpoints,
    traceProcess,
) where

import Control.Monad.Identity
import Control.Monad.State.Lazy
import Data.CallStack
import Data.List (find)
import qualified Data.Set as S
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Types (PUClasses)
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Model.ProcessorUnits.Tests.Utils
import NITTA.Model.Types
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
    _ <- evalUnitTestState name pu alg
    assertBool "test failed" True

data UnitTestState pu v x = UnitTestState
    { testName :: String
    , unit :: pu
    , functs :: [F v x]
    , -- | Initial values for coSimulation
      cntxCycle :: [(String, x)]
    }
    deriving (Show)

type DSLStatement pu v x t r = (HasCallStack, ProcessorUnit pu v x t, EndpointProblem pu v t) => StateT (UnitTestState pu v x) IO r

evalUnitTestState name st alg = evalStateT alg (UnitTestState name st [] [])

-- | Binds several provided functions to PU
assigns alg = mapM_ assign alg

-- | Binds provided function to PU
assign :: F v x -> DSLStatement pu v x t ()
assign f = do
    st@UnitTestState{unit, functs} <- get
    case tryBind f unit of
        Right unit_ -> put st{unit = unit_, functs = f : functs}
        Left err -> lift $ assertFailure err

{- | Store several provided functions and its initial values
for naive coSimulation
-}
assignsNaive alg cntxs = mapM_ (`assignNaive` cntxs) alg

{- | Store provided function and its initial values
for naive coSimulation
-}
assignNaive f cntxs = do
    st@UnitTestState{functs, cntxCycle} <- get
    put st{functs = f : functs, cntxCycle = cntxs <> cntxCycle}

-- | set initital values for coSimulation input variables
setValues :: (Function f String, WithFunctions pu f) => [(String, x)] -> DSLStatement pu String x t ()
setValues = mapM_ (uncurry setValue)

-- | set initital value for coSimulation input variables
setValue :: (Function f String, WithFunctions pu f) => String -> x -> DSLStatement pu String x t ()
setValue var val = do
    pu@UnitTestState{cntxCycle, unit} <- get
    when (var `elem` map fst cntxCycle) $
        lift $ assertFailure $ "The variable '" <> show var <> "' is already set!"
    unless (isVarAvailable var unit) $
        lift $ assertFailure $ "It's not possible to set the variable '" <> show var <> "'! It's not present in process"
    put pu{cntxCycle = (var, val) : cntxCycle}
    where
        isVarAvailable v pu = S.isSubsetOf (S.fromList [v]) $ inpVars $ functions pu

-- | Make synthesis decision with provided Endpoint Role and automatically assigned time
decide :: EndpointRole v -> DSLStatement pu v x t ()
decide role = do
    des <- epAt <$> getDecisionSpecific role
    doDecision $ EndpointSt role des

-- | Make synthesis decision with provided Endpoint Role and manually selected interval
decideAt :: t -> t -> EndpointRole v -> DSLStatement pu v x t ()
decideAt from to role = doDecision $ EndpointSt role (from ... to)

doDecision :: EndpointSt v (Interval t) -> DSLStatement pu v x t ()
doDecision endpSt = do
    st@UnitTestState{unit} <- get
    let isAvailable = isEpOptionAvailable endpSt unit
    if isAvailable
        then put st{unit = endpointDecision unit endpSt}
        else lift $ assertFailure $ "Such option isn't available: " <> show endpSt

isEpOptionAvailable (EndpointSt v interv) pu =
    let compIntervs = singleton (nextTick $ process pu) <=! interv
        compEpRoles = case v of
            (Target _) -> v `elem` map epRole (endpointOptions pu)
            (Source s) -> S.isSubsetOf s $ unionsMap ((\case Source ss -> ss; _ -> S.empty) . epRole) $ endpointOptions pu
     in compIntervs && compEpRoles

-- |Bind all functions to processor unit and decide till decisions left.
decideNaiveSynthesis :: DSLStatement pu v x t ()
decideNaiveSynthesis = do
    st@UnitTestState{unit, functs} <- get
    when (null functs) $
        lift $ assertFailure "You should assign function to do naive synthesis!"
    put st{unit = naiveSynthesis functs unit}

-- | Transforms provided variable to Target
consume = Target

-- | Transforms provided variables to Source
provide = Source . S.fromList

getDecisionSpecific :: EndpointRole v -> DSLStatement pu v x t (EndpointSt v (Interval t))
getDecisionSpecific role = do
    let s = variables role
    des <- getDecisionsFromEp
    case find (\case EndpointSt{epRole} | S.isSubsetOf s $ variables epRole -> True; _ -> False) des of
        Just v -> return $ endpointOptionToDecision v
        Nothing -> lift $ assertFailure $ "Can't provide decision with variable: " <> show s

getDecisionsFromEp :: DSLStatement pu v x t [EndpointSt v (TimeConstrain t)]
getDecisionsFromEp = do
    UnitTestState{unit} <- get
    case endpointOptions unit of
        [] -> lift $ assertFailure "Failed during decision making: there is no decisions left!"
        opts -> return opts

-- | Breaks loop on PU by using breakLoopDecision function
breakLoop :: BreakLoopProblem pu v x => x -> v -> [v] -> DSLStatement pu v x t ()
breakLoop x i o = do
    st@UnitTestState{unit} <- get
    case breakLoopOptions unit of
        [] -> lift $ assertFailure "Break loop function is not supported for such type of PU"
        _ -> put st{unit = breakLoopDecision unit BreakLoop{loopX = x, loopO = S.fromList o, loopI = i}}

assertBindFullness :: (Function f v, WithFunctions pu f, Show f) => DSLStatement pu v x t ()
assertBindFullness = do
    UnitTestState{unit, functs} <- get
    isOk <- lift $ isFullyBinded unit functs
    unless isOk $
        lift $ assertFailure $ "Function is not binded to process! expected: " ++ concatMap show functs ++ "; actual: " ++ concatMap show (functions unit)

isFullyBinded pu fs = do
    assertBool ("Outputs not equal, expected: " <> show fOuts <> "; actual: " <> show outs) $ outs == fOuts
    assertBool ("Inputs not equal, expected: " <> show fInps <> "; actual: " <> show inps) $ inps == fInps
    return $ not $ null fu
    where
        fu = functions pu
        outs = S.fromList $ map outputs fu
        inps = S.fromList $ map inputs fu
        fOuts = S.fromList $ map outputs fs
        fInps = S.fromList $ map inputs fs

assertSynthesisDone :: DSLStatement pu v x t ()
assertSynthesisDone = do
    UnitTestState{unit, functs, testName} <- get
    unless (isProcessComplete unit functs && null (endpointOptions unit)) $
        lift $ assertFailure $ testName <> " Process is not done: " <> incompleteProcessMsg unit functs

assertCoSimulation ::
    ( PUClasses pu String x Int
    , WithFunctions pu (F String x)
    , Testable pu String x
    , DefaultX pu x
    ) =>
    DSLStatement pu String x Int ()
assertCoSimulation =
    let checkInputVars pu fs cntx = S.union (inpVars $ functions pu) (inpVars fs) == S.fromList (map fst cntx)
     in do
            UnitTestState{unit, functs, testName, cntxCycle} <- get
            unless (checkInputVars unit functs cntxCycle) $
                lift $ assertFailure "you forgot to set initial values before coSimulation."

            res <- lift $ puCoSim testName unit cntxCycle functs False
            unless (P.tbStatus res) $
                lift $ assertFailure "coSimulation failed."

inpVars fs = unionsMap inputs fs
tracePU = do
    UnitTestState{unit} <- get
    lift $ putStrLn $ "PU: " <> show unit
    return ()

traceFunctions = do
    UnitTestState{functs} <- get
    lift $ putStrLn $ "Functions: " <> show functs
    return ()

traceEndpoints = do
    UnitTestState{unit} <- get
    lift $ do
        putStrLn "Endpoints:"
        mapM_ (\ep -> putStrLn $ "- " <> show ep) $ endpointOptions unit
    return ()

traceProcess = do
    UnitTestState{unit} <- get
    lift $ putStrLn $ "Process: " <> show (process unit)
    return ()
