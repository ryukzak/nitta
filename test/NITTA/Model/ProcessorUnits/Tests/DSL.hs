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

    -- *Asserts
    assertBindFullness,
    assertCoSimulation,
    assertNaiveCoSimulation,
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
import qualified NITTA.Model.Networks.Types as N
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Model.ProcessorUnits.Tests.Utils
import qualified NITTA.Model.Types as TS
import qualified NITTA.Project as P
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

evalUnitTestState name st alg = evalStateT alg (UnitTestState name st [] [])

-- | Binds several provided functions to PU
assigns alg = mapM_ assign alg

-- | Binds provided function to PU
assign ::
    ( HasCallStack
    , MonadState (UnitTestState pu v x) (t1 IO)
    , ProcessorUnit pu v x t2
    , MonadTrans t1
    ) =>
    F v x ->
    t1 IO ()
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
setValues ::
    ( HasCallStack
    , Foldable t1
    , MonadState (UnitTestState pu v b) (t2 IO)
    , Eq b
    , MonadTrans t2
    , Function f String
    , WithFunctions pu f
    ) =>
    t1 (String, b) ->
    t2 IO ()
setValues = mapM_ (uncurry setValue)

-- | set initital value for coSimulation input variables
setValue ::
    ( HasCallStack
    , MonadState (UnitTestState pu v b) (t IO)
    , Eq b
    , MonadTrans t
    , Function f String
    , WithFunctions pu f
    ) =>
    String ->
    b ->
    t IO ()
setValue var val = do
    pu@UnitTestState{cntxCycle, unit} <- get
    when ((var, val) `elem` cntxCycle) $
        lift $ assertFailure $ "The variable '" <> show var <> "' is already set!"
    unless (isVarAvailable var unit) $
        lift $ assertFailure $ "It's not possible to set the variable '" <> show var <> "'! It's not present in process"
    put pu{cntxCycle = (var, val) : cntxCycle}
    where
        isVarAvailable v pu = S.isSubsetOf (S.fromList [v]) $ inpVars $ functions pu

-- | Make synthesis decision with provided Endpoint Role and automatically assigned time
decide ::
    ( HasCallStack
    , EndpointProblem u v t2
    , MonadTrans t
    , MonadState (UnitTestState u v1 x1) (t IO)
    , ProcessorUnit u v3 x2 t2
    , Show v
    , Show (EndpointRole v)
    , Ord v
    ) =>
    EndpointRole v ->
    t IO ()
decide role = do
    des <- epAt <$> getDecisionSpecific role
    doDecision $ EndpointSt role des

-- | Make synthesis decision with provided Endpoint Role and manually selected interval
decideAt ::
    ( HasCallStack
    , EndpointProblem u v2 t2
    , MonadTrans t1
    , MonadState (UnitTestState u v1 x1) (t1 IO)
    , ProcessorUnit u v3 x2 t2
    , Show v2
    , Ord v2
    ) =>
    t2 ->
    t2 ->
    EndpointRole v2 ->
    t1 IO ()
decideAt from to role = doDecision $ EndpointSt role (from ... to)

doDecision ::
    ( HasCallStack
    , MonadState (UnitTestState u v1 x1) (t1 IO)
    , MonadTrans t1
    , Show v2
    , EndpointProblem u v2 a
    , EndpointProblem u v2 t2
    , ProcessorUnit u v3 x2 a
    , Ord v2
    ) =>
    EndpointSt v2 (Interval a) ->
    t1 IO ()
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

-- | Transforms provided variable to Target
consume = Target

-- | Transforms provided variables to Source
provide = Source . S.fromList

getDecisionSpecific ::
    ( HasCallStack
    , Variables a1 a2
    , MonadTrans t
    , MonadState (UnitTestState u v x) (t IO)
    , EndpointProblem u a2 a3
    , Num a3
    , Ord a2
    , Ord a3
    , Show a1
    ) =>
    a1 ->
    t IO (EndpointSt a2 (Interval a3))
getDecisionSpecific role = do
    let s = variables role
    des <- getDecisionsFromEp
    case find (\case EndpointSt{epRole} | S.isSubsetOf s $ variables epRole -> True; _ -> False) des of
        Just v -> return $ endpointOptionToDecision v
        Nothing -> lift $ assertFailure $ "Can't provide decision with variable: " <> show role

getDecisionsFromEp ::
    ( HasCallStack
    , MonadState (UnitTestState u v1 x) (t1 IO)
    , EndpointProblem u v2 t2
    , MonadTrans t1
    ) =>
    t1 IO [EndpointSt v2 (TS.TimeConstrain t2)]
getDecisionsFromEp = do
    UnitTestState{unit} <- get
    case endpointOptions unit of
        [] -> lift $ assertFailure "Failed during decision making: there is no decisions left!"
        opts -> return opts

-- | Breaks loop on PU by using breakLoopDecision function
breakLoop ::
    ( HasCallStack
    , MonadState (UnitTestState pu v x) (t IO)
    , MonadTrans t
    , BreakLoopProblem pu v2 x2
    , Ord v2
    ) =>
    x2 ->
    v2 ->
    [v2] ->
    t IO ()
breakLoop x i o = do
    st@UnitTestState{unit} <- get
    case breakLoopOptions unit of
        [] -> lift $ assertFailure "Break loop function is not supported for such type of PU"
        _ -> put st{unit = breakLoopDecision unit BreakLoop{loopX = x, loopO = S.fromList o, loopI = i}}

assertBindFullness ::
    ( HasCallStack
    , WithFunctions a b
    , Show b
    , Show v
    , MonadState (UnitTestState a v x) (t IO)
    , MonadTrans t
    , Ord v
    , Function b v
    ) =>
    t IO ()
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

assertSynthesisDone ::
    ( HasCallStack
    , ProcessorUnit pu v x c
    , EndpointProblem pu v2 c2
    , MonadTrans t
    , MonadState (UnitTestState pu v x) (t IO)
    ) =>
    t IO ()
assertSynthesisDone = do
    UnitTestState{unit, functs, testName} <- get
    unless (isProcessComplete unit functs && null (endpointOptions unit)) $
        lift $ assertFailure $ testName <> " Process is not done: " <> incompleteProcessMsg unit functs

assertNaiveCoSimulation ::
    ( HasCallStack
    , MonadState (UnitTestState pu String x) (t IO)
    , MonadTrans t
    , N.PUClasses pu String x Int
    , WithFunctions pu (F String x)
    , P.Testable pu String x
    , DefaultX pu x
    ) =>
    t IO ()
assertNaiveCoSimulation = assertCoSimulation' True

assertCoSimulation ::
    ( HasCallStack
    , MonadState (UnitTestState pu String x) (t IO)
    , MonadTrans t
    , N.PUClasses pu String x Int
    , WithFunctions pu (F String x)
    , P.Testable pu String x
    , DefaultX pu x
    ) =>
    t IO ()
assertCoSimulation = assertCoSimulation' False

assertCoSimulation' ::
    ( HasCallStack
    , MonadState (UnitTestState pu String x) (t IO)
    , MonadTrans t
    , N.PUClasses pu String x Int
    , WithFunctions pu (F String x)
    , P.Testable pu String x
    , DefaultX pu x
    ) =>
    Bool ->
    t IO ()
assertCoSimulation' isNaive =
    let checkInputVars pu fs cntx = S.union (inpVars $ functions pu) (inpVars fs) == S.fromList (map fst cntx)
     in do
            UnitTestState{unit, functs, testName, cntxCycle} <- get
            unless (checkInputVars unit functs cntxCycle) $
                lift $ assertFailure "you forgot to set initial values before coSimulation."

            res <- lift $ puCoSim testName unit cntxCycle functs isNaive
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
