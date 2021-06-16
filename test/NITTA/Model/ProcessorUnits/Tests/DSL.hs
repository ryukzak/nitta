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

= Module description

DSL (domain-specific language) is a module for testing Processor Units (PU).

= Algorithm

1. Choose PU and provide it into unitTestCase.
2. Assign function to this PU.
3. Schedule computational process for every variable in function.
4. Assert (check) the resulting PU.

= Example

Test case (numbers to the right correspond to the algorithm steps):

@
unitTestCase "multiplier smoke test" pu $ do     -- 1. Created test case for provided PU

         assign $ multiply "a" "b" ["c", "d"]      -- 2. Bind function 'a * b = c = d' to PU
         setValue "a" 2                            --    Set initial input values
         setValue "b" 7                            --    for further CoSimulation

         decideAt 1 2 $ consume "a"                -- 3. Bind input variable "a" from 1 to 2 tick
         decide $ consume "b"                      --    Bind input variable "b" at nearest tick
         decideAt 5 5 $ provide ["c"]              --    Bind output variable "c" at 5 tick
         decide $ provide ["d"]                    --    Bind output variable "d" at nearest tick

         traceProcess                              --    Print current process state to console

         assertSynthesisDone                       -- 4. Check that all decisions are made
         assertCoSimulation                        --    Run CoSimulation for current PU

...

pu = multiplier True :: Multiplier String Int Int  -- 1. Chose PU: Multiplier
@

= Algorithm steps description

* You can use any PU which instantiated with 'NITTA.Model.ProcessorUnits.Types.ProcessorUnit' and 'NITTA.Model.Problems.Endpoint.EndpointProblem' type class

* There are 4 functions for assign:

    * assign - binds function to PU right at the moment.

    * assigns - binds like 'assign', but uses a list of functions as an input.

    * assignNaive - store function in Test State and binds it only at naive synthesis.
                    Don't forget to call 'decideNaive' function.

    * assignsNaive - works like 'assignNaive', but uses a list of functions as an input.

* You can bind variables from the function to PU:

    * for first you need to wrap variables:

        * consume - for input variable.

        * provide - for output variables.

    * For second, you can pass wrapped variables to 'decide' function and
      schedule (make synthesis decisions) them. There 3 types of 'decide':

        * decide               - bind variable at the next tick of PU (nearest).

        * decideAt             - bind variable at provided moment.

        * decideNaiveSynthesis - runs naive synthesis (makes all available decisions).
                                 Requires using 'assignNaive' function.

* Assert function could be at any place in the test case.
   For a positive test case it usually at the end.

= CoSimulation:

To run simulation use 'assertCoSimulation' function.
Don't forget to set initial input values with 'setValue' function.

= Debug:

For debugging use functions starting with trace*, e.g. 'tracePU'.
-}
module NITTA.Model.ProcessorUnits.Tests.DSL (
    unitTestCase,
    assign,
    assigns,
    assignNaive,
    assignsNaive,
    setValue,
    setValues,

    -- *Process Unit Control
    decide,
    decideAt,
    decideAtUnsafe,
    consume,
    provide,
    breakLoop,
    decideNaiveSynthesis,

    -- *Asserts
    assertBindFullness,
    assertCoSimulation,
    assertSynthesisDone,
    assertEndpoint,
    assertAllEndpointRoles,
    assertLocks,

    -- *Trace
    tracePU,
    traceFunctions,
    traceEndpoints,
    traceProcess,

    -- *Target synthesis
    setNetwork,
    setBusType,
    setRecievedValue,
    setRecievedValues,
    assignLua,
    bindInit,
    bindVariable,
    bindVariables,
    traceBindVariables,
    traceDataflow,
    traceTransferOptions,
    traceAvailableRefactor,
    traceBus,
    assertSynthesisDoneAuto,
    assertSynthesisRunAuto,
    transferVariables,
    transferVariablesAt,
    getLoopFunctions,
    applyBreakLoop,
    applyBreakLoops,
    assertLoopBroken,
    assignFunction,
    assignFunctions,
    applyConstantFolding,
    assertConstantFolded,
    applyOptimizeAccum,
    assertOptimizeAccum,
) where

import Control.Monad.Identity
import Control.Monad.State.Lazy
import Data.CallStack
import Data.Either
import Data.List (find, isSubsequenceOf)
import Data.Maybe
import Data.Proxy
import qualified Data.Set as S
import Data.String.ToString
import qualified Data.String.Utils as S
import qualified Data.Text as T
import NITTA.Intermediate.DataFlow
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.LuaFrontend
import NITTA.Model.Networks.Bus
import NITTA.Model.Networks.Types (PUClasses)
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Model.ProcessorUnits.Tests.Utils
import NITTA.Model.Tests.Internals
import NITTA.Project
import NITTA.Synthesis
import NITTA.Utils
import Numeric.Interval.NonEmpty hiding (elem)
import Prettyprinter (pretty)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

unitTestCase ::
    HasCallStack =>
    String ->
    u ->
    StateT (UnitTestState u v x) IO () ->
    TestTree
unitTestCase name pu alg = testCase name $ do
    void $ evalUnitTestState name pu alg

-- | State of the processor unit used in test
data UnitTestState pu v x = UnitTestState
    { testName :: String
    , -- | Processor unit model.
      unit :: pu
    , -- | Contains functions assigned to PU.
      -- There two types of assign function:
      -- 1. assign - binds to PU.
      -- 2. assignNaive - will be binded during naive synthesis.
      functs :: [F v x]
    , -- | Initial values for coSimulation
      cntxCycle :: [(v, x)]
    , -- | TODO: add suitable type
      busType :: Maybe (Proxy x)
    , report :: Either String (TestbenchReport v x)
    }
    deriving (Show)

type DSLStatement pu v x t r = (HasCallStack, ProcessorUnit pu v x t, EndpointProblem pu v t) => StateT (UnitTestState pu v x) IO r

evalUnitTestState name st alg =
    evalStateT
        alg
        UnitTestState
            { testName = name
            , unit = st
            , functs = []
            , cntxCycle = []
            , busType = Nothing
            , report = Left "Report not ready!"
            }

-- | Binds several provided functions to PU
assigns = mapM_ assign

-- | Binds provided function to PU
assign :: F v x -> DSLStatement pu v x t ()
assign f = do
    st@UnitTestState{unit, functs} <- get
    case tryBind f unit of
        Right unit_ -> put st{unit = unit_, functs = f : functs}
        Left err -> lift $ assertFailure $ "assign: " <> err

{- | Store several provided functions and its initial values
for naive coSimulation
-}
assignsNaive alg cntxs = mapM_ (`assignNaive` cntxs) alg

{- | Store provided function and its initial values
for naive coSimulation
-}
assignNaive f cntxs = do
    st@UnitTestState{functs, cntxCycle} <- get
    -- TODO: add if value is present
    put st{functs = f : functs, cntxCycle = cntxs <> cntxCycle}

-- | set initital values for coSimulation input variables
setValues :: (Function f v, WithFunctions pu f) => [(v, x)] -> DSLStatement pu v x t ()
setValues = mapM_ $ uncurry setValue

-- | set initital value for coSimulation input variables
setValue :: (Var v, Function f v, WithFunctions pu f) => v -> x -> DSLStatement pu v x t ()
setValue var val = do
    pu@UnitTestState{cntxCycle, unit} <- get
    when (var `elem` map fst cntxCycle) $
        lift $ assertFailure $ "The variable '" <> toString var <> "' is already set!"
    unless (isVarAvailable var unit) $
        lift $ assertFailure $ "It's not possible to set the variable '" <> toString var <> "'! It's not present in process"
    put pu{cntxCycle = (var, val) : cntxCycle}
    where
        isVarAvailable v pu = S.isSubsetOf (S.fromList [v]) $ inpVars $ functions pu

assignFunction f = assignFunctions [f]

assignFunctions fs = do
    st@UnitTestState{functs, unit = ts@TargetSynthesis{}} <- get
    let tDFG' = fsToDataFlowGraph $ fs <> functs
    put st{functs = fs <> functs, unit = ts{tDFG = tDFG'}}

-- TODO u can keep this variant, but not recommend
assignLua src =
    let translateToIntermediate = return . frDataFlow . lua2functions
     in do
            st@UnitTestState{functs, unit = ts@TargetSynthesis{}} <- get
            tDFG' <- maybe (return $ fsToDataFlowGraph functs) translateToIntermediate $ Just src
            put st{unit = ts{tSourceCode = Just src, tDFG = tDFG'}}

setBusType busType = modify' $ \st -> st{busType = Just busType}

setRecievedValues = mapM_ $ uncurry setRecievedValue

setRecievedValue var val = do
    st@UnitTestState{unit = ts@TargetSynthesis{tReceivedValues = vs}} <- get
    put st{unit = ts{tReceivedValues = (var, val) : vs}}

setNetwork network = do
    st@UnitTestState{unit = ts@TargetSynthesis{}} <- get
    put st{unit = ts{tMicroArch = network}}

-- | Allows manual binding function. Incompatible with auto synthesis.
bindInit = do
    st@UnitTestState{unit = ts@TargetSynthesis{tMicroArch, tDFG}} <- get
    root <- lift $ getTreeUnit tMicroArch tDFG
    put st{unit = ts{tMicroArch = root}}

bindVariable f = do
    st@UnitTestState{unit = ts@TargetSynthesis{tMicroArch}, functs} <- get
    case find (\(Bind f' _) -> f == f') $ bindOptions tMicroArch of
        Just decision -> put st{unit = ts{tMicroArch = bindDecision tMicroArch decision}, functs = f : functs}
        Nothing -> lift $ assertFailure ("Cannot bind variable: " <> show f)

bindVariables = mapM_ bindVariable

-- TODO: don't run it more than once
getTreeUnit tMicroArch tDfg = targetUnit <$> synthesisTreeRootIO (mkModelWithOneNetwork tMicroArch tDfg)

transferVariables v = transferVariables' v Nothing

transferVariablesAt v from to = transferVariables' v $ Just (from, to)

-- TODO: is it possible to use unsafe parameter?
transferVariables' v intrvl = do
    st@UnitTestState{unit = ts@TargetSynthesis{tMicroArch = ma@BusNetwork{}}} <- get
    let res = findDecision ma v intrvl
    unless (length res == 1) $
        lift $ assertFailure ("Cannot transfer variable: " <> show v)
    case length res of
        1 -> put st{unit = ts{tMicroArch = dataflowDecision ma $ dataflowOption2decision $ head res}}
        0 -> lift $ assertFailure ("Cannot transfer variable: " <> show v <> "; Haven't found any decisions.")
        _ -> lift $ assertFailure ("Cannot transfer variable: " <> show v <> "; There are more than one possible decision: " <> show res)

findDecision u v intrvl =
    let isSame dfo = any (isSubroleOf v) $ provider dfo : consumer dfo
        provider dfo = epRole $ snd $ dfSource dfo
        consumer dfo = map (epRole . snd) $ dfTargets dfo
        isIntrvl Nothing _ = True
        isIntrvl (Just (a, b)) dfo = isValidInterval (a ... b) $ epAt $ snd $ dfSource dfo
        isValidInterval atA atB =
            atA `isSubsetOf` tcAvailable atB
                && member (width atA + 1) (tcDuration atB)
     in filter (\dfo -> isSame dfo && isIntrvl intrvl dfo) $ dataflowOptions u

-- | Make synthesis decision with provided Endpoint Role and automatically assigned time
decide :: EndpointRole v -> DSLStatement pu v x t ()
decide role = do
    des <- epAt <$> getDecisionSpecific role
    doDecision False $ EndpointSt role des

-- | Make synthesis decision with provided Endpoint Role and manually selected interval
decideAt :: t -> t -> EndpointRole v -> DSLStatement pu v x t ()
decideAt from to role = doDecision False $ EndpointSt role (from ... to)

decideAtUnsafe :: t -> t -> EndpointRole v -> DSLStatement pu v x t ()
decideAtUnsafe from to role = doDecision True $ EndpointSt role (from ... to)

doDecision :: Bool -> EndpointSt v (Interval t) -> DSLStatement pu v x t ()
doDecision unsafe endpSt = do
    st@UnitTestState{unit} <- get
    let isAvailable = isEpOptionAvailable endpSt unit
    if unsafe || isAvailable
        then put st{unit = endpointDecision unit endpSt}
        else lift $ assertFailure $ "doDecision: such option isn't available: " <> show endpSt <> " from " <> show (endpointOptions unit)

isEpOptionAvailable EndpointSt{epRole = role, epAt = atA} pu =
    case find (isSubroleOf role . epRole) $ endpointOptions pu of
        Nothing -> False
        Just EndpointSt{epAt = atB} ->
            atA `isSubsetOf` tcAvailable atB
                && member (width atA + 1) (tcDuration atB)

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
        Nothing -> lift $ assertFailure $ "Can't provide decision with variable: " <> show (vsToStringList s)

getDecisionsFromEp :: DSLStatement pu v x t [EndpointSt v (TimeConstraint t)]
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

assertAllEndpointRoles :: (Var v) => [EndpointRole v] -> DSLStatement pu v x t ()
assertAllEndpointRoles roles = do
    UnitTestState{unit} <- get
    let opts = S.fromList $ map epRole $ endpointOptions unit
    lift $ assertBool ("Actual endpoint roles: " <> show opts) $ opts == S.fromList roles

assertEndpoint :: t -> t -> EndpointRole v -> DSLStatement pu v x t ()
assertEndpoint a b role = do
    UnitTestState{unit} <- get
    let opts = endpointOptions unit
        ep = EndpointSt{epAt = a ... b, epRole = role}
    case find (\EndpointSt{epAt, epRole} -> tcAvailable epAt == (a ... b) && epRole == role) opts of
        Nothing -> lift $ assertFailure $ "assertEndpoint: '" <> show ep <> "' not defined in: " <> show opts
        Just _ -> return ()

isFullyBinded pu fs = do
    assertBool ("Outputs not equal, expected: " <> show' fOuts <> "; actual: " <> show' outs) $ outs == fOuts
    assertBool ("Inputs not equal, expected: " <> show' fInps <> "; actual: " <> show' inps) $ inps == fInps
    return $ not $ null fu
    where
        fu = functions pu
        outs = unionsMap outputs fu
        inps = unionsMap inputs fu
        fOuts = unionsMap outputs fs
        fInps = unionsMap inputs fs
        show' = show . S.map toString

assertSynthesisDone :: DSLStatement pu v x t ()
assertSynthesisDone = do
    UnitTestState{unit, functs, testName} <- get
    unless (isProcessComplete unit functs && null (endpointOptions unit)) $
        lift $ assertFailure $ testName <> " Process is not done: " <> incompleteProcessMsg unit functs

-- | Run both automatic synthesis and Testbench.
assertSynthesisDoneAuto = assertSynthesis True

-- | Run only automatic synthesis without Testbench.
assertSynthesisRunAuto = assertSynthesis False

assertSynthesis isTestbench = do
    st@UnitTestState{testName, functs, unit = ts@TargetSynthesis{tSourceCode}} <- get
    when (null functs && isNothing tSourceCode) $
        lift $ assertFailure "Can't run target synthesis, you haven't provided any functions or source code"
    let wd = toModuleName $ toString testName
    let namedTs = ts{tName = if isJust tSourceCode then "lua_" <> wd else wd}
    result <- lift $ synthesizeTargetSystemWithUniqName namedTs
    case result of
        Left l -> lift $ assertFailure $ "target synthesis failed" <> show l
        Right r -> put st{unit = namedTs{tMicroArch = pUnit r}}
    when isTestbench $
        lift $ getTestbenchReport result

getTestbenchReport project = do
    reportTestbench <- traverse runTestbench project
    case reportTestbench of
        Left err -> assertFailure ("synthesis process fail " <> err)
        Right TestbenchReport{tbStatus = True} -> return ()
        Right report@TestbenchReport{tbCompilerDump}
            | T.length tbCompilerDump > 2 ->
                assertFailure ("icarus synthesis error:\n" <> show report)
        Right report@TestbenchReport{} ->
            assertFailure ("icarus simulation error:\n" <> show report)

assertLocks :: (Locks pu v) => [Lock v] -> DSLStatement pu v x t ()
assertLocks expectLocks = do
    UnitTestState{unit} <- get
    let actualLocks0 = locks unit
        actualLocks = S.fromList actualLocks0
    lift $ assertBool ("assertLocks: locks contain duplicates: " <> show actualLocks0) $ length actualLocks0 == S.size actualLocks
    lift $ assertBool ("assertLocks:\n  expected locks:\n" <> show' expectLocks <> "\n  actual:\n" <> show' actualLocks0) $ actualLocks == S.fromList expectLocks
    where
        show' ls = S.join "\n" $ map (("    " <>) . show) ls

assertCoSimulation ::
    ( PUClasses pu v x Int
    , WithFunctions pu (F v x)
    , Testable pu v x
    , DefaultX pu x
    , Var v
    ) =>
    DSLStatement pu v x Int ()
assertCoSimulation =
    let checkInputVars pu fs cntx = S.union (inpVars $ functions pu) (inpVars fs) == S.fromList (map fst cntx)
     in do
            UnitTestState{unit, functs, testName, cntxCycle} <- get
            unless (checkInputVars unit functs cntxCycle) $
                lift $ assertFailure "you forgot to set initial values before coSimulation."

            report@TestbenchReport{tbStatus} <-
                lift $ puCoSim testName unit cntxCycle functs False

            unless tbStatus $
                lift $ assertFailure $ "coSimulation failed: \n" <> show report

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
    lift $ putStrLn $ "Process: " <> show (pretty $ process unit)
    return ()

traceDataflow = do
    UnitTestState{unit = TargetSynthesis{tDFG}} <- get
    lift $ putStrLn $ "Dataflow: " <> show tDFG
    return ()

traceBus = do
    UnitTestState{unit = TargetSynthesis{tMicroArch = b@BusNetwork{}}} <- get
    lift $ putStrLn $ "Bus: " <> show (pretty $ process b)
    return ()

traceTransferOptions = do
    UnitTestState{unit = TargetSynthesis{tMicroArch = ma@BusNetwork{}}} <- get
    lift $ putStrLn $ "Dataflow options: " <> show (dataflowOptions ma)
    return ()

traceBindVariables = do
    UnitTestState{unit = TargetSynthesis{tMicroArch}} <- get
    lift $ putStrLn $ "BindVariables: " <> show (bindOptions tMicroArch)
    return ()

traceAvailableRefactor = do
    UnitTestState{unit = TargetSynthesis{tMicroArch = bus}} <- get
    lift $ putStrLn "Available refactor"
    lift $ putStrLn $ "  breakLoopOptions: " <> show (breakLoopOptions bus)
    lift $ putStrLn $ "  constantFoldingOptions : " <> show (constantFoldingOptions bus)
    lift $ putStrLn $ "  optimizeAccumOptions: " <> show (optimizeAccumOptions bus)
    lift $ putStrLn $ "  resolveDeadlockOptions: " <> show (resolveDeadlockOptions bus)
    return ()

-- | Get all loop function. Can be used as value to assertLoopBroken after auto synthesis.
getLoopFunctions = do
    UnitTestState{unit = TargetSynthesis{tMicroArch, tDFG}} <- get
    root <- lift $ getTreeUnit tMicroArch tDFG
    let loopFs = filter isLoop $ map (\(Bind f _) -> f) $ bindOptions root
    return loopFs

applyBreakLoop f = do
    st@UnitTestState{unit = ts@TargetSynthesis{tMicroArch}} <- get
    case find (\l@BreakLoop{} -> recLoop l == f) $ breakLoopOptions tMicroArch of
        Just refactor -> put st{unit = ts{tMicroArch = breakLoopDecision tMicroArch refactor}}
        Nothing -> lift $ assertFailure $ "Can't find refactor for such function: " <> show f

applyBreakLoops fs = mapM_ applyBreakLoop fs

-- TODO combine with assertSynthesisInclude??
assertLoopBroken [] = lift $ assertFailure "Can't check is loop broken for empty list!"
assertLoopBroken fs = do
    UnitTestState{unit = TargetSynthesis{tMicroArch = ma@BusNetwork{}}} <- get
    -- TODO add loop filter to check that there is no other func
    let fs' = S.fromList $ concatMap (concatBind . loopExtract) fs
    let cad = S.fromList $ getCADs $ process ma
    unless (S.isSubsetOf fs' cad) $
        lift $
            assertFailure $
                "Can't find refactor for such functions: "
                    <> show (S.difference fs' cad)
                    <> "\n in: "
                    <> show cad
    where
        loopExtract f
            | Just f_@(Loop _ (O ov) (I iv)) <- castF f = Just (f_, ov, iv)
            | otherwise = Nothing
        concatBind (Just (f, ov, iv)) = ["bind LoopBegin " <> label f <> " " <> concatMap label (S.elems ov), "bind LoopEnd " <> label f <> " " <> label iv]
        concatBind Nothing = []

applyConstantFolding fs = do
    st@UnitTestState{unit = ts@TargetSynthesis{tMicroArch}} <- get
    case find (\ConstantFolding{cRefOld} -> isSubsequenceOf fs cRefOld) $ constantFoldingOptions tMicroArch of
        Just refactor -> put st{unit = ts{tMicroArch = constantFoldingDecision tMicroArch refactor}}
        Nothing -> lift $ assertFailure $ "Can't find refactor for such function: " <> show fs

assertConstantFolded = undefined

applyOptimizeAccum fs = do
    st@UnitTestState{unit = ts@TargetSynthesis{tMicroArch}} <- get
    case find (\OptimizeAccum{refOld} -> isSubsequenceOf fs refOld) $ optimizeAccumOptions tMicroArch of
        Just refactor -> put st{unit = ts{tMicroArch = optimizeAccumDecision tMicroArch refactor}}
        Nothing -> lift $ assertFailure $ "Can't find refactor for such function: " <> show fs

assertOptimizeAccum = undefined
