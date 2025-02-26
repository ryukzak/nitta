{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Tests.DSL
Description : Provides functions to test PU, by making synthesis decisions
Copyright   : (c) Artyom Kostyuchik, 2022
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

= Module description

DSL (domain-specific language) is a module for testing Processor Units (PU) and
target systems.

= Examples

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

- 'NITTA.Tests.test_manual' -- Target system manual test.
- 'NITTA.Model.ProcessorUnits.Tests.DSL.Tests' -- many examples.
-}
module NITTA.Model.ProcessorUnits.Tests.DSL (
    unitTestCase,

    -- * Process Unit Prepare
    assign,
    assignNaive,
    setValue,
    setValues,

    -- * Process Unit Synthesis
    decide,
    decideAt,
    decideAtUnsafe,
    consume,
    provide,
    decideNaiveSynthesis,

    -- * Prepare target system
    setBusType,
    bind2network,
    setReceivedValues,
    setNetwork,
    assignLua,

    -- * Synthesize target system
    doBind,
    doTransfer,
    doAllocation,
    synthesis,

    -- * Refactors for PU and target system
    assertRefactor,
    refactorAvail,
    refactor,
    mkBreakLoop,
    mkConstantFolding,
    mkOptimizeAccum,
    mkOptimizeLut,
    mkResolveDeadlock,

    -- * Asserts for PU and target system
    mkAllocation,
    mkAllocationOptions,
    assertAllocation,
    assertAllocationOptions,
    assertPU,
    assertAllEndpointRoles,
    assertBindFullness,
    assertPUCoSimulation,
    assertTargetSystemCoSimulation,
    assertEndpoint,
    assertLocks,
    assertSynthesisDone,
    synthesizeAndCoSim,
    assertSynthesisComplete,

    -- * Trace (inspection for debug)
    traceBind,
    traceDataflow,
    traceEndpoints,
    traceFunctions,
    tracePU,
    traceProcess,
    traceRefactor,
    traceProcessWaves,
    traceAllocation,
    traceDataflowState,
) where

import Control.Monad.Identity
import Control.Monad.State.Lazy
import Data.CallStack
import Data.Default
import Data.List (find)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Proxy
import Data.Set qualified as S
import Data.String.Interpolate
import Data.String.ToString
import Data.String.Utils qualified as S
import Data.Text qualified as T
import Data.Typeable
import NITTA.Frontends.Lua
import NITTA.Intermediate.Analysis (buildProcessWaves)
import NITTA.Intermediate.DataFlow
import NITTA.Intermediate.Simulation
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Networks.Types (PUClasses)
import NITTA.Model.Networks.Types qualified as NT (PU (PU, unit))
import NITTA.Model.Problems
import NITTA.Model.ProcessIntegrity
import NITTA.Model.ProcessorUnits
import NITTA.Model.ProcessorUnits.Tests.Utils
import NITTA.Model.TargetSystem
import NITTA.Model.Tests.Internals
import NITTA.Project
import NITTA.Synthesis
import NITTA.Utils
import Numeric.Interval.NonEmpty hiding (elem)
import Prettyprinter (pretty)
import System.Directory
import System.FilePath
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

{- | Unit test state. Be aware internal implementation is not fully consistent
 and can be replaced by a type family with PU and target system instances.
-}
data UnitTestState u v x = UnitTestState
    { testName :: String
    , unit :: u
    -- ^ Unit model, should be a process unit or target system.
    , functs :: [F v x]
    -- ^ Contains functions assigned to PU.
    , cntxCycle :: [(v, x)]
    -- ^ Initial values for coSimulation
    , receivedValues :: [(v, [x])]
    -- ^ Values for IO immitation
    , busType :: Maybe (Proxy x)
    , report :: Either String (TestbenchReport v x)
    -- ^ Report on process unit test bench.
    }
    deriving (Show)

type Statement u v x r = HasCallStack => StateT (UnitTestState u v x) IO r

type PUStatement pu v x t r =
    (HasCallStack, ProcessorUnit pu v x t, ProcessIntegrity pu, BreakLoopProblem pu v x, EndpointProblem pu v t) =>
    StateT (UnitTestState pu v x) IO r

type TSStatement x r =
    forall tag v t.
    (HasCallStack, tag ~ T.Text, v ~ T.Text, t ~ Int, Val x) =>
    Statement (TargetSystem (BusNetwork tag v x t) tag v x t) v x r

unitTestCase ::
    HasCallStack =>
    String ->
    u ->
    StateT (UnitTestState u v x) IO () ->
    TestTree
unitTestCase name pu alg = testCase name $ do
    void $ evalUnitTestState (toModuleName name) pu alg

evalUnitTestState name st alg =
    evalStateT
        alg
        UnitTestState
            { testName = name
            , unit = st
            , functs = []
            , cntxCycle = []
            , receivedValues = []
            , busType = Nothing
            , report = Left "Report not ready!"
            }

-- | Binds provided function to PU
assign :: F v x -> PUStatement pu v x t ()
assign f = do
    st@UnitTestState{unit, functs} <- get
    case tryBind f unit of
        Right unit_ -> put st{unit = unit_, functs = f : functs}
        Left err -> lift $ assertFailure $ "assign: " <> err

-- | Store provided function and its initial values for naive coSimulation
assignNaive :: [(v, x)] -> F v x -> PUStatement pu v x t ()
assignNaive cntxs f = do
    st@UnitTestState{functs, cntxCycle} <- get
    put st{functs = f : functs, cntxCycle = cntxs <> cntxCycle}

-- | set initital values for coSimulation input variables
setValues :: (Function f v, WithFunctions pu f) => [(v, x)] -> PUStatement pu v x t ()
setValues vxs = mapM_ (\(v, x) -> setValue v x) vxs

-- | set initital value for coSimulation input variables
setValue :: (Var v, Function f v, WithFunctions pu f) => v -> x -> PUStatement pu v x t ()
setValue var val = do
    pu@UnitTestState{cntxCycle, unit} <- get
    when (var `elem` map fst cntxCycle) $
        lift $
            assertFailure $
                "The variable '" <> toString var <> "' is already set!"
    unless (isVarAvailable var unit) $
        lift $
            assertFailure $
                "It's not possible to set the variable '" <> toString var <> "'! It's not present in process"
    put pu{cntxCycle = (var, val) : cntxCycle}
    where
        isVarAvailable v pu = S.isSubsetOf (S.fromList [v]) $ unionsMap inputs $ functions pu

-- | Make synthesis decision with provided Endpoint Role and automatically assigned time
decide :: EndpointRole v -> PUStatement pu v x t ()
decide role = do
    des <- epAt <$> getDecisionSpecific role
    doDecision False $ EndpointSt role des

-- | Make synthesis decision with provided Endpoint Role and manually selected interval
decideAt :: t -> t -> EndpointRole v -> PUStatement pu v x t ()
decideAt from to role = doDecision False $ EndpointSt role (from ... to)

decideAtUnsafe :: t -> t -> EndpointRole v -> PUStatement pu v x t ()
decideAtUnsafe from to role = doDecision True $ EndpointSt role (from ... to)

doDecision :: Bool -> EndpointSt v (Interval t) -> PUStatement pu v x t ()
doDecision unsafe endpSt = do
    st@UnitTestState{unit} <- get
    let isAvailable = isEpOptionAvailable endpSt unit
    if unsafe || isAvailable
        then put st{unit = endpointDecision unit endpSt}
        else lift $ assertFailure $ "doDecision: such option isn't available: " <> show endpSt <> " from " <> show (endpointOptions unit)

-- | Bind all functions to processor unit and decide till decisions left.
decideNaiveSynthesis :: PUStatement pu v x t ()
decideNaiveSynthesis = do
    st@UnitTestState{unit, functs} <- get
    when (null functs) $
        lift $
            assertFailure "You should assign function to do naive synthesis!"
    put st{unit = naiveSynthesis functs unit}

-- | Transforms provided variable to Target
consume = Target

-- | Transforms provided variables to Source
provide = Source . S.fromList

getDecisionSpecific role = do
    let s = variables role
    des <- getDecisionsFromEp
    case find (\case EndpointSt{epRole} | S.isSubsetOf s $ variables epRole -> True; _ -> False) des of
        Just v -> return $ endpointOptionToDecision v
        Nothing -> lift $ assertFailure $ "Can't provide decision with variable: " <> show (vsToStringList s)

getDecisionsFromEp = do
    UnitTestState{unit} <- get
    case endpointOptions unit of
        [] -> lift $ assertFailure "Failed during decision making: there is no decisions left!"
        opts -> return opts

isEpOptionAvailable EndpointSt{epRole = role, epAt = atA} pu =
    case find (isSubroleOf role . epRole) $ endpointOptions pu of
        Nothing -> False
        Just EndpointSt{epAt = atB} ->
            atA `isSubsetOf` tcAvailable atB
                && member (width atA + 1) (tcDuration atB)

assertBindFullness :: (Function f v, WithFunctions pu f, Show f) => PUStatement pu v x t ()
assertBindFullness = do
    UnitTestState{unit, functs} <- get
    isOk <- lift $ isFullyBound unit functs
    unless isOk $
        lift $
            assertFailure $
                "Function is not bound to process! expected: " ++ concatMap show functs ++ "; actual: " ++ concatMap show (functions unit)
    where
        isFullyBound pu fs = do
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

assertAllEndpointRoles :: Var v => [EndpointRole v] -> PUStatement pu v x t ()
assertAllEndpointRoles roles = do
    UnitTestState{unit} <- get
    let opts = S.fromList $ map epRole $ endpointOptions unit
    lift $ assertBool ("Actual endpoint roles: " <> show opts) $ opts == S.fromList roles

assertEndpoint :: t -> t -> EndpointRole v -> PUStatement pu v x t ()
assertEndpoint a b role = do
    UnitTestState{unit} <- get
    let opts = endpointOptions unit
        ep = EndpointSt{epAt = a ... b, epRole = role}
    case find (\EndpointSt{epAt, epRole} -> tcAvailable epAt == (a ... b) && epRole == role) opts of
        Nothing -> lift $ assertFailure $ "assertEndpoint: '" <> show ep <> "' not defined in: " <> show opts
        Just _ -> return ()

assertLocks :: Locks pu v => [Lock v] -> PUStatement pu v x t ()
assertLocks expectLocks = do
    UnitTestState{unit} <- get
    let actualLocks0 = locks unit
        actualLocks = S.fromList actualLocks0
    lift $ assertBool ("assertLocks: locks contain duplicates: " <> show actualLocks0) $ length actualLocks0 == S.size actualLocks
    lift $ assertBool ("assertLocks:\n  expected locks:\n" <> show' expectLocks <> "\n  actual:\n" <> show' actualLocks0) $ actualLocks == S.fromList expectLocks
    where
        show' ls = S.join "\n" $ map (("    " <>) . show) ls

assertSynthesisDone :: PUStatement pu v x t ()
assertSynthesisDone = do
    UnitTestState{unit, functs, testName} <- get
    unless (null $ endpointOptions unit) $
        lift $
            assertFailure $
                "In ''" <> testName <> "'' process still has endpoint options:\n" <> show (pretty $ process unit)
    unless (isProcessComplete unit functs) $
        lift $
            assertFailure $
                "In ''" <> testName <> "'' process is incomplete.\nAlgorithm: " <> show functs <> "\nProcess:\n" <> show (pretty $ process unit)

    case checkProcessIntegrity unit of
        Left err -> lift $ assertFailure $ testName <> " broken process: " <> err
        Right () -> return ()

assertPUCoSimulation ::
    ( PUClasses pu v x Int
    , WithFunctions pu (F v x)
    , Testable pu v x
    , DefaultX pu x
    , Var v
    ) =>
    PUStatement pu v x Int ()
assertPUCoSimulation = do
    UnitTestState{unit, functs, testName, cntxCycle} <- get
    unless (checkInputVars unit functs cntxCycle) $
        lift $
            assertFailure "you forgot to set initial values before coSimulation."

    report@TestbenchReport{tbStatus} <-
        lift $ puCoSim testName unit cntxCycle functs False

    unless tbStatus $
        lift $
            assertFailure $
                "coSimulation failed: \n" <> show report
    where
        checkInputVars pu fs cntx =
            let requiredVars =
                    S.union
                        (unionsMap inputs $ functions pu)
                        (unionsMap inputs fs)
                initialCntx = S.fromList (map fst cntx)
             in requiredVars == initialCntx

assignLua :: T.Text -> TSStatement x ()
assignLua src = do
    assertEmptyDataFlow
    st@UnitTestState{unit = TargetSystem{mUnit}} <- get
    let dfg = frDataFlow $ translateLua src
    put
        st
            { unit = mkModelWithOneNetwork mUnit dfg
            , functs = functions dfg
            }

-- | Bind function to network (except 'doBind' after that)
bind2network :: F T.Text x -> TSStatement x ()
bind2network f = do
    st@UnitTestState{functs, unit = ts@TargetSystem{mUnit, mDataFlowGraph}} <- get
    put
        st
            { unit =
                ts
                    { mDataFlowGraph = addFuncToDataFlowGraph f mDataFlowGraph
                    , mUnit = bind f mUnit
                    }
            , functs = f : functs
            }

setNetwork :: BusNetwork T.Text T.Text x Int -> TSStatement x ()
setNetwork network = do
    st@UnitTestState{unit} <- get
    assertEmptyDataFlow
    put st{unit = unit{mUnit = network}}

setBusType :: Proxy x -> TSStatement x ()
setBusType busType = modify' $ \st -> st{busType = Just busType}

-- | Set data for IO
setReceivedValues :: [(T.Text, [x])] -> TSStatement x ()
setReceivedValues values = do
    st@UnitTestState{receivedValues} <- get
    unless (null receivedValues) $
        lift $
            assertFailure "setReceivedValues: already setted"
    put st{receivedValues = values}

synthesis :: SynthesisMethod T.Text T.Text x Int -> TSStatement x ()
synthesis method = do
    st@UnitTestState{unit} <- get
    leaf <- lift $ do
        root <- synthesisTreeRootIO unit
        method root
    put st{unit = sTarget $ sState leaf}

doBind :: T.Text -> F T.Text x -> TSStatement x ()
doBind uTag f = do
    st@UnitTestState{unit = ts} <- get
    let d = SingleBind uTag f
        opts = bindOptions ts
    unless (d `L.elem` opts) $
        lift $
            assertFailure $
                "bind not available: " <> show d <> " in: " <> show opts
    put st{unit = bindDecision ts d}

doTransfer :: [T.Text] -> TSStatement x ()
doTransfer vs = do
    st@UnitTestState{unit = ts} <- get
    let opts = dataflowOptions ts
    case L.find (\o -> S.fromList vs == variables o) opts of
        Just o -> put st{unit = dataflowDecision ts $ dataflowOption2decision o}
        Nothing -> lift $ assertFailure $ "can't find transfer for: " <> show vs <> " in: " <> show opts

doAllocation :: T.Text -> T.Text -> TSStatement x ()
doAllocation networkTag processUnitTag = do
    st@UnitTestState{unit = ts} <- get
    let d = Allocation{networkTag = networkTag, processUnitTag = processUnitTag}
        opts = allocationOptions ts
    unless (d `L.elem` opts) $
        lift $
            assertFailure $
                "allocation not available: " <> show d <> " in: " <> show opts
    put st{unit = allocationDecision ts d}

class Refactor u ref where
    refactorAvail :: ref -> Statement u v x ()
    refactor :: ref -> Statement u v x ()

refactorAvail' ref options = do
    UnitTestState{unit} <- get
    unless (ref `L.elem` options unit) $
        lift $
            assertFailure "refactorAvail: required refactor not present in option"

refactor' ref options decision = do
    st@UnitTestState{unit} <- get
    unless (ref `L.elem` options unit) $
        lift $
            assertFailure $
                "refactor: required refactor not present in option: " <> show ref
    put st{unit = decision unit ref}

instance (Var v, Val x, BreakLoopProblem u v x) => Refactor u (BreakLoop v x) where
    refactorAvail ref = refactorAvail' ref breakLoopOptions
    refactor ref = refactor' ref breakLoopOptions breakLoopDecision

mkBreakLoop :: (Var v, Val x) => x -> v -> [v] -> Statement u v x (BreakLoop v x)
mkBreakLoop x input output = return $ BreakLoop x (S.fromList output) input

instance (Var v, Val x, OptimizeAccumProblem u v x) => Refactor u (OptimizeAccum v x) where
    refactorAvail ref = refactorAvail' ref optimizeAccumOptions
    refactor ref = refactor' ref optimizeAccumOptions optimizeAccumDecision

mkOptimizeAccum :: (Var v, Val x) => [F v x] -> [F v x] -> Statement u v x (OptimizeAccum v x)
mkOptimizeAccum old new = return $ OptimizeAccum old new

instance (Var v, Val x, OptimizeLutProblem u v x) => Refactor u (OptimizeLut v x) where
    refactorAvail ref = refactorAvail' ref optimizeLutOptions
    refactor ref = refactor' ref optimizeLutOptions optimizeLutDecision

mkOptimizeLut :: (Var v, Val x) => [F v x] -> [F v x] -> Statement u v x (OptimizeLut v x)
mkOptimizeLut old new = return $ OptimizeLut old new

instance (Var v, Val x, ResolveDeadlockProblem u v x) => Refactor u (ResolveDeadlock v x) where
    refactorAvail ref = refactorAvail' ref resolveDeadlockOptions
    refactor ref = refactor' ref resolveDeadlockOptions resolveDeadlockDecision

mkResolveDeadlock :: (Var v, Val x) => [v] -> Statement u v x (ResolveDeadlock v x)
mkResolveDeadlock vs = return $ resolveDeadlock (S.fromList vs)

instance (Var v, Val x, ConstantFoldingProblem u v x) => Refactor u (ConstantFolding v x) where
    refactorAvail ref = refactorAvail' ref constantFoldingOptions
    refactor ref = refactor' ref constantFoldingOptions constantFoldingDecision

mkConstantFolding :: (Var v, Val x) => [F v x] -> [F v x] -> Statement u v x (ConstantFolding v x)
mkConstantFolding old new = return $ ConstantFolding old new

assertRefactor :: (Typeable ref, Eq ref, Show ref) => ref -> TSStatement x ()
assertRefactor ref = do
    refactors <- filter isRefactorStep . map (descent . pDesc) . steps . process . unit <$> get
    case L.find
        ( \case
            (RefactorStep r) -> Just ref == cast r
            _ -> error "assertRefactor: impossible"
        )
        refactors of
        Nothing -> lift $ assertFailure $ "Refactor not present: " <> show ref <> " in " <> show refactors
        Just _ -> return ()

assertEmptyDataFlow :: TSStatement x ()
assertEmptyDataFlow = do
    UnitTestState{unit = TargetSystem{mDataFlowGraph}} <- get
    unless (null $ functions mDataFlowGraph) $
        error "assertEmptyDataFlow: dataflow should be empty"

mkAllocation :: T.Text -> T.Text -> Statement u v x (Allocation T.Text)
mkAllocation networkTag processUnitTag = return Allocation{networkTag, processUnitTag}

mkAllocationOptions :: (Typeable tag, UnitTag tag) => tag -> [tag] -> Statement u v x [Allocation tag]
mkAllocationOptions networkTag puTags =
    return $ map (\processUnitTag -> Allocation{networkTag, processUnitTag}) puTags

assertAllocation :: (Typeable a, Eq a, Show a) => Int -> a -> TSStatement x ()
assertAllocation number alloc = do
    allocations <- filter isAllocationStep . map (descent . pDesc) . steps . process . unit <$> get
    let matched =
            length $
                filter
                    ( \case
                        (AllocationStep a) -> Just alloc == cast a
                        _ -> error "assertAllocation: internal error"
                    )
                    allocations
    when (matched /= number) $
        lift $
            assertFailure
                [__i|
                    Number of allocations does not match the expected
                    expected: #{ show number }
                    actual: #{ show matched }
                    allocation: #{ show alloc }
                    steps:
                    #{ showArray allocations }
                    |]

-- | Asserts that allocation options that provides target system are equals to specified options
assertAllocationOptions :: [Allocation T.Text] -> TSStatement x ()
assertAllocationOptions options = do
    UnitTestState{unit = TargetSystem{mUnit}} <- get
    let actual = allocationOptions mUnit
    when (options /= actual) $
        lift $
            assertFailure
                [__i|
                    Allocation options doesn't match expected
                    expected:
                    #{ showArray options }
                    actual:
                    #{ showArray actual }
                    |]

assertPU :: Typeable a => T.Text -> Proxy a -> TSStatement x ()
assertPU tag puProxy = do
    UnitTestState{unit = TargetSystem{mUnit}} <- get
    let pu = M.lookup tag $ bnPus mUnit
        sameType NT.PU{NT.unit} = typeOf unit == typeRep puProxy
    case pu of
        Nothing ->
            lift . assertFailure $
                [__i|
                    The BusNetwork was expected to have a PU with a specific tag (#{tag})
                    Existing PUs: #{ show $ M.keys $ bnPus mUnit }
                    |]
        Just p ->
            unless (sameType p) . lift . assertFailure $
                "It was expected that the type of PU would be" <> show (typeRep puProxy) <> "but found" <> show (typeOf p)

assertSynthesisComplete :: TSStatement x ()
assertSynthesisComplete = do
    UnitTestState{unit = unit@TargetSystem{mUnit, mDataFlowGraph}} <- get
    unless (isSynthesisComplete unit) $
        lift $
            assertFailure $
                "synthesis is not complete: " <> show (variables mDataFlowGraph `S.difference` transferred mUnit)

assertTargetSystemCoSimulation :: TSStatement x ()
assertTargetSystemCoSimulation = do
    UnitTestState{unit = TargetSystem{mUnit, mDataFlowGraph}, testName, receivedValues} <- get
    report <- lift $ do
        pInProjectNittaPath <- either (error . T.unpack) id <$> collectNittaPath defProjectTemplates
        pwd <- getCurrentDirectory
        pName <- uniqTestPath testName
        let outputPath = "gen"
            loopsNumber = 5
            prj =
                Project
                    { pName = T.pack pName
                    , pLibPath = "hdl"
                    , pTargetProjectPath = outputPath </> pName
                    , pAbsTargetProjectPath = pwd </> outputPath </> pName
                    , pInProjectNittaPath
                    , pAbsNittaPath = pwd </> outputPath </> pInProjectNittaPath
                    , pUnit = mUnit
                    , pUnitEnv = bnEnv mUnit
                    , pTestCntx = simulateDataFlowGraph loopsNumber def receivedValues mDataFlowGraph
                    , pTemplates = defProjectTemplates
                    }
        writeProject prj
        runTestbench prj
    assertSuccessReport report

assertSuccessReport :: TestbenchReport T.Text x -> TSStatement x ()
assertSuccessReport report@TestbenchReport{tbStatus} =
    lift $ assertBool ("report with bad status:\n" <> show report) tbStatus

synthesizeAndCoSim :: TSStatement x ()
synthesizeAndCoSim = do
    synthesis $ stateOfTheArtSynthesisIO def
    assertSynthesisComplete
    assertTargetSystemCoSimulation

tracePU :: Show pu => PUStatement pu v x t ()
tracePU = do
    UnitTestState{unit} <- get
    lift $ putStrLn $ "PU: " <> show unit

traceFunctions :: Statement u v x ()
traceFunctions = do
    UnitTestState{functs} <- get
    lift $ putListLn "Functions: " functs

traceEndpoints :: PUStatement pu v x t ()
traceEndpoints = do
    UnitTestState{unit} <- get
    lift $ putListLn "Endpoints: " $ endpointOptions unit

traceProcess :: ProcessorUnit u v x Int => Statement u v x ()
traceProcess = do
    UnitTestState{unit} <- get
    lift $ putStrLn $ "Process: " <> show (pretty $ process unit)

traceDataflowState :: TSStatement x ()
traceDataflowState = do
    UnitTestState{unit = TargetSystem{mDataFlowGraph}} <- get
    lift $
        do
            putStrLn "DataFlowGraph: "
            print mDataFlowGraph

traceDataflow :: TSStatement x ()
traceDataflow = do
    UnitTestState{unit = TargetSystem{mUnit}} <- get
    lift $ putListLn "Dataflow: " $ dataflowOptions mUnit

traceProcessWaves :: TSStatement x ()
traceProcessWaves = do
    UnitTestState{unit = TargetSystem{mDataFlowGraph}} <- get
    lift $ putStrLn $ showArray $ buildProcessWaves [] $ functions mDataFlowGraph

traceBind :: TSStatement x ()
traceBind = do
    UnitTestState{unit = TargetSystem{mUnit}} <- get
    lift $ putListLn "Bind: " $ bindOptions mUnit

traceAllocation :: TSStatement x ()
traceAllocation = do
    UnitTestState{unit = TargetSystem{mUnit}} <- get
    lift $ putListLn "Allocation: " $ allocationOptions mUnit

traceRefactor :: TSStatement x ()
traceRefactor = do
    UnitTestState{unit = TargetSystem{mUnit}} <- get
    lift $ putListLn "breakLoopOptions: " $ breakLoopOptions mUnit
    lift $ putListLn "constantFoldingOptions: " $ constantFoldingOptions mUnit
    lift $ putListLn "optimizeAccumOptions: " $ optimizeAccumOptions mUnit
    lift $ putListLn "optimizeLutOptions: " $ optimizeLutOptions mUnit
    lift $ putListLn "resolveDeadlockOptions: " $ resolveDeadlockOptions mUnit

putListLn name opts = do
    putStrLn name
    mapM_ (\b -> putStrLn $ "- " <> show b) opts

showArray l = S.join "\n\t" (map show l)
