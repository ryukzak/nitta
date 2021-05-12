{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- TODO: A promising direction for the improvement is the implementation of the
-- accumulator into it. It allows multiplying an arbitrary number of arguments,
-- which will reduce the number of data transactions on the bus when multiplying
-- more than two variables by one function.

-- TODO: Add assertion, which checks that all synthesis decision compliant
-- available options.

{- |
Module      : NITTA.Model.ProcessorUnits.Multiplier
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

= Processor unit

A processor unit (PU) can be used for:

- data storage and processing;
- interaction with the periphery (IO);
- control of a NITTA processor (special case).

There are characterized by complicated behavior with:

- multifunctionality;
- internal parallelism;
- superscalar;
- pipelining;
- availability of internal resources.

The multiplier PU is one of the simplest processors because it realizes only
one function with sequence evaluation
('NITTA.Intermediate.Functions.Multiply'). Processor behavior in a specific
application is determined by the applied algorithm
('NITTA.Intermediate.DataFlow').

Any PU may include three components:

- hardware - set of prepared or automatically generated hardware descriptions
  (@\/hdl\/multiplier@);

- software - set of binary files that determine:

    - initial state and setting ;

    - a control program;

- PU model - CAD component that implements PU support (hardware and software
  generation, instance generation, computation process scheduling, testing
  environment, etc.).

All three components are hardly related to each other and needed to comply
with each other strictly. For a deeper understanding, a PU developer should
understand all of its components. The multiplier model will be described
above.

== Processor unit model

A model purpose is "teaching" CAD how to work with the PU:

- Which functions can be evaluated by PU? (see
  'NITTA.Model.ProcessorUnits.Types.ProcessorUnit')?

- How to control PU for evaluating specific functions (see
  'NITTA.Model.ProcessorUnits.Types.ProcessorUnit',
  'NITTA.Model.ProcessorUnits.Types.Controllable')?

- How to translating instructions to microcode (see
  'NITTA.Model.ProcessorUnits.Types.UnambiguouslyDecode')?

- What are the options of PU synthesis decision available? (see
  'NITTA.Model.Problems.Types.ProcessorUnit',
  'NITTA.Model.Problems.Types.EndpointDT'):

    - push variable to the PU ('NITTA.Model.Problems.Endpoint.Target');

    - pull at least one variable from the PU ('NITTA.Model.Problems.Endpoint.Source').

The basis of a PU model is a data structure that represents:

- PU state during computation process scheduling;

- process description (full or fragment), which can be translated to microcode.

Exactly around this data structure, all algorithmic part of the PU model is
developed. The types of the following variables parametrize the data structure:

- @v@ - variable id (usually 'String');

- @x@ - a type of processed value (see 'NITTA.Intermediate.Value.Val');

- @t@ - time moment id (usually 'Int').

= Multiplier processor unit

The multiplier processor unit can evaluate the following function type:

- 'NITTA.Intermediate.Functions.Multiply'.

Only one function can be processed in one moment, and its execution cannot be
interrupted.

This module should be considered as a tutorial for the development of other
models of processor units. Its source code is written almost in literature
style, so we recommend to continue reading within the source code.

== Interaction with multiplier processor unit

We will consider the example of the computation process scheduling for one
function. To do this, we need to start GHCi interpreter by executing `stack
repl` command from the project directory. After that:

@
> :l NITTA.Model.ProcessorUnits.Multiplier
[ 1 of 30] Compiling NITTA.Intermediate.Value ( UserspenskoiDocumentsnitta-corpnittasrcNITTAIntermediate/Value.hs, interpreted )
[ 2 of 30] Compiling NITTA.Intermediate.Variable ( UserspenskoiDocumentsnitta-corpnittasrcNITTAIntermediate/Variable.hs, interpreted )
[ 3 of 30] Compiling NITTA.Intermediate.Types ( UserspenskoiDocumentsnitta-corpnittasrcNITTAIntermediate/Types.hs, interpreted )
[ 4 of 30] Compiling NITTA.Model.Problems.Binding ( UserspenskoiDocumentsnitta-corpnittasrcNITTAModelProblemsBinding.hs, interpreted )
[ 5 of 30] Compiling NITTA.Model.Types ( UserspenskoiDocumentsnitta-corpnittasrcNITTAModel/Types.hs, interpreted )
[ 6 of 30] Compiling NITTA.Model.Problems.Endpoint ( UserspenskoiDocumentsnitta-corpnittasrcNITTAModelProblemsEndpoint.hs, interpreted )
[ 7 of 30] Compiling NITTA.Model.Problems.Dataflow ( UserspenskoiDocumentsnitta-corpnittasrcNITTAModelProblemsDataflow.hs, interpreted )
[ 8 of 30] Compiling NITTA.Project.Types ( UserspenskoiDocumentsnitta-corpnittasrcNITTAProject/Types.hs, interpreted )
[ 9 of 30] Compiling NITTA.Utils.Base ( UserspenskoiDocumentsnitta-corpnittasrcNITTAUtils/Base.hs, interpreted )
[10 of 30] Compiling NITTA.Intermediate.Functions.Accum ( UserspenskoiDocumentsnitta-corpnittasrcNITTAIntermediateFunctionsAccum.hs, interpreted )
[11 of 30] Compiling NITTA.Intermediate.Functions ( UserspenskoiDocumentsnitta-corpnittasrcNITTAIntermediate/Functions.hs, interpreted )
[12 of 30] Compiling NITTA.Model.Problems.Refactor ( UserspenskoiDocumentsnitta-corpnittasrcNITTAModelProblemsRefactor.hs, interpreted )
[13 of 30] Compiling NITTA.Model.Problems.Whole ( UserspenskoiDocumentsnitta-corpnittasrcNITTAModelProblemsWhole.hs, interpreted )
[14 of 30] Compiling NITTA.Model.Problems ( UserspenskoiDocumentsnitta-corpnittasrcNITTAModel/Problems.hs, interpreted )
[15 of 30] Compiling NITTA.Utils.CodeFormat ( UserspenskoiDocumentsnitta-corpnittasrcNITTAUtils/CodeFormat.hs, interpreted )
[16 of 30] Compiling NITTA.Model.ProcessorUnits.Types ( UserspenskoiDocumentsnitta-corpnittasrcNITTAModelProcessorUnitsTypes.hs, interpreted )
[17 of 30] Compiling NITTA.Utils      ( UserspenskoiDocumentsnitta-corpnittasrcNITTAUtils.hs, interpreted )
[18 of 30] Compiling NITTA.Project.VerilogSnippets ( UserspenskoiDocumentsnitta-corpnittasrcNITTAProject/Snippets.hs, interpreted )
[19 of 30] Compiling NITTA.Project.Implementation ( UserspenskoiDocumentsnitta-corpnittasrcNITTAProject/Implementation.hs, interpreted )
[20 of 30] Compiling NITTA.Project.Parts.Utils ( UserspenskoiDocumentsnitta-corpnittasrcNITTAProjectPartsUtils.hs, interpreted )
[21 of 30] Compiling NITTA.Project.TestBench ( UserspenskoiDocumentsnitta-corpnittasrcNITTAProjectPartsTestBench.hs, interpreted )
[22 of 30] Compiling NITTA.Project.Parts.TargetSystem ( UserspenskoiDocumentsnitta-corpnittasrcNITTAProjectPartsTargetSystem.hs, interpreted )
[23 of 30] Compiling NITTA.Project.Parts.Icarus ( UserspenskoiDocumentsnitta-corpnittasrcNITTAProjectPartsIcarus.hs, interpreted )
[24 of 30] Compiling NITTA.Model.Networks.Types ( UserspenskoiDocumentsnitta-corpnittasrcNITTAModelNetworksTypes.hs, interpreted )
[25 of 30] Compiling NITTA.Utils.ProcessDescription ( UserspenskoiDocumentsnitta-corpnittasrcNITTAUtils/ProcessDescription.hs, interpreted )
[26 of 30] Compiling NITTA.Model.Networks.Bus ( UserspenskoiDocumentsnitta-corpnittasrcNITTAModelNetworksBus.hs, interpreted )
[27 of 30] Compiling NITTA.Project.Parts.Quartus ( UserspenskoiDocumentsnitta-corpnittasrcNITTAProjectPartsQuartus.hs, interpreted )
[28 of 30] Compiling NITTA.Project.Utils ( UserspenskoiDocumentsnitta-corpnittasrcNITTAProject/Utils.hs, interpreted )
[29 of 30] Compiling NITTA.Project    ( UserspenskoiDocumentsnitta-corpnittasrcNITTAProject.hs, interpreted )
[30 of 30] Compiling NITTA.Model.ProcessorUnits.Multiplier ( UserspenskoiDocumentsnitta-corpnittasrcNITTAModelProcessorUnitsMultiplier.hs, interpreted )
Ok, 30 modules loaded.
> :module +NITTA.Model.Types NITTA.Intermediate.Functions Numeric.Interval.NonEmpty Data.Set
> :set prompt "ESC[34mλ> ESC[m"
@

Now create the function and multiplier model initial state. Unfortunately, it
is not enough information for GHC deduction of its type, so let's define its
implicitly.

>>> let f = F.multiply "a" "b" ["c", "d"] :: F String Int
>>> f
a * b = c = d
>>> let st0 = multiplier True :: Multiplier String Int Int
>>> st0
Multiplier {remain = [], targets = [], sources = [], currentWork = Nothing, process_ = Process
    steps     =
<BLANKLINE>
    relations =
<BLANKLINE>
    nextTick  = 0
    nextUid   = 0
, isMocked = True}
>>> endpointOptions st0
[]

Bind a function to the multiplier unit. This operation could be executed at
any time of working with a model, including when a computation process is
fully scheduled (new work can be added). The main rules are: 1) if work is
fully scheduled, then it is necessary to perform it, and any part of it
cannot be "lost" inside the model; 2) if a unit has its internal resources,
there should be enough to finish schedule, even it is inefficient.

>>> let Right st1 = tryBind f st0
>>> st1
Multiplier {remain = [a * b = c = d], targets = [], sources = [], currentWork = Nothing, process_ = Process
    steps     =
<BLANKLINE>
    relations =
<BLANKLINE>
    nextTick  = 0
    nextUid   = 0
, isMocked = True}
>>> endpointOptions st1
[?Target "a"@(1..∞ /P 1..∞),?Target "b"@(1..∞ /P 1..∞)]

As we can see, after binding, we have two different options of computational
process scheduling that match different argument loading sequences: @a@ or
@b@. We can see that they are similar from an execution sequence point of
view: loading can be started from 0 tick or after an arbitrary delay; for
loading of one argument needed only one tick, but it can continue for an
arbitrary time. Choose the variant.

>>> let st2 = endpointDecision st1 $ EndpointSt (Target "a") (0...2)
>>> st2
Multiplier {remain = [], targets = ["b"], sources = ["c","d"], currentWork = Just a * b = c = d, process_ = Process
    steps     =
        0) Step {pID = 0, pInterval = 0 ... 2, pDesc = Endpoint: Target a}
        1) Step {pID = 1, pInterval = 0 ... 2, pDesc = Instruction: Load A}
    relations =
        0) Vertical 0 1
    nextTick  = 2
    nextUid   = 2
, isMocked = True}
>>> mapM_ print $ endpointOptions st2
?Target "b"@(3..∞ /P 1..∞)
>>> let st3 = endpointDecision st2 $ EndpointSt (Target "b") (3...3)
>>> st3
Multiplier {remain = [], targets = [], sources = ["c","d"], currentWork = Just a * b = c = d, process_ = Process
    steps     =
        0) Step {pID = 0, pInterval = 0 ... 2, pDesc = Endpoint: Target a}
        1) Step {pID = 1, pInterval = 0 ... 2, pDesc = Instruction: Load A}
        2) Step {pID = 2, pInterval = 3 ... 3, pDesc = Endpoint: Target b}
        3) Step {pID = 3, pInterval = 3 ... 3, pDesc = Instruction: Load B}
    relations =
        0) Vertical 2 3
        1) Vertical 0 1
    nextTick  = 3
    nextUid   = 4
, isMocked = True}
>>> mapM_ print $ endpointOptions st3
?Source "c","d"@(6..∞ /P 1..∞)

After loading both arguments, we can see that the next option is unloading
@c@ and @d@ variables. Note, these variables can be unloaded either
concurrently or sequentially (for details, see how the multiplier works
inside). Consider the second option:

>>> let st4 = endpointDecision st3 $ EndpointSt (Source $ S.fromList ["c"]) (6...6)
>>> st4
Multiplier {remain = [], targets = [], sources = ["d"], currentWork = Just a * b = c = d, process_ = Process
    steps     =
        0) Step {pID = 0, pInterval = 0 ... 2, pDesc = Endpoint: Target a}
        1) Step {pID = 1, pInterval = 0 ... 2, pDesc = Instruction: Load A}
        2) Step {pID = 2, pInterval = 3 ... 3, pDesc = Endpoint: Target b}
        3) Step {pID = 3, pInterval = 3 ... 3, pDesc = Instruction: Load B}
        4) Step {pID = 4, pInterval = 6 ... 6, pDesc = Endpoint: Source c}
        5) Step {pID = 5, pInterval = 6 ... 6, pDesc = Instruction: Out}
    relations =
        0) Vertical 4 5
        1) Vertical 2 3
        2) Vertical 0 1
    nextTick  = 6
    nextUid   = 6
, isMocked = True}
>>> mapM_ print $ endpointOptions st4
?Source "d"@(7..∞ /P 1..∞)
>>> let st5 = endpointDecision st4 $ EndpointSt (Source $ S.fromList ["d"]) (7...7)
>>> st5
Multiplier {remain = [], targets = [], sources = [], currentWork = Nothing, process_ = Process
    steps     =
        0) Step {pID = 0, pInterval = 0 ... 2, pDesc = Endpoint: Target a}
        1) Step {pID = 1, pInterval = 0 ... 2, pDesc = Instruction: Load A}
        2) Step {pID = 2, pInterval = 3 ... 3, pDesc = Endpoint: Target b}
        3) Step {pID = 3, pInterval = 3 ... 3, pDesc = Instruction: Load B}
        4) Step {pID = 4, pInterval = 6 ... 6, pDesc = Endpoint: Source c}
        5) Step {pID = 5, pInterval = 6 ... 6, pDesc = Instruction: Out}
        6) Step {pID = 6, pInterval = 7 ... 7, pDesc = Endpoint: Source d}
        7) Step {pID = 7, pInterval = 7 ... 7, pDesc = Instruction: Out}
        8) Step {pID = 8, pInterval = 0 ... 7, pDesc = Intermediate: a * b = c = d}
    relations =
        0) Vertical 8 6
        1) Vertical 8 4
        2) Vertical 8 2
        3) Vertical 8 0
        4) Vertical 6 7
        5) Vertical 4 5
        6) Vertical 2 3
        7) Vertical 0 1
    nextTick  = 7
    nextUid   = 9
, isMocked = True}
>>> endpointOptions st5
[]

All options of computing process scheduling are run out. All bound functions
are planned. Further microcode can be generated, which can be organizing the
described computational process on the multiplier.
-}
module NITTA.Model.ProcessorUnits.Multiplier (
    multiplier,
    Multiplier,
    Ports (..),
    IOPorts (..),
) where

import Control.Monad (when)
import Data.Default
import Data.List (find, partition, (\\))
import Data.Maybe
import qualified Data.Set as S
import Data.String.Interpolate
import Data.String.ToString
import qualified NITTA.Intermediate.Functions as F
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Types
import NITTA.Project
import NITTA.Utils
import NITTA.Utils.ProcessDescription
import Numeric.Interval.NonEmpty (inf, sup, (...))

{- |It is a PU model state representation, which describes each state of
synthesis model for that PU.
-}
data Multiplier v x t = Multiplier
    { -- |List of the assigned but not processed functions. To execute a
      -- function:
      --
      -- - removing the function from this list;
      --
      -- - transfering information from function to 'targets' and 'sources'
      --   fields.
      --
      -- An assigned function can be executed in random order.
      remain :: [F v x]
    , -- |List of variables, which is needed to push to the PU for current
      -- function evaluation.
      targets :: [v]
    , -- |List of variables, which is needed to pull from PU for current
      -- function evaluation. Pull order is arbitrary. All pulled variables
      -- correspond to the same value (same result).
      sources :: [v]
    , -- |Current work, if some function is executed.
      currentWork :: Maybe (F v x)
    , -- |Description of scheduled computation process
      -- ('NITTA.Model.ProcessorUnits.Types').
      process_ :: Process t (StepInfo v x t)
    , -- |HDL implementation of PU contains a multiplier IP core from Altera.
      -- Icarus Verilog can not simulate it. If `isMocked` is set, a target
      -- system will be contained non-synthesizable implementation of that
      -- IP-core.
      isMocked :: Bool
    }

deriving instance (VarValTime v x t) => Show (Multiplier v x t)

{- | Multiplier PU model constructor. Argument defines the computation unit's
internal organization: using multiplier IP kernel (False) or mock (True). For
more information, look hardware function in 'TargetSystemComponent' class.
-}
multiplier mock =
    Multiplier
        { remain = []
        , targets = []
        , sources = []
        , currentWork = Nothing
        , process_ = def
        , isMocked = mock
        }

-- |Default initial state of multiplier PU model.
instance (Time t) => Default (Multiplier v x t) where
    def = multiplier True

instance Default x => DefaultX (Multiplier v x t) x

{- |This class is allowed to extract all bound functions. It has a very simple
implementation: we take process description (all planned functions), and
function in progress, if it is.
-}
instance (Ord t) => WithFunctions (Multiplier v x t) (F v x) where
    functions Multiplier{process_, remain, currentWork} =
        functions process_
            ++ remain
            ++ maybeToList currentWork

{- |Tracking internal dependencies on the processed variables. It includes:

- dependencies between inputs and outputs of currently evaluated function;

- dependencies of all remain functions from the currently evaluated function
  (if it is).
-}
instance (Var v) => Locks (Multiplier v x t) v where
    locks Multiplier{remain, sources, targets} =
        [ Lock{lockBy, locked}
        | locked <- sources
        , lockBy <- targets
        ]
            ++ [ Lock{lockBy, locked}
               | locked <- concatMap (S.elems . variables) remain
               , lockBy <- sources ++ targets
               ]
            ++ concatMap locks remain

{- |That type classes ('BreakLoopProblem', 'OptimizeAccumProblem',
'ResolveDeadlockProblem', 'ConstantFoldingProblem') describes the possibility of PU to modify an
algorithm. Empty implementation means that multiplier PU doesn't have such
possibilities.
-}
instance BreakLoopProblem (Multiplier v x t) v x

instance ConstantFoldingProblem (Multiplier v x t) v x
instance OptimizeAccumProblem (Multiplier v x t) v x
instance ResolveDeadlockProblem (Multiplier v x t) v x

{- | This type class specifies how to bind functions to the PU. If it is
possible, @tryBind@ function will return @Right@ value with a new PU model
state. If not, @Left@ value with reason description. And also specify how to
get computation process description.

From the CAD point of view, bind looks like:

- CAD asks PU models: "Who can evaluate this function?" and get the list of
  possible bindings.

- CAD, based on the different metrics (see 'NITTA.Synthesis'), the best variant
  is chosen.

Binding can be done either gradually due synthesis process at the start.
-}
instance (VarValTime v x t) => ProcessorUnit (Multiplier v x t) v x t where
    tryBind f pu@Multiplier{remain}
        | Just F.Multiply{} <- castF f = Right pu{remain = f : remain}
        | otherwise = Left $ "The function is unsupported by Multiplier: " ++ show f

    -- Unified interface for getting computation process description.
    process = process_

-- | Execute function (set as current and remove from remain).
execution pu@Multiplier{targets = [], sources = [], remain} f
    | Just (F.Multiply (I a) (I b) (O c)) <- castF f =
        pu
            { targets = [a, b]
            , currentWork = Just f
            , sources = S.elems c
            , remain = remain \\ [f]
            }
execution _ _ = error "Multiplier: internal execution error."

{- | A computational process of PU from a hardware architectural perspective can
be described as a sequence of pushing and pulling values. From a synthesis
perspective, it is represented by 'EndpointProblem', which describes when PU
is a 'Source' or 'Target' of data transfers.

Work with endpoint problem implemented by only two functions:

__endpointOptions__ define what the possible synthesis decision is. It
includes three cases:

- Not a function is executed. That means that we have options to push any
  input variables of remain functions.

- The function is executed, and not all arguments are received. We have
  options to push remain variables.

- The function is executed, and all arguments are received. We have options
  to pull the result from the multiplier, which can include several
  variables. These variables can be got one by one or all at once because the
  value will be written to the bus and read by several processor units on the
  hardware level.

Note: an option don't specify moment for action but specify an available
interval ('NITTA.Model.Types.TimeConstraint'). That describes the interval for
action start and restriction on process duration.

__endpointDecision__ defines how to apply synthesis decision to the PU model.
It includes three cases:

- Push an input variable of the executed function. We need to schedule
  instruction for endpoint action and modify the model state.

- Pull an output variable or variables of the executed function. We need to
  schedule instruction for endpoint action and modify the model state.

- Push an input variable of a not executed function. In this case, we need to
  find the selected function, 'execute' it, and do a recursive call with the
  same decision.
-}
instance (VarValTime v x t) => EndpointProblem (Multiplier v x t) v t where
    endpointOptions Multiplier{targets, process_}
        | not $ null targets =
            let at = nextTick process_ + 1 ... maxBound
                duration = 1 ... maxBound
             in map (\v -> EndpointSt (Target v) $ TimeConstraint at duration) targets
    endpointOptions Multiplier{sources, currentWork = Just f, process_}
        | not $ null sources =
            let doneAt = inputsPushedAt process_ f + 3
                at = max doneAt (nextTick process_ + 1) ... maxBound
                duration = 1 ... maxBound
             in [EndpointSt (Source $ S.fromList sources) $ TimeConstraint at duration]
    endpointOptions pu@Multiplier{remain} = concatMap (endpointOptions . execution pu) remain

    endpointDecision pu@Multiplier{targets} d@EndpointSt{epRole = Target v, epAt}
        | not $ null targets
          , ([_], targets') <- partition (== v) targets
          , -- @sel@ veriable is used for uploading queuing of variable to hardware block, that is
            -- requred because of realisation.
            let sel = if null targets' then B else A
          , --  Computation process planning is carried out.
            let (_, process_') = runSchedule pu $ do
                    -- this is required for correct work of automatically generated tests,
                    -- that takes information about time from Process
                    updateTick (sup epAt)
                    scheduleEndpoint d $ scheduleInstruction epAt $ Load sel =
            pu
                { process_ = process_'
                , -- The remainder of the work is saved for the next loop
                  targets = targets'
                }
    endpointDecision pu@Multiplier{targets = [], sources, currentWork = Just f, process_} d@EndpointSt{epRole = Source v, epAt}
        | not $ null sources
          , let sources' = sources \\ S.elems v
          , sources' /= sources
          , let a = inf $ stepsInterval $ relatedEndpoints process_ $ variables f
          , -- Compututation process planning is carring on.
            let (_, process_') = runSchedule pu $ do
                    endpoints <- scheduleEndpoint d $ scheduleInstruction epAt Out
                    when (null sources') $ do
                        high <- scheduleFunction (a ... sup epAt) f
                        let low = endpoints ++ map pID (relatedEndpoints process_ $ variables f)
                        -- Set up the vertical relantions between functional unit
                        -- and related to that data sending.
                        establishVerticalRelations high low
                    -- this is needed to correct work of automatically generated tests
                    -- that takes time about time from Process
                    updateTick (sup epAt)
                    return endpoints =
            pu
                { process_ = process_'
                , -- In case if not all variables what asked - remaining are saved.
                  sources = sources'
                , -- if all of works is done, then time when result is ready,
                  -- current work and data transfering, what is done is the current function is reset.
                  currentWork = if null sources' then Nothing else Just f
                }
    endpointDecision pu@Multiplier{targets = [], sources = [], remain} d
        | let v = oneOf $ variables d
          , Just f <- find (\f -> v `S.member` variables f) remain =
            endpointDecision (execution pu f) d
    -- If something went wrong.
    endpointDecision pu d = error $ "Multiplier decision error\npu: " ++ show pu ++ ";\n decison:" ++ show d

-- TODO: optimize ArgumentSelector

{- |Multiplications argument id

As we said before, because of some hardware organisation features, we need to
take in mind operators sequence in planned process on instruction level. This
type os defined to do it. But instide of this we need to notice, that from
algorhytm and model way of view argument order doesn't mean, that is
represented in class computation process' planning responding that realised
above.
-}
data ArgumentSelector = A | B
    deriving (Show, Eq)

{- |For each PU, we can specify the instruction set and microcode, which allows
us to control the PU at the hardware level.

- instructions set describes a computation process from a programmer point of
  view;

- microcode describes the structure of processors that controls signals.
-}
instance Controllable (Multiplier v x t) where
    data Instruction (Multiplier v x t)
        = Load ArgumentSelector
        | Out
        deriving (Show)

    data Microcode (Multiplier v x t) = Microcode
        { -- | Write to mUnit signal.
          wrSignal :: Bool
        , -- |Uploading to mUnit argument selector.
          selSignal :: Bool
        , -- |Downloading from mUnit signal.
          oeSignal :: Bool
        }
        deriving (Show, Eq, Ord)

    zipSignalTagsAndValues MultiplierPorts{..} Microcode{..} =
        [ (wr, Bool wrSignal)
        , (wrSel, Bool selSignal)
        , (oe, Bool oeSignal)
        ]

    usedPortTags MultiplierPorts{wr, wrSel, oe} = [wr, wrSel, oe]

    takePortTags (wr : wrSel : oe : _) _ = MultiplierPorts wr wrSel oe
    takePortTags _ _ = error "can not take port tags, tags are over"

{- |Default microcode state should be equal to @nop@ function, which should be a
safe way to do nothing (not take a bus, not change internal PU state, etc.).
-}
instance Default (Microcode (Multiplier v x t)) where
    def =
        Microcode
            { wrSignal = False
            , selSignal = False
            , oeSignal = False
            }

{- |Instruction and microcode should have exact matching, which allows us to
translate PU instructions to microcode value.
-}
instance UnambiguouslyDecode (Multiplier v x t) where
    decodeInstruction (Load A) = def{wrSignal = True, selSignal = False}
    decodeInstruction (Load B) = def{wrSignal = True, selSignal = True}
    decodeInstruction Out = def{oeSignal = True}

{- |Processor unit control signal ports. In
'NITTA.Model.Networks.Bus.BusNetwork', these ports are directly connecting to
@ControlUnit@.
-}
instance Connected (Multiplier v x t) where
    data Ports (Multiplier v x t) = MultiplierPorts
        { -- |get data from the bus (data_in)
          wr :: SignalTag
        , -- |determine argument on the bus (A | B)
          wrSel :: SignalTag
        , -- |send result to the bus
          oe :: SignalTag
        }
        deriving (Show)

instance IOConnected (Multiplier v x t) where
    data IOPorts (Multiplier v x t) = MultiplierIO
        deriving (Show)

{- |Usage of PU requires some artifacts of a synthesis process:

- Hardware implementation, which depends from 'isMocked' value:

- Software (not needed for the multiplier because it does not have any
  configuration and is controlled from the network level).

- Hardware instance in the upper structure element.
-}
instance (VarValTime v x t) => TargetSystemComponent (Multiplier v x t) where
    moduleName _title _pu = "pu_multiplier"

    hardware _tag Multiplier{isMocked} =
        Aggregate
            Nothing
            [ if isMocked
                then FromLibrary "multiplier/mult_mock.v"
                else FromLibrary "multiplier/mult_inner.v"
            , FromLibrary "multiplier/pu_multiplier.v"
            ]

    software _ _ = Empty

    hardwareInstance
        tag
        _pu
        UnitEnv
            { sigClk
            , sigRst
            , ctrlPorts = Just MultiplierPorts{..}
            , valueIn = Just (dataIn, attrIn)
            , valueOut = Just (dataOut, attrOut)
            } =
            [__i|
                pu_multiplier \#
                        ( .DATA_WIDTH( #{ dataWidth (def :: x) } )
                        , .ATTR_WIDTH( #{ attrWidth (def :: x) } )
                        , .SCALING_FACTOR_POWER( #{ fractionalBitSize (def :: x) } )
                        , .INVALID( 0 )
                        ) #{ tag }
                    ( .clk( #{ sigClk } )
                    , .rst( #{ sigRst } )
                    , .signal_wr( #{ wr } )
                    , .signal_sel( #{ wrSel } )
                    , .data_in( #{ dataIn } )
                    , .attr_in( #{ attrIn } )
                    , .signal_oe( #{ oe } )
                    , .data_out( #{ dataOut } )
                    , .attr_out( #{ attrOut } )
                    );
            |]
    hardwareInstance _title _pu _env = error "internal error"

{- |Empty implementation of 'NITTA.Project.TestBench.IOTestBench' class
means that multiplier, as expected, doesn't have any IO.
-}
instance IOTestBench (Multiplier v x t) v x

{- |The main purpose of this class is to generate autotests for PU. It allows to
generate testbench for the PU according to its model and scheduled computational
process. You can see tests in @test/Spec.hs@. Testbench contains:

- The sequence of control signals that implement the already scheduled process.

- The sequence of bus state checks in which we compare actual values with the
  results of the functional simulation.
-}
instance (VarValTime v x t) => Testable (Multiplier v x t) v x where
    testBenchImplementation prj@Project{pName, pUnit} =
        Immediate (toString $ moduleName pName pUnit <> "_tb.v") $
            snippetTestBench
                prj
                SnippetTestBenchConf
                    { -- List of control signals. It is needed to initialize
                      -- registers with the same names.
                      tbcSignals = ["oe", "wr", "wrSel"]
                    , -- A processor unit connects to the environment by signal
                      -- lines. In 'NITTA.Project.TestBench.tbcPorts'
                      -- describes IDs signal lines of testbench. In
                      -- 'NITTA.Project.TestBench.tbcSignalConnect' how
                      -- abstract numbers are translate to source code.
                      tbcPorts =
                        MultiplierPorts
                            { oe = SignalTag "oe"
                            , wr = SignalTag "wr"
                            , wrSel = SignalTag "wrSel"
                            }
                    , -- Map microcode to registers in the testbench.
                      tbcMC2verilogLiteral = \Microcode{oeSignal, wrSignal, selSignal} ->
                        [i|oe <= #{bool2verilog oeSignal};|]
                            <> [i| wr <= #{bool2verilog wrSignal};|]
                            <> [i| wrSel <= #{bool2verilog selSignal};|]
                    }
