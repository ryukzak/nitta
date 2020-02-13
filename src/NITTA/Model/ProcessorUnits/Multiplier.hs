{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Model.ProcessorUnits.Multiplier
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

Multiplier mUnit can evaluate the following function type:

- 'NITTA.Intermediate.Functions.Multiply'.

In one moment of time only one function can be processing, and its execution
cannot be interrupted.

This module should be considered as a template of development another models of
computational blocks. Its source code is written almost literally, so we
recommend to continue reading within the source code.

= Work example

We will consider the example of computation process planning for one function.
For do this, we will start up GHCI interpreter with execution @stack repl@
process from the project directory. It is the high probability that output of
products actual version will differ.

Connect necessary modules and set up terminals prompt string.

>>> :l NITTA.Model.ProcessorUnits.Multiplier
[ 1 of 10] Compiling NITTA.Model.Problems.Types ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types/Poly.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types/Poly.o ) [flags changed]
[ 2 of 10] Compiling NITTA.Model.Types ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types/Time.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types/Time.o ) [flags changed]
[ 3 of 10] Compiling NITTA.Types.Base ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types/Base.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types/Base.o ) [flags changed]
[ 4 of 10] Compiling NITTA.Model.Networks.Types ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types/Network.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types/Network.o ) [flags changed]
[ 5 of 10] Compiling NITTA.Types      ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types.o ) [flags changed]
[ 6 of 10] Compiling NITTA.Utils.Lens ( /Users/penskoi/Documents/src/nitta/src/NITTA/Utils/Lens.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Utils/Lens.o ) [flags changed]
[ 7 of 10] Compiling NITTA.Utils      ( /Users/penskoi/Documents/src/nitta/src/NITTA/Utils.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Utils.o ) [flags changed]
[ 8 of 10] Compiling NITTA.Intermediate.Functions ( /Users/penskoi/Documents/src/nitta/src/NITTA/Functions.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Functions.o ) [flags changed]
[10 of 10] Compiling NITTA.Model.ProcessorUnits.Multiplier ( /Users/penskoi/Documents/src/nitta/src/NITTA/ProcessUnits/Multiplier.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/ProcessUnits/Multiplier.o )
Ok, 10 modules loaded.
>>> :module +NITTA.Model.Types NITTA.Intermediate.Functions Numeric.Interval Data.Set
>>> :set prompt "\ESC[34mλ> \ESC[m"

Now create the function and multiplier initial state. Unfortunately, it is not
enough information for GHC deduction of its type, so let's define its
implicitly.

>>> let f = multiply "a" "b" ["c", "d"] :: F String Int
>>> f
c = d = a * b
>>> let st0 = multiplier True :: Multiplier String Int Int
>>> st0
Multiplier {puRemain = [], targets = [], sources = [], doneAt = Nothing, process_ = Process {steps = [], relations = [], nextTick = 0, nextUid = 0}, isMocked = True}
>>> endpointOptions st0
[]

Bind a function to the multiplier. This operation could be executed at any time
of working with the model, including when a computation process is fully planned
(new work can be added). The main rule is: if work is fully planned, then it is
necessary to perform it and any part of it cannot be "lost" inside the model. If
a mUnit has his own interior resources, there should be enough to finish
planning, even it is inefficient.

>>> let Right st1 = tryBind f st0
>>> st1
Multiplier {puRemain = [<Multiply (I "a") (I "b") (O (fromList ["c","d"]))>], targets = [], sources = [], doneAt = Nothing, process_ = Process {steps = [], relations = [], nextTick = 0, nextUid = 0}, isMocked = True}
>>> endpointOptions st1
[?Target "a"@(1..∞ /P 1..∞),?Target "b"@(1..∞ /P 1..∞)]

As we can see, after binding we have two different options of computational
process planning, that match different argument loading sequences: @a@ or @b@.
We can see that they are similar from an execution time point of view: loading
can be started from 0 tick or after an arbitrary delay; for loading of one
argument needed only one tick, but it can continue for an arbitrary time. Choose
the variant (note, that if decision matches to proposed options then it cannot
cause a mistake or block another function).

>>> let st2 = endpointDecision st1 $ EndpointSt (Target "a") (0...2)
>>> st2
Multiplier {puRemain = [], targets = ["b"], sources = ["c","d"], doneAt = Nothing, process_ = Process {steps = [Step {sKey = 1, sTime = Activity (0 ... 2), sDesc = Load A},Step {sKey = 0, sTime = Activity (0 ... 2), sDesc = Target "a"}], relations = [], nextTick = 3, nextUid = 2}, isMocked = True}
>>> mapM_ print $ endpointOptions st2
?Target "b"@(3..∞ /P 1..∞)
>>> let st3 = endpointDecision st2 $ EndpointSt (Target "b") (3...3)
>>> st3
Multiplier {puRemain = [], targets = [], sources = ["c","d"], doneAt = Just 6, process_ = Process {steps = [Step {sKey = 3, sTime = Activity (3 ... 3), sDesc = Load B},Step {sKey = 2, sTime = Activity (3 ... 3), sDesc = Target "b"},Step {sKey = 1, sTime = Activity (0 ... 2), sDesc = Load A},Step {sKey = 0, sTime = Activity (0 ... 2), sDesc = Target "a"}], relations = [], nextTick = 4, nextUid = 4}, isMocked = True}
>>> mapM_ print $ endpointOptions st3
?Source (fromList ["c","d"])@(6..∞ /P 1..∞)

After loading of all arguments, we can see that the next option is unloading @c@
and @d@ variables. Note, these variables can be unloaded ether concurrently or
sequentially (for details, see how the multiplier works). Consider the second
option:

>>> let st4 = endpointDecision st3 $ EndpointSt (Source $ fromList ["c"]) (6...6)
>>> st4
Multiplier {puRemain = [], targets = [], sources = ["d"], doneAt = Just 6, process_ = Process {steps = [Step {sKey = 5, sTime = Activity (6 ... 6), sDesc = Out},Step {sKey = 4, sTime = Activity (6 ... 6), sDesc = Source (fromList ["c"])},Step {sKey = 3, sTime = Activity (3 ... 3), sDesc = Load B},Step {sKey = 2, sTime = Activity (3 ... 3), sDesc = Target "b"},Step {sKey = 1, sTime = Activity (0 ... 2), sDesc = Load A},Step {sKey = 0, sTime = Activity (0 ... 2), sDesc = Target "a"}], relations = [], nextTick = 7, nextUid = 6}, isMocked = True}
>>> mapM_ print $ endpointOptions st4
?Source (fromList ["d"])@(7..∞ /P 1..∞)
>>> let st5 = endpointDecision st4 $ EndpointSt (Source $ fromList ["d"]) (7...7)
>>> st5
Multiplier {puRemain = [], targets = [], sources = [], doneAt = Nothing, process_ = Process {steps = [Step {sKey = 7, sTime = Activity (7 ... 7), sDesc = Out},Step {sKey = 6, sTime = Activity (7 ... 7), sDesc = Source (fromList ["d"])},Step {sKey = 5, sTime = Activity (6 ... 6), sDesc = Out},Step {sKey = 4, sTime = Activity (6 ... 6), sDesc = Source (fromList ["c"])},Step {sKey = 3, sTime = Activity (3 ... 3), sDesc = Load B},Step {sKey = 2, sTime = Activity (3 ... 3), sDesc = Target "b"},Step {sKey = 1, sTime = Activity (0 ... 2), sDesc = Load A},Step {sKey = 0, sTime = Activity (0 ... 2), sDesc = Target "a"}], relations = [], nextTick = 8, nextUid = 8}, isMocked = True}
>>> endpointOptions st5
[]

All options of computing process planning are run out. All bound functions
planned. Further microcode can be generated, which can be organizing the
described computational process on the multiplier.
-}

-- FIXME: A promising direction for the improvement is the implementation of the
-- accumulator into it. It allows multiplying an arbitrary number of arguments,
-- which will reduce the number of data transactions on the bus when multiplying
-- more than two variables by one function.

module NITTA.Model.ProcessorUnits.Multiplier
    ( multiplier
    , Multiplier
    , Ports(..), IOPorts(..)
    ) where

import           Control.Monad                   (when)
import           Data.Bits                       (finiteBitSize)
import           Data.Default
import           Data.List                       (find, partition, (\\))
import           Data.Set                        (elems, fromList, member)
import qualified NITTA.Intermediate.Functions    as F
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.Problems.Refactor
import           NITTA.Model.ProcessorUnits.Time
import           NITTA.Model.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Types
import           NITTA.Utils
import           NITTA.Utils.ProcessDescription
import           Numeric.Interval                (sup, (...))
import           Text.InterpolatedString.Perl6   (qc)

{-
= Processor (or process unit in an early version)

A mUnit with any type can be used for:

- data storage and processing;
- interaction with the periphery;
- control of a NITTA mUnit.

Wherein, they are characterized by complicated behavior, that is expressed in:

- multifunctionality
- internal parallelism
- superscalar
- pipelining
- availability of internal resources

The multiplier is one of the easiest processors from this point of view because
it realizes only data processing by only one function
('NITTA.Intermediate.Functions.Multiply'). Processor behavior in a specific application
determined by the applied algorithm (composition of function with data
dependencies).

Any mUnit may have three components:

- hardware - set of prepared or automatically generated hardware descriptions
  (@/hdl/multiplier@);
- software - set of binary files, that determines:
    - mUnit's initial state and setting (optional) ;
    - a control program for the specific mUnit;
- CAD model - CAD component, that realizes mUnit support (hardware and
  software generation, instance generation, computation process planning and
  etc).

Wherein all of three components are hardly related to each other and needed to
strictly comply to each other. For a deeper understanding, mUnit developer
should understand all of its components. Multiplier model will be described
above.
-}

{-
*CAD mUnit model

A mUnit model purpose is "teaching" CAD how to work with the mUnit:

- which functions could be evaluated (see 'NITTA.Types', class @ProcessorUnit@,
  function @tryBind@);
- how to controlling of the mUnit for evaluating specific function (see
  'NITTA.Type', class @ProcessorUnit@ and @Controllable@);
- how to translating instructions to microcode (see 'NITTA.Type', class
  @UnambiguouslyDecode@);
- which options of mUnit computation process are available (see
  'NITTA.Model.Problems.Types', class @ProcessorUnit@, function @options@ and @EndpointDT@):
  - push variable to the mUnit (@Target@);
  - pull at least one variable from the mUnit (@Source@);
- computation process planning ('NITTA.Model.Problems.Types', class @ProcessorUnit@,
  function @decision@ and @EndpointDT@).
-}

{-|
The basis of a mUnit model is a data structure, that represents:

- mUnit state while computation process planning;
- process description (fully or fragmentary), which can be translated to
  software.

Exactly around this data structure, all algorithmic part of mUnit model is
developed. The data structure is parametrized by the following variables types:
- v - variable id (usually @String@);
- x - a type of value (see 'NITTA.Types', @IntX@ and @FX@), with which
  multiplier works;
- t - time moment id (usually @Int@).
-}

-- FIXME: Add assertion, which checks that all synthesis decision compliant
-- available options.

data Multiplier v x t = Multiplier
    {
    -- |List of the assigned, but not processed functions. Functions execution
    -- starts with:
    --
    -- - removing functions from this list;
    -- - transfering information from function to 'targets' and 'sources'
    --   fields.
    --
    -- Assigned function can be executed in a random order.
      remain               :: [ F v x ]
    -- |List of variables, which are needed to upload to mUnit for current
    -- function computation.
    , targets              :: [ v ]
    -- |List of variables, which are needed to download from mUnit for
    -- current function computation. Download order is arbitrary. Necessary
    -- to notice that all downloading variables match to one value -
    -- multiplying result.
    , sources              :: [ v ]
    -- Actual process of multiplying will be finished in the specified
    -- moment and its result will be available for download. Value is
    -- established after uploading of all arguments.
    , doneAt               :: Maybe t
    , currentWork          :: Maybe ( t, F v x )
    -- |While planning of execution of function necessary to define
    -- undefined value of uploading / downloading of data to / from mUnit,
    -- to then set up vertical behavior between information about executing
    -- function and this send.
    , currentWorkEndpoints :: [ ProcessUid ]
    -- |Description of target computation process
    -- ('NITTA.Model.ProcessorUnits.Time')
    , process_             :: Process v x t
    , tick                 :: t
    -- |HDL implementation of PU contains multiplier IP core from Altera
    -- Quartus. This can not be simulated by Icarus Verilog. If `isMocked`
    -- is marked, a target system will be contained non-synthesizable
    -- implementation of that IP-core.
    , isMocked             :: Bool
    }
    deriving ( Show )



-- |Tracking internal dependencies on the data generated by the mUnit.
instance ( Var v ) => Locks (Multiplier v x t) v where
    locks Multiplier{ remain, sources, targets } =
        -- The dependence of the output of the loaded arguments. If @ sources @ is an empty list,
        -- then there will be no dependencies.
        [ Lock{ lockBy, locked }
        | locked <- sources
        , lockBy <- targets
        ]
        ++
        -- The dependencies of the functions in the queue on the current moment.
        [ Lock{ lockBy, locked }
        | locked <- concatMap (elems . variables) remain
        , lockBy <- sources ++ targets
        ]

instance RefactorProblem (Multiplier v x t) v x


-- |Multiplier mUnit construction. Argument define inner organisation of the computation
-- unit:  using of multiplier IP kernel (False) or mock (True). For more information look hardware function
-- in 'TargetSystemComponent' class.
multiplier mock = Multiplier
    { remain=[]
    , targets=[]
    , sources=[]
    , doneAt=Nothing
    , currentWork=Nothing
    , currentWorkEndpoints=[]
    , process_=def
    , tick=def
    , isMocked=mock
    }


-- This type class carry out binding of functions to computational blocks. It lets to check,
-- can function be computated by this mUnit and if can - carry out functuons assignment.
-- Within it binding renouncement can be related  either to that type of functions doesn't supporting
-- or with that inner resources of mUnit are empty.
--
-- From CAD point of view bind looks like: CAD aks models from all available instances of
-- mUnit and get list of instances ready to start work with considered function. After this, based
-- on the different metrics (for example, uploading of processors, number and types of still not binded functions)
-- the best variant is choosed. Binding can be done either gradully while computation process planning or
-- at the same time on the start for all functions.
instance ( VarValTime v x t
         ) => ProcessorUnit (Multiplier v x t) v x t where
    -- Binding to mUnit is carried out by this function.
    tryBind f pu@Multiplier{ remain }
        -- To do this, it is checked whether the function type is reduced to one of the supported
        -- by ('NITTA.FunctionalBlocks.castF')  and in case of success model conditions is returned
        -- after binding with 'Right' mark.
        --
        -- Important to notice, that "binding" doesn't mean actually beginning of work, that
        -- allows firstly make bindings of all tasks and after plan computation process.
        | Just F.Multiply{} <- castF f = Right pu{ remain=f : remain }
        -- In case of impossibility of binding string with short description of renouncement
        --cause and 'Left' is returned.
        | otherwise = Left $ "The function is unsupported by Multiplier: " ++ show f
    -- Unificate interface for get computation process description.
    process = process_
    -- This method is used for set up mUnit time outside.
    -- At the time this is needed only for realisation
    -- of branching, which is on the prototyping stage.
    setTime t pu@Multiplier{} = pu{ tick=t }


-- |This function carry out actual take functional block to work.
execution pu@Multiplier{ targets=[], sources=[], remain, tick } f
    | Just (F.Multiply (I a) (I b) (O c)) <- castF f
    = pu
        { targets=[a, b]
        , currentWork=Just (tick + 1, f)
        , sources=elems c
        , remain=remain \\ [ f ]
        }
execution _ _ = error "Multiplier: internal execution error."



{-
Result of planning is description of one computation cycle, which later can be translated to microcode,
directly control mUnit. From NITTA architecture point of view, process can be described as
consistent execution two roles by processoe:

- data source ('Source');
- data target ('Target');

The planning process itself consists of two operations performed in a cycle:
-}
instance ( VarValTime v x t
        ) => EndpointProblem (Multiplier v x t) v t
        where
    --1. Processors is asked about roles it can realise (in the other words, how computation
    --process can develop). It is realised by @options@ functions, result of which is
    --one of the further list:

    --list of variants of uploading to mUnit variables, which are needed to function
    --that is in work;
    endpointOptions Multiplier{ targets=vs@(_:_), tick }
        = map (\v -> EndpointSt (Target v) $ TimeConstrain (tick + 1 ... maxBound) (1 ... maxBound)) vs

     --   list of variants of downloading from mUnit variables;
    endpointOptions Multiplier{ sources, doneAt=Just at, tick }
        | not $ null sources
        = [ EndpointSt (Source $ fromList sources) $ TimeConstrain (max at (tick + 1) ... maxBound) (1 ... maxBound) ]

    -- list of variables of uploading to mUnit variables, upload any one of that
    -- will cause to actual start of working with mathched function.
    endpointOptions pu@Multiplier{ remain } = concatMap (endpointOptions . execution pu) remain

    -- Note, that options provided by this function require clarification, because:

    --	1.	They point to not specific moment for work, but to available interval
    --		('NITTA.Types.Base.TimeConstrain'), that describe from and to which time
    -- 		uploading and downloading can be done, and how much time the process can continue.
    -- 	2.	One value can be download from mUnit as several different variables. This can
    --		be done either all at once (on the hardware level the value writed to the bus and
    -- 		read by several processors), as a consistent (firstly value on the bus can be writed for
    --		one mUnit, and after for next one), what should be specified too.


    -- 2. 	Process planning or making decision about compuatation process development to
    --	  	mUnit model state is carried out by @decision@. Variant transformation
    --		from got from @options@ is carried out by CAD outside the mUnit model.
    --		We can distinguish the following solutions:
    --
    --		1. If model wait variable uploading:
    endpointDecision pu@Multiplier{ targets=vs, currentWorkEndpoints } d@EndpointSt{ epRole=Target v, epAt }
           -- From the list of uploading value we get a needed value, and remainder is saved
           -- for the next steps.
        | ([_], xs) <- partition (== v) vs
             -- @sel@ veriable is used for uploading queuing of variable to hardware block, that is
             -- requred because of realisation.
        , let sel = if null xs then B else A
             --  Computation process planning is carried out.
        , let (newEndpoints, process_') = runSchedule pu $ do
                -- this is required for correct work of automatically generated tests,
                -- that takes information about time from Process
                updateTick (sup epAt)
                scheduleEndpoint d $ scheduleInstruction epAt $ Load sel
        = pu
            { process_=process_'
            -- The remainder of the work is saved for the next loop
            , targets=xs
            -- We save information about events that describe sending or recieving data for
            -- current functionatl unit.
            , currentWorkEndpoints=newEndpoints ++ currentWorkEndpoints
            -- If all required arguments are upload (@null xs@), then the moment of time
            -- when we get a result is saved.
             , doneAt=if null xs
                then Just $ sup epAt + 3
                else Nothing
            -- Model time is running
             , tick=sup epAt
            }
--	2. If model is waiting, that we will download variables from it.
    endpointDecision pu@Multiplier{ targets=[], sources, doneAt, currentWork=Just (a, f), currentWorkEndpoints } d@EndpointSt{ epRole=Source v, epAt }
        | not $ null sources
        , let sources' = sources \\ elems v
        , sources' /= sources
        -- Compututation process planning is carring on.
        , let (newEndpoints, process_') = runSchedule pu $ do
                endpoints <- scheduleEndpoint d $ scheduleInstruction epAt Out
                when (null sources') $ do
                    high <- scheduleFunction (a ... sup epAt) f
                    let low = endpoints ++ currentWorkEndpoints
                    -- Set up the vertical relantions between functional unit
                    -- and related to that data sending.
                    establishVerticalRelations high low
                -- this is needed to correct work of automatically generated tests
                -- that takes time about time from Process
                updateTick (sup epAt)
                return endpoints
        = pu
            { process_=process_'
              -- In case if not all variables what asked - remaining are saved.
            , sources=sources'
              -- if all of works is done, then time when result is ready,
              -- current work and data transfering, what is done is the current function is reset.
            , doneAt=if null sources' then Nothing else doneAt
            , currentWork=if null sources' then Nothing else Just (a, f)
            , currentWorkEndpoints=if null sources' then [] else newEndpoints ++ currentWorkEndpoints
              -- Model time is running up
            , tick=sup epAt
            }
    --    3. If no function is executed at the moment, then we need to find function in the list
    --    of assigned function, executed it to work and only then make decision
    --    and plan a fragment of computation process with call recursion in situation 1.
    endpointDecision pu@Multiplier{ targets=[], sources=[], remain } d
        | let v = oneOf $ variables d
        , Just f <- find (\f -> v `member` variables f) remain
        = endpointDecision (execution pu f) d
    -- If smth went wrong.
    endpointDecision pu d = error $ "Multiplier decision error\npu: " ++ show pu ++ ";\n decison:" ++ show d



-- |Multiplications argument id
--
-- As we said before, because of some hardware organisation features, we need to take in
-- mind operators boot sequence in planned process on instruction level. This type os defined to do it.
-- But instide of this we need to notice, that from algorhytm and model  way of view
-- argument order doesn't mean, that is represented in class computation process'
-- planning responding  that realised above.
data ArgumentSelector = A | B
    deriving ( Show, Eq )



-- |Now we will consider questions of computation process planning organisation on hardware level.
-- For do this on model level two levels of view is defined:
--
-- - instructions level, where  describes computation process in
-- convienment to develover form.
-- - microcode level, where describes structure of processors controls  signals and
-- values.
instance Controllable (Multiplier v x t) where
    -- |Instructions for multiplier mUnit controlling. Multiplier can only
    -- upload arguments A and B, and download multiplication result. This construction
    -- are used in computation process planning by 'schedule' function. Instead of them,
    -- there is a @nop@ function - when no actions execute.
    data Instruction (Multiplier v x t)
        = Load ArgumentSelector
        | Out
        deriving (Show)

    -- Set of signals for mUnit control and microcode view for
    -- the mUnit
    data Microcode (Multiplier v x t)
        = Microcode
          { -- | Write to mUnit signal.
              wrSignal :: Bool
              -- |Uploading to mUnit argument selector.
            , selSignal :: Bool
              -- |Downloading from mUnit signal.
            , oeSignal :: Bool
            }
        deriving ( Show, Eq, Ord )

    mapMicrocodeToPorts Microcode{..} MultiplierPorts{..}
        =
            [ (wr, Bool wrSignal)
            , (wrSel, Bool selSignal)
            , (oe, Bool oeSignal)
            ]

    portsToSignals MultiplierPorts{ wr, wrSel, oe } = [wr, wrSel, oe]

    signalsToPorts (wr:wrSel:oe:_) _ = MultiplierPorts wr wrSel oe
    signalsToPorts _               _ = error "pattern match error in signalsToPorts MultiplierPorts"

-- |Also we need to define default state for microcode (that is match to implicit @nop@ function)
-- This state mean that mUnit is in inaction state, but doesn't busy the bus and storage
-- inner state in predictable view. In multiplier case - it doesn't reset multiplication result and
-- doesn't work with bus. Default state is using for mUnit stop, pause or waiting

instance Default (Microcode (Multiplier v x t)) where
    def = Microcode
        { wrSignal=False
        , selSignal=False
        , oeSignal=False
        }

instance ( Time t ) => Default (Multiplier v x t) where
    def = multiplier True

-- |Instruction and microcode binding is carried up by this class, which requires their
-- unambiguous matching, as well as regardless of the status and settings of the model.
instance UnambiguouslyDecode (Multiplier v x t) where
    decodeInstruction (Load A) = def{ wrSignal=True, selSignal=False }
    decodeInstruction (Load B) = def{ wrSignal=True, selSignal=True }
    decodeInstruction Out      = def{ oeSignal=True }

-- |Processor signals. In @BusNetwork@ this signal directly connecting to ControlUnit.
instance Connected (Multiplier v x t) where
    data Ports (Multiplier v x t) = MultiplierPorts
        { wr    :: SignalTag -- ^get data from the bus (data_in)
        , wrSel :: SignalTag -- ^determine argument on the bus (A | B)
        , oe    :: SignalTag -- ^send result to the bus
        } deriving ( Show )

instance IOConnected (Multiplier v x t) where
  data IOPorts (Multiplier v x t) = MultiplierIO
        deriving ( Show )

-- |The availability of standard values, with which actual result of mUnit in simlator
-- is compared, has the main role in testing. This class carry on standard values generation.
instance ( VarValTime v x t, Integral x
         ) => Simulatable (Multiplier v x t) v x where
    simulateOn cntx _ f
        -- We define the function and delegate its calculation to default realization.
        | Just f'@F.Multiply{} <- castF f = simulate cntx f'
        | otherwise = error $ "Can't simulate on Multiplier: " ++ show f



-- | We use functions that is realized below to generate processors and tests, that use this
-- mUnit. These methods are called while generation of project with net, that include this
-- mUnit or also with tests generation.
instance ( VarValTime v x t
         ) => TargetSystemComponent (Multiplier v x t) where
    -- | Naming of hardwawre module, instance of which is creting for embedding to mUnit.
    -- In this case it is defined in @/hdl/multiplier/pu_multiplier.v@.
    moduleName _title _pu = "pu_multiplier"

    -- | Processors software generator. In case of multiplier this is no software.
    --Let's figure it out. Before we said, that software has two components:
    --
    -- 1. Setting and begin states. In case of multiplier there is no specific settings
    --    for the applied algorhytm.
    -- 2. Microprogram. Processor cannot be user not in mUnit ner sructure, we needn't to
    --    determine software in context of separate unit. Besides, signal lines of separated
    --    processors can be multiplexed. Thereby, microprogram is formed for
    --    processors net just at once in way of merge of the microprogramms, that are
    --    generated on the base of computational planning description
    --    (look. 'NITTA.Model.Networks.Bus').
    software _ _ = Empty

    --	|Processor hardware generator. In case of multiplier, there is no generation.
    --	Multiplier is described by two files: (1) directly multiplier, that is realized
    --	by IP kernel or functional stub. (2) module. that realize interface between
    --	multuplier and processors infostructure.
    hardware tag pu@Multiplier{ isMocked }
        = Aggregate Nothing
            [ if isMocked
                then FromLibrary "multiplier/mult_mock.v"
                else FromLibrary "multiplier/mult_inner.v"
            , FromLibrary $ "multiplier/" ++ moduleName tag pu ++ ".v"
            ]

    --	|Source code fragment generation for create mUnit instance within the processorю
    -- 	The main task of the function is to include mUnit to mUnit infostructure correctly.
    --	and set uo all parameters, names and wires.
    --
    -- Take attention to function @codeBlock@. This function allows a programmer to use
    -- normal code block indentation.
    hardwareInstance tag _pu TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk, signalRst } MultiplierPorts{..} MultiplierIO
        = codeBlock [qc|
            pu_multiplier #
                    ( .DATA_WIDTH( { finiteBitSize (def :: x) } )
                    , .ATTR_WIDTH( { parameterAttrWidth } )
                    , .SCALING_FACTOR_POWER( { fractionalBitSize (def :: x) } )
                    , .INVALID( 0 )  // FIXME: Сделать и протестировать работу с атрибутами.
                    ) { tag }
                ( .clk( {signalClk} )
                , .rst( {signalRst} )
                , .signal_wr( { signal wr } )
                , .signal_sel( { signal wrSel } )
                , .data_in( { dataIn } )
                , .attr_in( { attrIn } )
                , .signal_oe( { signal oe } )
                , .data_out( { dataOut } )
                , .attr_out( { attrOut } )
                );
            |]
    hardwareInstance _title _pu TargetEnvironment{ unitEnv=NetworkEnv{} } _ports _io
        = error "Should be defined in network."


-- As you can see ahead, this class uses to get data bus width from the type level (@x@ type variable).
instance IOTestBench (Multiplier v x t) v x


-- | This class is service and used to extract all functions binding to mUnit.
-- This class is easy realized: we take process description
-- (all planned functions) from mUnit, and function in progress,
-- if it is.
instance ( Ord t ) => WithFunctions (Multiplier v x t) (F v x) where
    functions Multiplier{ process_, remain, currentWork }
        = functions process_
        ++ remain
        ++ case currentWork of
            Just (_, f) -> [f]
            Nothing     -> []


-- The main purpose of this class is to generate auto tests isolated to the mUnit.
-- In case of this it allows to generate test bench for computational unit according to its model
-- and planned computational process. Use can see tests in 'Spec'.
--
-- Testing is carried out as follows: om the base of mUnit description it generate sequence
-- of outer influence o mUnit (signals and input data), and also check sequence of output signals
-- and data. Output data is compared with results of functional simulations and if they doesn't match
-- then error message is displaing.
instance ( VarValTime v x t, Integral x
         ) => Testable (Multiplier v x t) v x where
    testBenchImplementation prj@Project{ pName, pUnit }
        -- Test bech is one file described below. We use ready snippet for it generation, because
        -- in most cases they will be similar. The data structure 'NITTA.Project.Parts.SnippetTestBenchConf' has the
        -- key role and describes this module specific.
        = Immediate (moduleName pName pUnit ++ "_tb.v")
            $ snippetTestBench prj SnippetTestBenchConf
            -- List of control signals. It is needed to initialize registers with the same names.
                { tbcSignals=["oe", "wr", "wrSel"]
             --Processor to environment connect function and signal lines IDs. In @tbcPorts@ describes
             -- to what connect signal lines of test block. In @tbcSignalConnect@  how abstract numbers
             -- is displays to generated source code.
                , tbcPorts=MultiplierPorts
                    { oe=SignalTag 0
                    , wr=SignalTag 1
                    , wrSel=SignalTag 2
                    }
                , tbcIOPorts=MultiplierIO
                , tbcSignalConnect= \case
                    (SignalTag 0) -> "oe"
                    (SignalTag 1) -> "wr"
                    (SignalTag 2) -> "wrSel"
                    _ -> error "testBenchImplementation wrong signal"
                  -- While test bench generation know how processors control signal is defined.
                  -- This is described below. Notice, that work with data bus is realized in snippet.
                , tbcCtrl= \Microcode{ oeSignal, wrSignal, selSignal } ->
                    [qc|oe <= {bool2verilog oeSignal}; wr <= {bool2verilog wrSignal}; wrSel <= {bool2verilog selSignal};|]
                , tbDataBusWidth=finiteBitSize (def :: x)
                }

