{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

{-|
Module: Multiplier (model of computational multiplier block)

In this module model of computational multiplier block is developing for CAD. 
It can count the following functions:

- 'NITTA.Functions.Multiply'.

In one monent of time only one function can be being calculate, 
and its execution cannot be interrupted.

This module should be considered as a template of development another models of computational blocks. 
Its source code is written almost literally, so we recommend to continue reading with in source code.



= Work example

We will consider  example of computation process planning for one function. 
For do this, we will start up GHCI interpreter with execution @stack repl@ process from project directory. 
It is the high probability that ouput of products actual version will differ.

Connect neccer modules and set up terminals promt string. 

>>> :l NITTA.ProcessUnits.Multiplier
[ 1 of 10] Compiling NITTA.Types.Poly ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types/Poly.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types/Poly.o ) [flags changed]
[ 2 of 10] Compiling NITTA.Types.Time ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types/Time.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types/Time.o ) [flags changed]
[ 3 of 10] Compiling NITTA.Types.Base ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types/Base.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types/Base.o ) [flags changed]
[ 4 of 10] Compiling NITTA.Types.Network ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types/Network.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types/Network.o ) [flags changed]
[ 5 of 10] Compiling NITTA.Types      ( /Users/penskoi/Documents/src/nitta/src/NITTA/Types.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Types.o ) [flags changed]
[ 6 of 10] Compiling NITTA.Utils.Lens ( /Users/penskoi/Documents/src/nitta/src/NITTA/Utils/Lens.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Utils/Lens.o ) [flags changed]
[ 7 of 10] Compiling NITTA.Utils      ( /Users/penskoi/Documents/src/nitta/src/NITTA/Utils.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Utils.o ) [flags changed]
[ 8 of 10] Compiling NITTA.Functions ( /Users/penskoi/Documents/src/nitta/src/NITTA/Functions.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/Functions.o ) [flags changed]
[10 of 10] Compiling NITTA.ProcessUnits.Multiplier ( /Users/penskoi/Documents/src/nitta/src/NITTA/ProcessUnits/Multiplier.hs, /Users/penskoi/Documents/src/nitta/.stack-work/odir/NITTA/ProcessUnits/Multiplier.o )
Ok, 10 modules loaded.
>>> :module +NITTA.Types NITTA.Functions Numeric.Interval Data.Set
>>> :set prompt "\ESC[34mλ> \ESC[m"

Now create function and initial state multiplier computing unit. Unfortunately, it is not ehoght information for GHC
from context to print it types, so lets set up clearly.

>>> let f = multiply "a" "b" ["c", "d"] :: F (Parcel String Int)
>>> f
<Multiply (I "a") (I "b") (O (fromList ["c","d"]))>
>>> let st0 = multiplier True :: Multiplier String Int Int
>>> st0
Multiplier {puRemain = [], targets = [], sources = [], doneAt = Nothing, process_ = Process {steps = [], relations = [], nextTick = 0, nextUid = 0}, isMocked = True}
>>> options endpointDT st0
[]

Assign function to computing unit. This operation could be executed at any time of work with model, including 
when computation process is fully planned (new work adding). The main rule is: if work is fully planned,
then it is necessarily to perform it and cannot be "lost" inside the model. If computing unit has his own interior resources, their should be enoght to finish planning, 
even it is uneffecient.

>>> let Right st1 = tryBind f st0
>>> st1
Multiplier {puRemain = [<Multiply (I "a") (I "b") (O (fromList ["c","d"]))>], targets = [], sources = [], doneAt = Nothing, process_ = Process {steps = [], relations = [], nextTick = 0, nextUid = 0}, isMocked = True}
>>> mapM_ print $ options endpointDT st1
?Target "a"@(0..∞ /P 1..∞)
?Target "b"@(0..∞ /P 1..∞)


As we can see, after assignment we have two diffentent ways of computational process development, 
that match different sequences of argument loading: firstly load variable @a@ or @b@. We can see that they are
simiral from execution time point of view: loading can be started from 0 tact or after arbitrary delay;
for loading of one argument needed one tact, but it can continues for arbitary time. We choose one
of variant (notice, that if solution mathes to proposed options then it cannot have mistakes while acceptance
or it cannot fully block another function).


>>> let st2 = decision endpointDT st1 $ EndpointD (Target "a") (0...2)
>>> st2
Multiplier {puRemain = [], targets = ["b"], sources = ["c","d"], doneAt = Nothing, process_ = Process {steps = [Step {sKey = 1, sTime = Activity (0 ... 2), sDesc = Load A},Step {sKey = 0, sTime = Activity (0 ... 2), sDesc = Target "a"}], relations = [], nextTick = 3, nextUid = 2}, isMocked = True}
>>> mapM_ print $ options endpointDT st2
?Target "b"@(3..∞ /P 1..∞)
>>> let st3 = decision endpointDT st2 $ EndpointD (Target "b") (3...3)
>>> st3
Multiplier {puRemain = [], targets = [], sources = ["c","d"], doneAt = Just 6, process_ = Process {steps = [Step {sKey = 3, sTime = Activity (3 ... 3), sDesc = Load B},Step {sKey = 2, sTime = Activity (3 ... 3), sDesc = Target "b"},Step {sKey = 1, sTime = Activity (0 ... 2), sDesc = Load A},Step {sKey = 0, sTime = Activity (0 ... 2), sDesc = Target "a"}], relations = [], nextTick = 4, nextUid = 4}, isMocked = True}
>>> mapM_ print $ options endpointDT st3
?Source (fromList ["c","d"])@(6..∞ /P 1..∞)

After loading of all arguments we can see that next variant is unloading @c@ and @d@ variables 
from multipliers computing unit. It is necesseray to notice that variable can be unloaded ether parallely or
consistently (for details, see how the processor architecture works)/
Consider the second option:

>>> let st4 = decision endpointDT st3 $ EndpointD (Source $ fromList ["c"]) (6...6)
>>> st4
Multiplier {puRemain = [], targets = [], sources = ["d"], doneAt = Just 6, process_ = Process {steps = [Step {sKey = 5, sTime = Activity (6 ... 6), sDesc = Out},Step {sKey = 4, sTime = Activity (6 ... 6), sDesc = Source (fromList ["c"])},Step {sKey = 3, sTime = Activity (3 ... 3), sDesc = Load B},Step {sKey = 2, sTime = Activity (3 ... 3), sDesc = Target "b"},Step {sKey = 1, sTime = Activity (0 ... 2), sDesc = Load A},Step {sKey = 0, sTime = Activity (0 ... 2), sDesc = Target "a"}], relations = [], nextTick = 7, nextUid = 6}, isMocked = True}
>>> mapM_ print $ options endpointDT st4
?Source (fromList ["d"])@(7..∞ /P 1..∞)
>>> let st5 = decision endpointDT st4 $ EndpointD (Source $ fromList ["d"]) (7...7)
>>> st5
Multiplier {puRemain = [], targets = [], sources = [], doneAt = Nothing, process_ = Process {steps = [Step {sKey = 7, sTime = Activity (7 ... 7), sDesc = Out},Step {sKey = 6, sTime = Activity (7 ... 7), sDesc = Source (fromList ["d"])},Step {sKey = 5, sTime = Activity (6 ... 6), sDesc = Out},Step {sKey = 4, sTime = Activity (6 ... 6), sDesc = Source (fromList ["c"])},Step {sKey = 3, sTime = Activity (3 ... 3), sDesc = Load B},Step {sKey = 2, sTime = Activity (3 ... 3), sDesc = Target "b"},Step {sKey = 1, sTime = Activity (0 ... 2), sDesc = Load A},Step {sKey = 0, sTime = Activity (0 ... 2), sDesc = Target "a"}], relations = [], nextTick = 8, nextUid = 8}, isMocked = True}
>>> options endpointDT st5
[]

Variants of computing process development run out. All assigned functions were executed.
 Further microcode can be generated, organizing the described computational process.

-}

-- FIXME: A promising direction for the development of this computing unit is the implantation of a register into it.
-- a drive that allows you to multiply an arbitrary number of arguments, which will reduce the number of transactions of 
-- data on the bus when multiplying more than two variables by one function.

module NITTA.ProcessUnits.Multiplier
    ( multiplier
    , Multiplier
    , PUPorts(..)
    ) where

import           Control.Monad                 (when)
import           Data.Default
import           Data.List                     (find, partition, (\\))
import           Data.Set                      (elems, fromList, member)
import           Data.Typeable
import qualified NITTA.Functions               as F
import           NITTA.Project
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Process
import           Numeric.Interval              (inf, sup, (...))
import           Text.InterpolatedString.Perl6 (qq)

{-
= Computing unit

Computing unit can realize:

- data storage and processing;
- interaction with the periphery;
- management and control of the computer.

Wherein, they are characterized by complicated behavior, that is expressed in:

- multifunctionality
- internal parallelism
- superscalar
- pipelining
- availability of internal resources

Considered computation unit is one of the easiest from this point of view because it realize only 
data processing by one function.

Computation unit behavior determined by applied algorhytm that is compose of function with data dependencies. 
process evolution.

Any computation unit means three components:

- computation unit hardware - set of prepared in advance or generated automatically hardware description files 
on Hardware Description Language (@/hdl/multiplier@); 
- computation unit software - set of binary files, that determines:
	- computation unit's initial state and setting
	- control programm;
- computation unit model in CAD - CAD component, that realize computation unit support 
(hardware and software generation, computation unit union to processors, computation process planning
and etc).

Wherein all of three components are hardly related to each other and needed to strictly comply to 
each other. For deeper understanding if computation unit functioning needed to have an idea about all 
of all of its components. Model of multiplier computation unit and how it is realized will be considered above.

-}

{-
*Computation unit model

Computation unit model objective is "teaching" CAD to work with computation unit:

- which functions could be computated with its help (see 'NITTA.Type.ProcessUnit');
- assign computation unit instance to execution of function (see 'NITTA.Type.Controllable');
- transform instructions to microcode (see 'NITTA.Type.UnambiguouslyDecode')
- which options (@options@)  of computation process development is (upload or download 
one or second variable of variables group);
- computation process planning, which is deescribed by upload or download to pr from computation unit. 
(see @decision@)
-}

{-|
The basis of computation units model is data structure, that fixes:

- conputation unit state while computation process planning;
- computation unit desciption (fully or fragmentary), which can be translated to software.

Exactly around this data structure all algorhitmic part of computation unit is developed. 
Data structure is parametrizes by following variables types:
- v - variable id;
- x - type of value, with that multiplier works;
- t - time moment id.

|-}

-- FIXME: Разработать safeDecision, которая будет проверять осуществлять дополнительные проверки
-- корректности принятого решения.

-- FIXME: Убрать сигнал wrSignal.





data Multiplier v x t
    = Multiplier
    { --| List of the assigned, but still not processed or cannot be processed functions.
      -- Functions execution starts with:
      --
      -- - deleting functions from this list;
      -- - transfering information from function to 'targets' and 'sources' ('assignment') fields,
      --   furtherly this function will be current.
      -- 
      -- Assigned function can be executed in random order. Information about executed functions storaging
      -- explicitly does not carried out, because it is in description os computation
      -- process 'process_
      remain               :: [F (Parcel v x)]'
      -- |List of variables, which are needed to upwnload to computation unit for
      -- current function computation.
      , targets              :: [v]
      -- |List of variables, which are needed to download from computation unit for
      -- current function computation. Download order is arbitrary. Necessary to notice that
      -- all downloading variables match to one value - multipliing result.
      , sources              :: [v]
      -- Actual process of multipliing will be finished in the specified moment and its
      -- result will be availible for download. Value is established after uploading of 
      -- all arguments.
  	  , doneAt               :: Maybe t
      , currentWork          :: Maybe (t, F (Parcel v x))
      -- | While planning of execution of function necessery to define undefined value of uploading /
      -- downloading of data to / from computation unit, to then set up vertical behavior between
      -- information about executing function and this send.
      , currentWorkEndpoints :: [ ProcessUid ]
      -- | Description of computation process, planned to the computation unit 
      -- 'NITTA.Types.Base.Process'.
      , process_             :: Process (Parcel v x) t
      , tick                 :: t
      -- | In realisation of the computation unit IP kernel that supplied with Altera Quartus used.
      -- This is don't allow to simulate with Icarus Verilog.
      -- To get around with the restriction the mock was created, that connect instrad of IP kernel
      -- ig the flag is set up.
              , isMocked             :: Bool
       }
    deriving ( Show )

	-- | Multiplier processor construction. Argument define inner organisation of the computation
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


-- | This type class carry out binding of functions to computational blocks. It lets to check,
-- can function be computated by this processor and if can - carry out functuons assignment.
-- Within it binding renouncement can be related  either to that type of functions doesn't supporting
-- or with that inner resources of processor are empty.
--
-- From CAD point of view bind looks like: CAD aks models from all available instances of
-- processor and get list of instances ready to start work with considered function. After this, based
-- on the different metrics (for example, uploading of processors, number and types of still not binded functions)
-- the best variant is choosed. Binding can be done either gradully while computation process planning or
-- at the same time on the start for all functions. 



instance ( Var v, Time t
         ) => ProcessUnit (Multiplier v x t) (Parcel v x) t where
	--| Binding to processor is carried out by this function.
	tryBind f pu@Multiplier{ remain }
		-- To do this, it is checked whether the function type is reduced to one of the supported 
		-- by ('NITTA.FunctionalBlocks.castF')  and in case of success model conditions is returned
		-- after binding with 'Right' mark.
		--
		-- Important to notice, that "binding" doesn't mean actually beginning of work, that
		-- allows firstly make bindings of all tasks and after plan computation process.
		| Just F.Multiply{} <- F.castF f = Right pu{ remain=f : remain }
		-- In case of impossibility of binding string with short description of renouncement 
		--cause and 'Left' is returned.
		| otherwise = Left $ "The function is unsupported by Multiplier: " ++ show f
	--Unificated interface for get computation process description.
	process = process_
	-- | This method is used for set up processor time outside. At the time this is needed only for realisation 
	-- of branching, which is on the prototyping stage.
	setTime t pu@Multiplier{} = pu{ tick=t }

-- |This function carry out actual take functional block to work.
assignment pu@Multiplier{ targets=[], sources=[], remain, tick } f
    | Just (F.Multiply (I a) (I b) (O c)) <- F.castF f
    = pu
        { targets=[a, b]
        , currentWork=Just (tick + 1, f)
        , sources=elems c, remain=remain \\ [ f ]
        }
assignment _ _ = error "Multiplier: internal assignment error."



{-
Result of planning is description of one computation cycle, which later can be translated to microcode,
directly control processor. From NITTA architecture point of view, process can be described as
consistent execution two roles by processoe:

- data source ('Source');
- data target ('Target');

The planning process itself consists of two operations performed in a cycle:
-}
instance ( Var v, Time t, Typeable x
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (Multiplier v x t)
        where


    --1. Processors is asked about roles it can realise (in the other words, how computation 
    --process can develop). It is realised by @options@ functions, result of which is 
    --one of the further list:

    --list of variants of uploading to processor variables, which are needed to function
    --that is in work;
    options _proxy Multiplier{ targets=vs@(_:_), tick }
        = map (\v -> EndpointO (Target v) $ TimeConstrain (tick + 1 ... maxBound) (1 ... maxBound)) vs

     --   list of variants of downloading from processor variables;
     options _proxy Multiplier{ sources, doneAt=Just at, tick }
        | not $ null sources
        = [ EndpointO (Source $ fromList sources) $ TimeConstrain (max at (tick + 1) ... maxBound) (1 ... maxBound) ]

    -- list of variables of uploading to processor variables, upload any one of that 
    -- will cause to actual start of working with mathched function.
    options proxy pu@Multiplier{ remain } = concatMap (options proxy . assignment pu) remain

    -- Note, that options provided by this function require clarification, because:

    --	1.	They point to not specific moment for work, but to available interval 
    --		('NITTA.Types.Base.TimeConstrain'), that describe from and to which time 
    -- 		uploading and downloading can be done, and how much time the process can continue.
    -- 	2.	One value can be download from processor as several different variables. This can
    --		be done either all at once (on the hardware level the value writed to the bus and
    -- 		read by several processors), as a consistent (firstly value on the bus can be writed for 
    --		one processor, and after for next one), what should be specified too.


    -- 2. 	Process planning or making decision about compuatation process development to
    --	  	processor model state is carried out by @decision@. Variant transformation
    --		from got from @options@ is carried out by CAD outside the processor model. 
    --		We can distinguish the following solutions:
    --
    --		1. If model wait variable uploading:
    decision _proxy pu@Multiplier{ targets=vs, currentWorkEndpoints } d@EndpointD{ epdRole=Target v, epdAt }
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
                updateTick (sup epdAt)
                scheduleEndpoint d $ scheduleInstruction (inf epdAt) (sup epdAt) $ Load sel
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
                then Just $ sup epdAt + 3
                else Nothing
            -- Model time is running
             , tick=sup epdAt
            }
--	2. If model is waiting, that we will download variables from it.
    decision _proxy pu@Multiplier{ targets=[], sources, doneAt, currentWork=Just (a, f), currentWorkEndpoints } d@EndpointD{ epdRole=Source v, epdAt }
        | not $ null sources
        , let sources' = sources \\ elems v
        , sources' /= sources
        -- Compututation process planning is carring on.
        , let (newEndpoints, process_') = runSchedule pu $ do
                endpoints <- scheduleEndpoint d $ scheduleInstruction (inf epdAt) (sup epdAt) Out
                when (null sources') $ do
                    high <- scheduleFunction a (sup epdAt) f
                    let low = endpoints ++ currentWorkEndpoints
                 	-- Set up the vertical relantions between functional unit
                 	-- and related to that data sending.
                 	establishVerticalRelations high low
                 	-- this is needed to correct work of automatically generated tests
                 	-- that takes time about time from Process
                     updateTick (sup epdAt)
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
 			, tick=sup epdAt
    	    }
   	-- 		3. If no function is executed at the moment, then we need to find function in the list
   	-- 		of assigned function, executed it to work and only then make decision 
   	-- 		and plan a fragment of computation process with call recursion in situation 1.
   	 decision proxy pu@Multiplier{ targets=[], sources=[], remain } d
    | let v = oneOf $ variables d
    , Just f <- find (\f -> v `member` variables f) remain
    = decision proxy (assignment pu f) d
    -- If smth went wrong. 
	decision _ pu d = error $ "Multiplier decision error\npu: " ++ show pu ++ ";\n decison:" ++ show d



--| Multiplications argument id
--
-- As we said before, because of some hardware organisation features, we need to take in 
-- mind operators boot sequence in planned process on instruction level. This type os defined to do it.
-- But instide of this we need to notice, that from algorhytm and model  way of view 
-- argument order doesn't mean, that is represented in class computation process'
-- planning responding  that realised above.  
data ArgumentSelector = A | B
    deriving ( Show, Eq )



--| Now we will consider questions of computation process planning organisation on hardware level.
-- For do this on model level two levels of view is defined:
--
-- - instructions level, where  describes computation process in 
-- convienment to develover form.
-- - microcode level, where describes structure of processors controls  signals and
-- values. 
instance Controllable (Multiplier v x t) where
--| Instructions for multiplier processor controlling. Multiplier can only 
-- upload arguments A and B, and download multiplication result. This construction 
-- are used in computation process planning by 'schedule' function. Instead of them,
-- there is a @nop@ function - when no actions execute.
data Instruction (Multiplier v x t)
       = Load ArgumentSelector
       | Out
       deriving (Show)

	-- Set of signals for processor control and microcode view for 
	-- the processor
    data Microcode (Multiplier v x t)
        = Microcode
          { -- | Write to processor signal.
              wrSignal :: Bool
              -- |Uploading to processor argument selector.
            , selSignal :: Bool
              -- |Downloading from processor signal.
            , oeSignal :: Bool
            }
        deriving ( Show, Eq, Ord )

--|Also we need to define default state for microcode (that is match to implicit @nop@ function)
-- This state mean that processor is in inaction state, but doesn't busy the bus and storage 
-- inner state in predictable view. In multiplier case - it doesn't reset multiplication result and
-- doesn't work with bus. Default state is using for processor stop, pause or waiting

instance Default (Microcode (Multiplier v x t)) where
    def = Microcode
        { wrSignal=False
        , selSignal=False
        , oeSignal=False
        }

-- |Instuction  and microcode binding is carried up by this class, which requires their 
-- unambiguous matching, as well as regardless of the status and settings of the model.

-- TODO: We need binding to PU, because in model setting we can define width 
-- of all values (like addr in Fram)
instance UnambiguouslyDecode (Multiplier v x t) where
    decodeInstruction (Load A) = def{ wrSignal=True, selSignal=False }
    decodeInstruction (Load B) = def{ wrSignal=True, selSignal=True }
    decodeInstruction Out      = def{ oeSignal=True }

-- |Processor signal lines define, this is using for manual connect to 
-- signal bus on net level, and also microcode mapping on line. In the future this class
-- will be recycle to make process automation.
instance Connected (Multiplier v x t) where
    data PUPorts (Multiplier v x t)
        = PUPorts
            { wr           -- ˆUpload argument
            , wrSel        -- ˆSelect uploading argument (A | B).
            , oe :: Signal -- ˆDownload work result
            } deriving ( Show )
    transmitToLink Microcode{..} PUPorts{..}
        =
            [ (wr, Bool wrSignal)
            , (wrSel, Bool selSignal)
            , (oe, Bool oeSignal)
            ]

--|The availability of standart values, with which actual result of processor in simlator 
-- is compared, has the main role in testing. This class carry on Standart values generation. 

instance ( Var v
         , Integral x
         ) => Simulatable (Multiplier v x t) v x where
    simulateOn cntx _ f
        -- We define the function and delegate its calculation to default realization.
        | Just f'@F.Multiply{} <- F.castF f = simulate cntx f'
        | otherwise = error $ "Can't simultate on Multiplier: " ++ show f


--| We use functions that is realized below to generate processors and tests, that use this 
-- processor. These methods are called while generation of project with net, that include this 
-- processor or also with tests generation.
instance ( Time t, Var v
         ) => TargetSystemComponent (Multiplier v x t) where
	--| Naming of hardwawre module, instance of which is creting for embedding to processor.
	-- In this case it is defined in @/hdl/multiplier/pu_multiplier.v@.
	moduleName _title _pu = "pu_multiplier"

	--| Processors software generator. In case of multiplier this is no software. 
	--Let's figure it out. Before we said, that software has two components:
	--
	-- 1.	Setting and begin states. In case of multiplier there is no specific settings  
	-- 		for the applied algorhytm.
	-- 2. 	Microprogram. Processor cannot be user not in processor ner sructure, we needn't to
	--		determine software in context of separate unit. Besides, signal lines of separated 
	--		processors can be multiplexed. Thereby, microprogram is formed for
	--		processors net just at once in way of merge of the microprogramms, that are
	--		generated on the base of computational planning description
	--		(look. 'NITTA.BusNetwork').
    software _ _ = Empty
    
    --	|Processor hardware generator. In case of multiplier, there is no generation.
    --	Multiplier is described by two files: (1) directly multiplier, that is realized
    --	by IP kernel or functional stub. (2) module. that realize interface between
    --	multuplier and processors infostructure.
    hardware title pu@Multiplier{ isMocked }
        = Aggregate Nothing
            [ if isMocked
                then FromLibrary "multiplier/mult_mock.v"
                else FromLibrary "multiplier/mult_inner.v"
            , FromLibrary $ "multiplier/" ++ moduleName title pu ++ ".v"
            ]
    

    --	|Source code fragment generation for create processor instance within the processorю 
    -- 	The main task of the function is to include processor to processor infostructure correctly.
	--	and set uo all parameters, names and wires.
    hardwareInstance title _pu Enviroment{ net=NetEnv{..}, signalClk, signalRst } PUPorts{..}
        = [qq|pu_multiplier #
        ( .DATA_WIDTH( $parameterDataWidth )
        , .ATTR_WIDTH( $parameterAttrWidth )
        , .INVALID( 0 )  // FIXME: Сделать и протестировать работу с атрибутами.
        ) $title
    ( .clk( $signalClk )
    , .rst( $signalRst )
    , .signal_wr( {signal wr} )
    , .signal_sel( {signal wrSel} )
    , .data_in( $dataIn )
    , .attr_in( $attrIn )
    , .signal_oe( {signal oe} )
    , .data_out( $dataOut )
    , .attr_out( $attrOut )
    );|]	




































