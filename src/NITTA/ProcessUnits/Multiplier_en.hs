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


