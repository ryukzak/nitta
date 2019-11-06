{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE QuasiQuotes           #-}
module NITTA.Model.ProcessorUnits.Serial.Accum
  ( Accum
  , accum
  , Ports(..), IOPorts(..)
  ) where

import           Control.Monad                    (when)
import           Data.Bits                        (finiteBitSize)
import           Data.Default
import           Data.List                        (find, partition, (\\))
import           Data.Set                         (elems, fromList, member)
import qualified NITTA.Intermediate.Functions     as F
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.Problems.Refactor
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Snippets
import           NITTA.Project.Types
import           NITTA.Utils
import           NITTA.Utils.ProcessDescription
import           Numeric.Interval                 (singleton, sup, (...))
import           Text.InterpolatedString.Perl6    (qc)
import           Debug.Trace (trace)

{--
>>> :module +NITTA.Model.Types NITTA.Intermediate.Functions Numeric.Interval Data.Set
>>> :set prompt "\ESC[34mλ> \ESC[m"

>>> f = add "a" "b" ["c", "d"] :: F String Int
>>> f
c = d = a * b

>>> st0 = accum :: Accum String Int Int
>>> st0
Accum {remain = [], targets = [], sources = [], doneAt = Nothing, currentWork = Nothing, currentWorkEndpoints = [], process_ = Process {steps = [], relations = [], nextTick = 0, nextUid = 0}, tick = 0}

>>> endpointOptions st0
[]

>>> Right st1 = tryBind f st0
>>> st1
Accum {remain = [c = d = a + b], targets = [], sources = [], doneAt = Nothing, currentWork = Nothing, currentWorkEndpoints = [], process_ = Process {steps = [], relations = [], nextTick = 0, nextUid = 0}, tick = 0}

>>> endpointOptions st1
[?Target "a"@(1..∞ /P 1..∞),?Target "b"@(1..∞ /P 1..∞)]

>>> st2 = endpointDecision st1 $ EndpointD (Target "a") (0...2)
>>> st2
Accum {remain = [], targets = ["b"], sources = ["c","d"], doneAt = Nothing, currentWork = Just (1,c = d = a + b), currentWorkEndpoints = [0], process_ = Process {steps = [Step {sKey = 1, sTime = 0 ... 2, sDesc = Load A},Step {sKey = 0, sTime = 0 ... 2, sDesc = Target "a"}], relations = [Vertical 0 1], nextTick = 2, nextUid = 2}, tick = 2}

>>> endpointOptions st2
[?Target "b"@(3..∞ /P 1..∞)

>>> st3 = endpointDecision st2 $ EndpointD (Target "b") (3...3)
>>> st3
Accum {remain = [], targets = [], sources = ["c","d"], doneAt = Just 6, currentWork = Just (1,c = d = a + b), currentWorkEndpoints = [2,0], process_ = Process {steps = [Step {sKey = 3, sTime = 3 ... 3, sDesc = Load B},Step {sKey = 2, sTime = 3 ... 3, sDesc = Target "b"},Step {sKey = 1, sTime = 0 ... 2, sDesc = Load A},Step {sKey = 0, sTime = 0 ... 2, sDesc = Target "a"}], relations = [Vertical 2 3,Vertical 0 1], nextTick = 3, nextUid = 4}, tick = 3}

>>> endpointOptions st3
[?Source "c","d"@(6..∞ /P 1..∞)]

>>> st4 = endpointDecision st3 $ EndpointD (Source $ fromList ["c"]) (6...6)
>>> st4
Accum {remain = [], targets = [], sources = ["d"], doneAt = Just 6, currentWork = Just (1,c = d = a + b), currentWorkEndpoints = [4,2,0], process_ = Process {steps = [Step {sKey = 5, sTime = 6 ... 6, sDesc = Out},Step {sKey = 4, sTime = 6 ... 6, sDesc = Source "c"},Step {sKey = 3, sTime = 3 ... 3, sDesc = Load B},Step {sKey = 2, sTime = 3 ... 3, sDesc = Target "b"},Step {sKey = 1, sTime = 0 ... 2, sDesc = Load A},Step {sKey = 0, sTime = 0 ... 2, sDesc = Target "a"}], relations = [Vertical 4 5,Vertical 2 3,Vertical 0 1], nextTick = 6, nextUid = 6}, tick = 6}

>>> endpointOptions st4
[?Source "d"@(7..∞ /P 1..∞)]

>>> st5 = endpointDecision st4 $ EndpointD (Source $ fromList ["d"]) (7...7)
>>> st5
Accum {remain = [], targets = [], sources = [], doneAt = Nothing, currentWork = Nothing, currentWorkEndpoints = [], process_ = Process {steps = [Step {sKey = 8, sTime = 1 ... 7, sDesc = "c" = "d" = "a" + "b"},Step {sKey = 7, sTime = 7 ... 7, sDesc = Out},Step {sKey = 6, sTime = 7 ... 7, sDesc = Source "d"},Step {sKey = 5, sTime = 6 ... 6, sDesc = Out},Step {sKey = 4, sTime = 6 ... 6, sDesc = Source "c"},Step {sKey = 3, sTime = 3 ... 3, sDesc = Load B},Step {sKey = 2, sTime = 3 ... 3, sDesc = Target "b"},Step {sKey = 1, sTime = 0 ... 2, sDesc = Load A},Step {sKey = 0, sTime = 0 ... 2, sDesc = Target "a"}], relations = [Vertical 8 6,Vertical 8 4,Vertical 8 2,Vertical 8 0,Vertical 6 7,Vertical 4 5,Vertical 2 3,Vertical 0 1], nextTick = 7, nextUid = 9}, tick = 7}

>>> endpointOptions st5
[]
--}
data Accum v x t = Accum
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
    , targets              :: [ (Bool, v) ]
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
    -- ('NITTA.Model.ProcessorUnits.Types')
    , process_             :: Process v x t
    , tick                 :: t
    -- |check is it sub or add
    , isNeg                :: Bool
    }
    deriving ( Show )

accum :: (VarValTime v x t) => Accum v x t
accum = Accum
    { remain=[]
    , targets=[]
    , sources=[]
    , doneAt=Nothing
    , currentWork=Nothing
    , currentWorkEndpoints=[]
    , process_=def
    , tick=def
    }

--
instance ( VarValTime v x t
         ) => ProcessorUnit (Accum v x t) v x t where
    -- Binding to mUnit is carried out by this function.
    tryBind f pu@Accum{ remain }
        -- To do this, it is checked whether the function type is reduced to one of the supported
        -- by ('NITTA.FunctionalBlocks.castF')  and in case of success model conditions is returned
        -- after binding with 'Right' mark.
        --
        -- Important to notice, that "binding" doesn't mean actually beginning of work, that
        -- allows firstly make bindings of all tasks and after plan computation process.
            | Just F.Add {} <- castF f = Right pu{ remain=f : remain }
            | Just F.Sub {} <- castF f = Right pu{ remain=f : remain }
        -- In case of impossibility of binding string with short description of renouncement
        --cause and 'Left' is returned.
        | otherwise = Left $ "The function is unsupported by Accum: " ++ show f
    tryBind _ _ = error "Try bind to non-zero state. (Accum)"
    -- Unificate interface for get computation process description.
    process = process_
    -- This method is used for set up mUnit time outside.
    -- At the time this is needed only for realisation
    -- of branching, which is on the prototyping stage.
    setTime t pu@Accum{} = pu{ tick=t }


-- |This function carry out actual take functional block to work.
assignment pu@Accum{ targets=[], sources=[], remain, tick } f
    | Just (F.Add (I a) (I b) (O c)) <- castF f
    = pu
        { targets=[(False, a), (False, b)]
        , currentWork=Just (tick + 1, f)
        , sources=elems c
        , remain=remain \\ [ f ]
        }
    | Just (F.Sub (I a) (I b) (O c)) <- castF f
    = pu
        { targets=[(False, a), (True, b)]
        , currentWork=Just (tick + 1, f)
        , sources=elems c
        , remain=remain \\ [ f ]
        }
assignment _ _ = error "Accum: internal assignment error."


-- instance ( Time t ) => Default (Accum v x t) where
--     def = accum

{-
Result of planning is description of one computation cycle, which later can be translated to microcode,
directly control mUnit. From NITTA architecture point of view, process can be described as
consistent execution two roles by processoe:

- data source ('Source');
- data target ('Target');

The planning process itself consists of two operations performed in a cycle:
-}
instance ( VarValTime v x t
        ) => EndpointProblem (Accum v x t) v t
        where


  -- stateOptions Accum{ acIn=vs@(_:_) } now
  --   = map (\(_, v) -> EndpointO (Target v) $ TimeConstrain (now ... maxBound) (singleton 1)) vs
  -- stateOptions Accum{ acOut=vs@(_:_) } now -- вывод
  --   = [ EndpointO (Source $ fromList vs) $ TimeConstrain (now + 2 ... maxBound) (1 ... maxBound) ]
  -- stateOptions _ _ = []






    --1. Processors is asked about roles it can realise (in the other words, how computation
    --process can develop). It is realised by @endpointOptions@ function, result of which is
    --one of the further list:

    --list of variants of uploading to mUnit variables, which are needed to function
    --that is in work;
    endpointOptions Accum{ targets=vs@(_:_), tick }
        = map (\v -> EndpointO (Target v) $ TimeConstrain (tick ... maxBound) (singleton 1)) (map snd vs)

     --   list of variants of downloading from mUnit variables;
    endpointOptions Accum{ sources, doneAt=Just at, tick }
        | not $ null sources
        = [ EndpointO (Source $ fromList sources) $ TimeConstrain (tick + 3 ... maxBound) (1 ... maxBound) ]

    -- list of variables of uploading to mUnit variables, upload any one of that
    -- will cause to actual start of working with mathched function.
    endpointOptions pu@Accum{ remain } = concatMap (endpointOptions . assignment pu) remain

    -- Note, that options provided by this function require clarification, because:

    --	1.	They point to not specific moment for work, but to available interval
    --		('NITTA.Types.Base.TimeConstrain'), that describe from and to which time
    -- 		uploading and downloading can be done, and how much time the process can continue.
    -- 	2.	One value can be download from mUnit as several different variables. This can
    --		be done either all at once (on the hardware level the value writed to the bus and
    -- 		read by several processors), as a consistent (firstly value on the bus can be writed for
    --		one mUnit, and after for next one), what should be specified too.


    -- 2. 	Process planning or making decision about compuatation process development to
    --	  	mUnit model state is carried out by @endpointDecision@. Variant transformation
    --		from got from @endpointOptions@ is carried out by CAD outside the mUnit model.
    --		We can distinguish the following solutions:
    --
    --		1. If model wait variable uploading:
    endpointDecision pu@Accum{ targets=vs, currentWorkEndpoints } d@EndpointD{ epdRole=Target v, epdAt }
           -- From the list of uploading value we get a needed value, and remainder is saved
           -- for the next steps.
        | ([(neg, _)], xs) <- partition ((== v) . snd) vs
             -- @sel@ veriable is used for uploading queuing of variable to hardware block, that is
             -- requred because of realisation.
        , let sel = if null xs then Init neg else Load neg
             --  Computation process planning is carried out.
        , let (newEndpoints, process_') = runSchedule pu $ do
                -- this is required for correct work of automatically generated tests,
                -- that takes information about time from Process
                updateTick (sup epdAt)
                scheduleEndpoint d $ scheduleInstruction epdAt sel
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
    endpointDecision pu@Accum{ targets=[], sources, doneAt, currentWork=Just (a, f), currentWorkEndpoints } d@EndpointD{ epdRole=Source v, epdAt }
        | not $ null sources
        , let sources' = sources \\ elems v
        , sources' /= sources
        -- Compututation process planning is carring on.
        , let (newEndpoints, process_') = runSchedule pu $ do
                endpoints <- scheduleEndpoint d $ scheduleInstruction (epdAt-1) Out
                when (null sources') $ do
                    high <- scheduleFunction (a ... sup epdAt) f
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
    --    3. If no function is executed at the moment, then we need to find function in the list
    --    of assigned function, executed it to work and only then make decision
    --    and plan a fragment of computation process with call recursion in situation 1.
    endpointDecision pu@Accum{ targets=[], sources=[], remain } d
        | let v = oneOf $ variables d
        , Just f <- find (\f -> v `member` variables f) remain
        = endpointDecision (assignment pu f) d
    -- If smth went wrong.
    endpointDecision pu d = error $ "Multiplier decision error\npu: " ++ show pu ++ ";\n decison:" ++ show d


instance Controllable (Accum v x t) where
    -- |Instructions for multiplier mUnit controlling. Multiplier can only
    -- upload arguments A and B, and download multiplication result. This construction
    -- are used in computation process planning by 'schedule' function. Instead of them,
    -- there is a @nop@ function - when no actions execute.
    data Instruction (Accum v x t) = Init Bool | Load Bool | Out deriving (Show)
    -- Set of signals for mUnit control and microcode view for
    -- the mUnit
    data Microcode (Accum v x t) =
        Microcode
            { oeSignal :: Bool
            , initSignal :: Bool
            , loadSignal :: Bool
            , negSignal :: Maybe Bool
            } deriving ( Show, Eq, Ord )

    mapMicrocodeToPorts Microcode{..} AccumPorts{..} =
        [ (init, Bool initSignal)
        , (load, Bool loadSignal)
        , (neg, maybe Undef Bool negSignal)
        , (oe, Bool oeSignal)
        ]

    portsToSignals AccumPorts{ init, load, neg, oe } = [init, load, neg, oe]

    signalsToPorts (init:load:neg:oe:_) _ = AccumPorts init load neg oe
    signalsToPorts _                    _ = error "pattern match error in signalsToPorts AccumPorts"



instance Default (Microcode (Accum v x t)) where
  def = Microcode{ oeSignal=False
                 , initSignal=False
                 , loadSignal=False
                 , negSignal=Nothing
                 }

instance UnambiguouslyDecode (Accum v x t) where
  decodeInstruction (Init neg) = def{ initSignal=False, loadSignal=True, negSignal=Just neg }
  decodeInstruction (Load neg) = def{ initSignal=True, loadSignal=True, negSignal=Just neg }
  decodeInstruction Out        = def{ oeSignal=True }


instance ( VarValTime v x t
         , Num x
         ) => Simulatable (Accum v x t) v x where
  simulateOn cntx _ f
    | Just f'@F.Add{} <- castF f = simulate cntx f'
    | Just f'@F.Sub{} <- castF f = simulate cntx f'
    | otherwise = error $ "Can't simulate " ++ show f ++ " on Accum."


instance Connected (Accum v x t) where
    data Ports (Accum v x t)
        = AccumPorts{ init, load, neg, oe :: SignalTag } deriving ( Show )

instance IOConnected (Accum v x t) where
    data IOPorts (Accum v x t) = AccumIO


-- |Tracking internal dependencies on the data generated by the mUnit.
instance ( Var v ) => Locks (Accum v x t) v where
    locks Accum{ remain, sources, targets } =
        -- The dependence of the output of the loaded arguments. If @ sources @ is an empty list,
        -- then there will be no dependencies.
        [ Lock{ lockBy, locked }
        | locked <- sources
        , lockBy <- map snd targets
        ]
        ++
        -- The dependencies of the functions in the queue on the current moment.
        [ Lock{ lockBy, locked }
        | locked <- concatMap (elems . variables) remain
        , lockBy <- sources ++ map snd targets
        ]

instance ( Val x, Default x ) => TargetSystemComponent (Accum v x t) where
    moduleName _ _ = "pu_accum"
    hardware tag pu = FromLibrary $ moduleName tag pu ++ ".v"
    software _ _ = Empty
    hardwareInstance tag _pu TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk, signalRst } AccumPorts{..} AccumIO
        = codeBlock [qc|
            pu_accum #
                    ( .DATA_WIDTH( { finiteBitSize (def :: x) } )
                    , .ATTR_WIDTH( { show parameterAttrWidth } )
                    ) { tag }
                ( .clk( { signalClk } )
                , .rst( { signalRst } )
                , .signal_init( { signal init } )
                , .signal_load( { signal load } )
                , .signal_neg( { signal neg } )
                , .signal_oe( { signal oe } )
                , .data_in( { dataIn } )
                , .attr_in( { attrIn } )
                , .data_out( { dataOut } )
                , .attr_out( { attrOut } )
                );
            |]
    hardwareInstance _title _pu TargetEnvironment{ unitEnv=NetworkEnv{} } _ports _io
        = error "Should be defined in network."


instance ( VarValTime v x t ) => Default (Accum v x t) where
    def = accum

instance IOTestBench (Accum v x t) v x

instance RefactorProblem (Accum v x t) v x
