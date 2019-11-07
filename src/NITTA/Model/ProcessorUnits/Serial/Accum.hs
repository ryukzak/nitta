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

data Accum v x t = Accum
    {
    -- |List of the assigned, but not processed functions. Functions execution
    -- starts with:
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
        | Just F.Add {} <- castF f = Right pu{ remain=f : remain }
        | Just F.Sub {} <- castF f = Right pu{ remain=f : remain }
        | otherwise = Left $ "The function is unsupported by Accum: " ++ show f

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


instance ( VarValTime v x t
        ) => EndpointProblem (Accum v x t) v t
        where

    --1. Processors is asked about roles it can realise (in the other words, how computation
    --process can develop).

    -- |List of variants of uploading to mUnit variables, which are needed to function
    -- that is in work;
    endpointOptions Accum{ targets=vs@(_:_), tick }
        = map (\v -> EndpointO (Target v) $ TimeConstrain (tick ... maxBound) (singleton 1)) (snds vs)

     -- ^list of variants of downloading from mUnit variables;
    endpointOptions Accum{ sources, doneAt=Just at, tick }
        | not $ null sources
        = [ EndpointO (Source $ fromList sources) $ TimeConstrain (tick + 3 ... maxBound) (1 ... maxBound) ]

    -- ^will cause to actual start of working with mathched function.
    endpointOptions pu@Accum{ remain } = concatMap (endpointOptions . assignment pu) remain




    --  1. If model wait variable uploading:
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
    data Instruction (Accum v x t) = Init Bool | Load Bool | Out deriving (Show)
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
        , lockBy <- snds targets
        ]
        ++
        -- The dependencies of the functions in the queue on the current moment.
        [ Lock{ lockBy, locked }
        | locked <- concatMap (elems . variables) remain
        , lockBy <- sources ++ snds targets
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
-- simple function to not write map snd lst
snds = map snd
