{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Shift
Description : Shift Processor Unit
Copyright   : (c) Daniil Prohorov, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Shift (
    Shift,
    Ports (..),
    IOPorts (..),
    shift,
) where

import Control.Monad (when)
import Data.Default
import Data.List (find, (\\))
import Data.Set (elems, fromList, member)
import Data.String.Interpolate
import NITTA.Intermediate.Functions hiding (remain)
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Time
import NITTA.Project
import NITTA.Utils
import NITTA.Utils.ProcessDescription
import Numeric.Interval.NonEmpty (inf, singleton, sup, (...))
import Prelude hiding (init)

data Shift v x t = Shift
    { remain :: [F v x]
    -- ^ list of FU, that will be bound later
    , target :: Maybe v
    -- ^ current input value, that we want to shift
    , sources :: [v]
    -- ^ list of output values
    , sRight :: Bool
    -- ^ True -> shift right; False -> shift left
    , byteShiftDiv :: Int
    -- ^ shift div 8 (is used for byte shift)
    , byteShiftMod :: Int
    -- ^ shift mod 8 (is used for bit shift)
    , currentWork :: Maybe (F v x)
    -- ^ current function in PU
    , process_ :: Process t (StepInfo v x t)
    -- ^ description of target computation process
    }

instance Var v => Locks (Shift v x t) v where
    locks Shift{sources, target = Just t} =
        [ Lock{lockBy = t, locked}
        | locked <- sources
        ]
    locks Shift{target = Nothing} = []

shift sRight =
    Shift
        { remain = []
        , target = Nothing
        , sources = []
        , sRight
        , byteShiftDiv = 0
        , byteShiftMod = 0
        , currentWork = Nothing
        , process_ = def
        }

instance Default t => Default (Shift v x t) where
    def = shift True

instance BreakLoopProblem (Shift v x t) v x
instance ConstantFoldingProblem (Shift v x t) v x
instance OptimizeAccumProblem (Shift v x t) v x
instance OptimizeLutProblem (Shift v x t) v x
instance ResolveDeadlockProblem (Shift v x t) v x

instance VarValTime v x t => ProcessorUnit (Shift v x t) v x t where
    tryBind f pu@Shift{remain}
        | Just f' <- castF f =
            case f' of
                ShiftL{} -> Right pu{remain = f : remain}
                ShiftR{} -> Right pu{remain = f : remain}
        | otherwise = Left $ "The function is unsupported by Shift: " ++ show f
    process = process_

-- | This function carry out actual take functional block to work.
execution pu@Shift{target = Nothing, sources = [], remain} f
    | Just f' <- castF f =
        case f' of
            ShiftL s (I i_) (O o) -> toPU i_ o False s
            ShiftR s (I i_) (O o) -> toPU i_ o True s
    where
        toPU inp out sRight step =
            pu
                { target = Just inp
                , currentWork = Just f
                , sources = elems out
                , remain = remain \\ [f]
                , sRight = sRight
                , byteShiftDiv = step `div` 8
                , byteShiftMod = step `mod` 8
                }
execution _ _ = error "Not right arguments in execution function in shift module"

instance VarValTime v x t => EndpointProblem (Shift v x t) v t where
    endpointOptions pu@Shift{target = Just t} =
        [EndpointSt (Target t) $ TimeConstraint (nextTick pu ... maxBound) (singleton 1)]
    endpointOptions pu@Shift{sources, byteShiftDiv, byteShiftMod}
        | not $ null sources
        , byteShiftDiv == 0 =
            let timeConstrain = TimeConstraint (startTime ... maxBound) (1 ... maxBound)
                startTime = nextTick pu + fromIntegral byteShiftMod + 2
             in [EndpointSt (Source $ fromList sources) timeConstrain]
        | not $ null sources =
            let endByteShift = nextTick pu + fromIntegral byteShiftDiv
                timeConstrain = TimeConstraint (startTime ... maxBound) (1 ... maxBound)
                startTime = endByteShift + fromIntegral byteShiftMod + 2
             in [EndpointSt (Source $ fromList sources) timeConstrain]
    endpointOptions pu@Shift{remain} = concatMap (endpointOptions . execution pu) remain

    endpointDecision
        pu@Shift
            { target = (Just _)
            , sRight
            , byteShiftDiv
            , byteShiftMod
            }
        d@EndpointSt
            { epRole = Target _
            , epAt
            } =
            let startByteShift = inf epAt + 1
                numByteShiftMod = fromIntegral byteShiftMod
                endByteShift = sup epAt + fromIntegral byteShiftDiv
                process_' = execSchedule pu $ do
                    scheduleEndpoint d $ do
                        scheduleInstructionUnsafe_ epAt Init
                        case (byteShiftDiv, byteShiftMod) of
                            (0, _) ->
                                scheduleInstructionUnsafe
                                    (inf epAt + 1 ... sup epAt + numByteShiftMod)
                                    Work{shiftRight = sRight, stepByte = False, shiftType = Logic}
                            (_, 0) ->
                                scheduleInstructionUnsafe
                                    (startByteShift ... endByteShift)
                                    Work{shiftRight = sRight, stepByte = True, shiftType = Logic}
                            _ ->
                                do
                                    _ <-
                                        scheduleInstructionUnsafe
                                            (startByteShift ... endByteShift)
                                            Work{shiftRight = sRight, stepByte = True, shiftType = Logic}
                                    scheduleInstructionUnsafe
                                        (endByteShift + 1 ... endByteShift + numByteShiftMod)
                                        Work{shiftRight = sRight, stepByte = False, shiftType = Logic}
             in pu
                    { process_ = process_'
                    , target = Nothing
                    }
    endpointDecision
        pu@Shift
            { target = Nothing
            , sources
            , currentWork = Just f
            , process_
            }
        d@EndpointSt
            { epRole = Source v
            , epAt
            }
            | not $ null sources
            , let sources' = sources \\ elems v
            , let a = inf $ stepsInterval $ relatedEndpoints process_ $ variables f
            , sources' /= sources =
                let process_' = execSchedule pu $ do
                        endpoints <- scheduleEndpoint d $ scheduleInstructionUnsafe (shiftI (-1) epAt) Out
                        when (null sources') $ do
                            -- FIXME: here ([]) you can see the source of error.
                            -- Function don't connected to bind step. It should be fixed.
                            scheduleFunctionFinish_ [] f $ a ... sup epAt
                        return endpoints
                 in pu
                        { process_ = process_'
                        , sources = sources'
                        , currentWork = if null sources' then Nothing else Just f
                        }
    endpointDecision pu@Shift{target = Nothing, sources = [], remain} d
        | let v = oneOf $ variables d
        , Just f <- find (\f -> v `member` variables f) remain =
            endpointDecision (execution pu f) d
    endpointDecision _pu d = error [i|incorrect decision #{ d } for Shift|]

data Mode = Logic | Arithmetic deriving (Show, Eq)

instance Controllable (Shift v x t) where
    data Instruction (Shift v x t)
        = Init
        | Work
            { shiftRight :: Bool
            , stepByte :: Bool
            , shiftType :: Mode
            }
        | Out
        deriving (Show)

    data Microcode (Shift v x t) = Microcode
        { workSignal :: Bool
        , directionSignal :: Bool
        , modeSignal :: Bool
        , stepSignal :: Bool
        , initSignal :: Bool
        , oeSignal :: Bool
        }
        deriving (Show, Eq, Ord)

    zipSignalTagsAndValues ShiftPorts{..} Microcode{..} =
        [ (work, Bool workSignal)
        , (direction, Bool directionSignal)
        , (mode, Bool modeSignal)
        , (step, Bool stepSignal)
        , (init, Bool initSignal)
        , (oe, Bool oeSignal)
        ]

    usedPortTags ShiftPorts{work, direction, mode, step, init, oe} =
        [work, direction, mode, step, init, oe]

    takePortTags (work : direction : mode : step : init : oe : _) _ = ShiftPorts work direction mode step init oe
    takePortTags _ _ = error "can not take port tags, tags are over"

instance Default (Microcode (Shift v x t)) where
    def =
        Microcode
            { workSignal = False
            , directionSignal = False
            , modeSignal = False
            , stepSignal = True
            , initSignal = False
            , oeSignal = False
            }

instance UnambiguouslyDecode (Shift v x t) where
    decodeInstruction Init = def{initSignal = True}
    decodeInstruction Out = def{oeSignal = True}
    decodeInstruction (Work toRight step mode) =
        def
            { workSignal = True
            , directionSignal = not toRight
            , modeSignal = mode == Arithmetic
            , stepSignal = step
            }

instance Connected (Shift v x t) where
    data Ports (Shift v x t) = ShiftPorts {work, direction, mode, step, init, oe :: SignalTag}
        deriving (Show)

instance IOConnected (Shift v x t) where
    data IOPorts (Shift v x t) = ShiftIO

instance Val x => TargetSystemComponent (Shift v x t) where
    moduleName _ _ = "pu_shift"
    hardware _tag _pu = FromLibrary "pu_shift.v"
    software _ _ = Empty
    hardwareInstance
        tag
        _pu
        UnitEnv
            { sigClk
            , ctrlPorts = Just ShiftPorts{..}
            , valueIn = Just (dataIn, attrIn)
            , valueOut = Just (dataOut, attrOut)
            } =
            [__i|
                pu_shift \#
                        ( .DATA_WIDTH( #{ dataWidth (def :: x) } )
                        , .ATTR_WIDTH( #{ attrWidth (def :: x) } )
                        ) #{ tag }
                    ( .clk( #{ sigClk } )
                    , .signal_work( #{ work } ), .signal_direction( #{ direction } )
                    , .signal_mode( #{ mode } ), .signal_step( #{ step } )
                    , .signal_init( #{ init } ), .signal_oe( #{ oe } )
                    , .data_in( #{ dataIn } )
                    , .attr_in( #{ attrIn } )
                    , .data_out( #{ dataOut } )
                    , .attr_out( #{ attrOut } )
                );
            |]
    hardwareInstance _title _pu _env = error "internal error"

instance IOTestBench (Shift v x t) v x
