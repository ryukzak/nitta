{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


{-|
Module      : NITTA.Model.ProcessorUnits.Serial.Shift
Description : Shift Processor Unit
Copyright   : (c) Daniil Prohorov, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Shift
  ( Shift
  , Ports(..), IOPorts(..)
  )
  where

import           Control.Monad ( when )
import           Data.Bits ( finiteBitSize )
import           Data.Default
import           Data.List ( find, (\\) )
import           Data.Set ( elems, fromList, member )
import           NITTA.Intermediate.Functions hiding ( remain )
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.Types
import           NITTA.Project
import           NITTA.Utils
import           NITTA.Utils.ProcessDescription
import           Numeric.Interval ( inf, singleton, sup, (...) )
import           Prelude hiding ( init )
import           Text.InterpolatedString.Perl6 ( qc )

data Shift v x t = Shift
    { remain               :: [ F v x ]
    , target               :: Maybe v
    , sources              :: [ v ]
    , sRight               :: Bool
    , byteShiftDiv         :: Int
    , byteShiftMod         :: Int
    , currentWork          :: Maybe ( t, F v x )
    , currentWorkEndpoints :: [ ProcessStepID ]
    , process_             :: Process v x t
    } deriving (Show)

instance ( Var v ) => Locks (Shift v x t) v where
    locks Shift{ sources, target=Just t } =
        [ Lock{ lockBy=t, locked }
        | locked <- sources
        ]
    locks Shift{ target=Nothing } = []

instance Default t => Default (Shift v x t) where
  def = Shift
        { remain=[]
        , target=Nothing
        , sources=[]
        , sRight=True
        , byteShiftDiv=0
        , byteShiftMod=0
        , currentWork=Nothing
        , currentWorkEndpoints=[]
        , process_=def
        }

instance RefactorProblem (Shift v x t) v x

instance ( VarValTime v x t
         ) => ProcessorUnit (Shift v x t) v x t where
    tryBind f pu@Shift{ remain }
        | Just f' <- castF f
        = case f' of
            ShiftL{} -> Right pu{ remain=f : remain }
            ShiftR{} -> Right pu{ remain=f : remain }

        | otherwise = Left $ "The function is unsupported by Shift: " ++ show f
    process = process_

-- |This function carry out actual take functional block to work.
execution pu@Shift{ target=Nothing, sources=[], remain, process_} f
    | Just f' <- castF f
    = case f' of
        ShiftL s (I i) (O o) -> toPU i o False s
        ShiftR s (I i) (O o) -> toPU i o True s

      where
          toPU inp out sRight step = pu
              { target=Just inp
              , currentWork=Just (nextTick process_, f)
              , sources=elems out
              , remain=remain \\ [ f ]
              , sRight=sRight
              , byteShiftDiv = step `div` 8
              , byteShiftMod = step `mod` 8
              }
execution _ _ = error "Shift: internal execution error."

instance ( VarValTime v x t
        ) => EndpointProblem (Shift v x t) v t
        where
    endpointOptions Shift{ target=Just t, process_ }
        = [ EndpointSt (Target t) $ TimeConstrain (nextTick process_ ... maxBound) (singleton 1) ]

    endpointOptions Shift{ sources, process_, byteShiftDiv, byteShiftMod }
        | not $ null sources
        , byteShiftDiv == 0
        = let
            timeConstrain = TimeConstrain (startTime ... maxBound) (1 ... maxBound)
            startTime = (nextTick process_) + fromIntegral byteShiftMod + 2
        in
            [ EndpointSt (Source $ fromList sources) timeConstrain ]

        | not $ null sources
        = let
            endByteShift = nextTick process_ + fromIntegral byteShiftDiv
            timeConstrain = TimeConstrain (startTime ... maxBound) (1 ... maxBound)
            startTime = endByteShift + fromIntegral byteShiftMod + 2
        in
            [ EndpointSt (Source $ fromList sources) timeConstrain ]

    endpointOptions pu@Shift{ remain } = concatMap (endpointOptions . execution pu) remain

    endpointDecision
        pu@Shift
            { target=(Just _)
            , currentWorkEndpoints
            , sRight
            , byteShiftDiv
            , byteShiftMod
            }
        d@EndpointSt
            { epRole=Target _
            , epAt
            }
        = let
            startByteShift = inf epAt + 1
            numByteShiftMod = fromIntegral byteShiftMod
            endByteShift = sup epAt + fromIntegral byteShiftDiv
            (newEndpoints, process_') = runSchedule pu $ do
                updateTick (sup epAt)
                scheduleEndpoint d $ do
                    _ <- scheduleInstruction epAt Init
                    case (byteShiftDiv, byteShiftMod) of
                        (0, _) ->
                            scheduleInstruction
                                (inf epAt + 1 ... sup epAt + numByteShiftMod)
                                (Work sRight False Logic)
                        (_, 0) ->
                            scheduleInstruction
                                (startByteShift ... endByteShift)
                                (Work sRight True Logic)
                        _      ->
                            do
                                _ <- scheduleInstruction
                                    (startByteShift ... endByteShift)
                                    (Work sRight True Logic)
                                scheduleInstruction
                                    (endByteShift + 1 ... endByteShift + numByteShiftMod)
                                    (Work sRight False Logic)
        in
            pu
                { process_=process_'
                , target=Nothing
                , currentWorkEndpoints=newEndpoints ++ currentWorkEndpoints
                }

    endpointDecision
        pu@Shift
            { target=Nothing
            , sources
            , currentWork=Just (a, f)
            , currentWorkEndpoints
            }
        d@EndpointSt
            { epRole=Source v
            , epAt
            }

        | not $ null sources
        , let sources' = sources \\ elems v
        , sources' /= sources
        = let
            (newEndpoints, process_') = runSchedule pu $ do
                updateTick (sup epAt)
                endpoints <- scheduleEndpoint d $ scheduleInstruction (shiftI (-1) epAt) Out
                when (null sources') $ do
                    high <- scheduleFunction (a ... sup epAt) f
                    let low = endpoints ++ currentWorkEndpoints
                    establishVerticalRelations high low
                return endpoints
        in
            pu
                { process_=process_'
                , sources=sources'
                , currentWork=if null sources' then Nothing else Just (a+1, f)
                , currentWorkEndpoints=if null sources' then [] else newEndpoints ++ currentWorkEndpoints
                }

    endpointDecision pu@Shift{ target=Nothing, sources=[], remain } d
        | let v = oneOf $ variables d
        , Just f <- find (\f -> v `member` variables f) remain
        = endpointDecision (execution pu f) d

    endpointDecision pu d = error $ "Shift decision error\npu: " ++ show pu ++ ";\n decison:" ++ show d

data Mode = Logic | Arithmetic deriving ( Show, Eq )

instance Controllable (Shift v x t) where
    data Instruction (Shift v x t)
        = Init
        | Work Bool Bool Mode
        | Out
        deriving (Show)

    data Microcode (Shift v x t)
        = Microcode
            { workSignal :: Bool
            , directionSignal :: Bool
            , modeSignal :: Bool
            , stepSignal :: Bool
            , initSignal :: Bool
            , oeSignal :: Bool
            } deriving ( Show, Eq, Ord )

    mapMicrocodeToPorts Microcode{..} ShiftPorts{..} =
        [ (work, Bool workSignal)
        , (direction, Bool directionSignal)
        , (mode, Bool modeSignal)
        , (step, Bool stepSignal)
        , (init, Bool initSignal)
        , (oe, Bool oeSignal)
        ]

    portsToSignals ShiftPorts{ work, direction, mode, step, init, oe}
        = [work, direction, mode, step, init, oe]

    signalsToPorts (work:direction:mode:step:init:oe:_) _ = ShiftPorts work direction mode step init oe
    signalsToPorts _                                    _ = error "pattern match error in signalsToPorts ShiftPorts"

instance Default (Microcode (Shift v x t)) where
  def = Microcode{ workSignal=False
                 , directionSignal=False
                 , modeSignal=False
                 , stepSignal=True
                 , initSignal=False
                 , oeSignal=False
                 }

instance UnambiguouslyDecode (Shift v x t) where
  decodeInstruction Init = def{ initSignal=True }
  decodeInstruction Out = def{ oeSignal=True }
  decodeInstruction (Work toRight step mode)
    = def{ workSignal=True
         , directionSignal=not toRight
         , modeSignal=mode == Arithmetic
         , stepSignal=step
         }


instance Connected (Shift v x t) where
  data Ports (Shift v x t)
    = ShiftPorts{ work, direction, mode, step, init, oe :: SignalTag } deriving ( Show )

instance IOConnected (Shift v x t) where
  data IOPorts (Shift v x t) = ShiftIO


instance ( Val x ) => TargetSystemComponent (Shift v x t) where
    moduleName _ _ = "pu_shift"
    hardware tag pu = FromLibrary $ moduleName tag pu ++ ".v"
    software _ _ = Empty
    hardwareInstance tag _pu TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk } ShiftPorts{..} ShiftIO
        = codeBlock [qc|
            pu_shift #
                    ( .DATA_WIDTH( { finiteBitSize (def :: x) } )
                    , .ATTR_WIDTH( { show parameterAttrWidth } )
                    ) { tag }
                ( .clk( { signalClk } )
                , .signal_work( { signal work } ), .signal_direction( { signal direction } )
                , .signal_mode( { signal mode } ), .signal_step( { signal step } )
                , .signal_init( { signal init } ), .signal_oe( { signal oe } )
                , .data_in( { dataIn } )
                , .attr_in( { attrIn } )
                , .data_out( { dataOut } )
                , .attr_out( { attrOut } )
                );
            |]

    hardwareInstance _title _pu TargetEnvironment{ unitEnv=NetworkEnv{} } _ports _op
        = error "Should be defined in network."

instance IOTestBench (Shift v x t) v x
