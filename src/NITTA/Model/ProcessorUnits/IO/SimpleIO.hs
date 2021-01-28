{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.Model.ProcessorUnits.IO.SimpleIO
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.IO.SimpleIO (
    SimpleIOInterface,
    SimpleIO (..),
    Ports (..),
) where

import Control.Monad
import Data.Default
import qualified Data.List as L
import Data.Maybe
import qualified Data.Set as S
import Data.Typeable
import qualified NITTA.Intermediate.Functions as F
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Types
import NITTA.Utils
import NITTA.Utils.ProcessDescription
import Numeric.Interval.NonEmpty (sup, (...))
import Text.InterpolatedString.Perl6 (qc)

class (Typeable i) => SimpleIOInterface i

data SimpleIO i v x t = SimpleIO
    { bounceFilter :: Int
    , -- |if 'Nothing' then size should defined by algorithm
      bufferSize :: Maybe Int
    , receiveQueue :: [Q v x]
    , receiveN :: Int
    , -- |set if send buffer overlap receive buffer
      isReceiveOver :: Bool
    , sendQueue :: [Q v x]
    , sendN :: Int
    , process_ :: Process v x t
    }

instance (VarValTime v x t, SimpleIOInterface i) => Show (SimpleIO i v x t) where
    show io =
        codeBlock
            [qc|
        bounceFilter  = {bounceFilter io}
        bufferSize    = {bufferSize io}
        receiveQueue  = {receiveQueue io}
        receiveN      = {receiveN io}
        isReceiveOver = {isReceiveOver io}
        sendQueue     = {sendQueue io}
        sendN         = {sendN io}
        process_      =
            {inline $ show $ process_ io}
        |]

data Q v x = Q {vars :: [v], function :: F v x, cads :: [ProcessStepID]}
    deriving (Show)

instance
    ( VarValTime v x t
    , SimpleIOInterface i
    ) =>
    ProcessorUnit (SimpleIO i v x t) v x t
    where
    tryBind f sio@SimpleIO{sendQueue, receiveQueue, receiveN, sendN, bufferSize}
        | Just F.Receive{} <- castF f
          , fromMaybe maxBound bufferSize == receiveN =
            Left "IO process unit to small buffer size"
        | Just F.Send{} <- castF f
          , fromMaybe maxBound bufferSize == sendN =
            Left "IO process unit to small buffer size"
        | Just (F.Receive (O vs)) <- castF f
          , let (cads, process_) = runSchedule sio $ scheduleFunctionBind f =
            Right
                sio
                    { receiveQueue = Q{vars = S.elems vs, function = f, cads} : receiveQueue
                    , receiveN = receiveN + 1
                    , process_
                    }
        | Just (F.Send (I v)) <- castF f
          , let (cads, process_) = runSchedule sio $ scheduleFunctionBind f =
            Right
                sio
                    { sendQueue = Q{vars = [v], function = f, cads} : sendQueue
                    , sendN = sendN + 1
                    , process_
                    }
        | otherwise = Left $ "IO processor unit do not support: " ++ show f

    process = process_

instance BreakLoopProblem (SimpleIO i v x t) v x
instance OptimizeAccumProblem (SimpleIO i v x t) v x
instance ResolveDeadlockProblem (SimpleIO i v x t) v x

instance
    ( VarValTime v x t
    , SimpleIOInterface i
    ) =>
    EndpointProblem (SimpleIO i v x t) v t
    where
    endpointOptions SimpleIO{receiveQueue, sendQueue, process_ = Process{nextTick}} =
        let source vs = EndpointSt (Source $ S.fromList vs) $ TimeConstrain (nextTick + 1 ... maxBound) (1 ... maxBound)
            receiveOpts = map (source . vars) receiveQueue

            target v = EndpointSt (Target v) $ TimeConstrain (nextTick ... maxBound) (1 ... 1)
            sendOpts = map (target . head . vars) sendQueue
         in receiveOpts ++ sendOpts

    endpointDecision sio@SimpleIO{receiveQueue} d@EndpointSt{epRole = Source vs, epAt}
        | ([q@Q{function, vars = allVars}], receiveQueue') <-
            L.partition ((vs `S.isSubsetOf`) . S.fromList . vars) receiveQueue
          , let remainVars = allVars L.\\ S.elems vs
                process_ = execSchedule sio $ do
                    void $ scheduleEndpoint d $ scheduleInstruction (shiftI 0 epAt) $ Receiving $ null remainVars
                    when (null remainVars) $ void $ scheduleFunction epAt function
                    updateTick (sup epAt + 1)
                    return ()
                receiveQueue'' =
                    if null remainVars
                        then receiveQueue'
                        else q{vars = remainVars} : receiveQueue' =
            sio{receiveQueue = receiveQueue'', process_}
    endpointDecision sio@SimpleIO{sendQueue, sendN, receiveQueue, receiveN} d@EndpointSt{epRole = Target v, epAt}
        | ([Q{function}], sendQueue') <- L.partition ((v ==) . head . vars) sendQueue
          , let (_, process_) = runSchedule sio $ do
                    _ <- scheduleEndpoint d $ scheduleInstruction epAt Sending
                    updateTick (sup epAt + 1)
                    scheduleFunction epAt function =
            sio
                { sendQueue = sendQueue'
                , isReceiveOver = (sendN - length sendQueue) >= (receiveN - length receiveQueue)
                , process_
                }
    endpointDecision sio d = error $ "SPI model internal error; decision: " ++ show d ++ "\nSPI model: \n" ++ show sio

{- |Access to received data buffer was implemented like a queue. OE signal read
received value multiple times __without changing__ "pointer" to the next value.
OE and WR signals simultaneously read received value and __increment__ "pointer"
to the next value. We do that for the reduced number of signal lines.

Example:

1. Nop - do nothing;

2. Send (WR signal) - read a value from data_bus to send buffer[0], pointer
   increments automatically.

3. Send (WR signal) - read a value from data_bus to send buffer[1], pointer
   increments automatically.

4. Nop - do nothing.

5. Receive False (OE signal) - write a value to data_bus from receive buffer[0]
   without pointer changing.

6. Receive True (OE and WR signal) - write a value to data_bus from receive
   buffer[0] with pointer increment.
-}
instance Controllable (SimpleIO i v x t) where
    data Instruction (SimpleIO i v x t)
        = Receiving Bool
        | Sending
        deriving (Show)

    data Microcode (SimpleIO i v x t) = Microcode
        { wrSignal :: Bool
        , oeSignal :: Bool
        }
        deriving (Show, Eq, Ord)

    mapMicrocodeToPorts Microcode{..} SimpleIOPorts{..} =
        [ (wr, Bool wrSignal)
        , (oe, Bool oeSignal)
        ]

    portsToSignals SimpleIOPorts{wr, oe} = [wr, oe]

    signalsToPorts (wr : oe : _) _ = SimpleIOPorts wr oe "stop"
    signalsToPorts _ _ = error "pattern match error in signalsToPorts SimpleIOPorts"

instance Default (Microcode (SimpleIO i v x t)) where
    def =
        Microcode
            { wrSignal = False
            , oeSignal = False
            }

instance UnambiguouslyDecode (SimpleIO i v x t) where
    decodeInstruction Sending = def{wrSignal = True}
    decodeInstruction (Receiving next) = def{oeSignal = True, wrSignal = next}

instance Connected (SimpleIO i v x t) where
    data Ports (SimpleIO i v x t) = SimpleIOPorts
        { wr, oe :: SignalTag
        , -- |this flag which indicates an end of the data transaction
          -- requires for stop computational process while data transferring
          -- to avoid loses
          stop :: String
        }
        deriving (Show)

instance (Var v) => Locks (SimpleIO i v x t) v where
    locks SimpleIO{} = []
