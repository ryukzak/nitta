{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
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
    protocolDescription,
) where

import Control.Monad
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Encode.Pretty
import Data.Default
import Data.List qualified as L
import Data.Maybe
import Data.Set qualified as S
import Data.String.Interpolate
import Data.String.ToString
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import Data.Typeable
import GHC.Generics (Generic)
import NITTA.Intermediate.Functions qualified as F
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Time
import NITTA.Project.Types (Implementation (Immediate))
import NITTA.Utils
import NITTA.Utils.ProcessDescription
import Numeric.Interval.NonEmpty ((...))
import Numeric.Interval.NonEmpty qualified as I
import Prettyprinter

class Typeable i => SimpleIOInterface i

data SimpleIO i v x t = SimpleIO
    { bounceFilter :: Int
    , bufferSize :: Maybe Int
    -- ^ if 'Nothing' then size should defined by algorithm
    , receiveQueue :: [Q v x]
    , receiveN :: Int
    , isReceiveOver :: Bool
    -- ^ set if send buffer overlap receive buffer
    , sendQueue :: [Q v x]
    , sendN :: Int
    , process_ :: Process t (StepInfo v x t)
    }

instance (VarValTime v x t, SimpleIOInterface i) => Pretty (SimpleIO i v x t) where
    pretty io =
        [__i|
            SimpleIO:
                bounceFilter: #{ bounceFilter io }
                bufferSize: #{ bufferSize io }
                receiveQueue: #{ receiveQueue io }
                receiveN: #{ receiveN io }
                isReceiveOver: #{ isReceiveOver io }
                sendQueue: #{ sendQueue io }
                sendN: #{ sendN io }
                #{ indent 4 $ pretty $ process_ io }
            |]

data Q v x = Q {vars :: [v], function :: F v x, cads :: [ProcessStepID]}

instance (Var v, Val x) => Show (Q v x) where
    show Q{vars, function, cads} =
        concat
            [ "Q{"
            , "vars: " <> concatMap toString vars <> ","
            , "function: " <> show function <> ","
            , "cads : " <> show cads <> "}"
            ]

instance
    (VarValTime v x t, SimpleIOInterface i) =>
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
instance ConstantFoldingProblem (SimpleIO i v x t) v x
instance OptimizeAccumProblem (SimpleIO i v x t) v x
instance OptimizeLutProblem (SimpleIO i v x t) v x
instance ResolveDeadlockProblem (SimpleIO i v x t) v x

instance
    (VarValTime v x t, SimpleIOInterface i) =>
    EndpointProblem (SimpleIO i v x t) v t
    where
    endpointOptions pu@SimpleIO{receiveQueue, sendQueue} =
        let source vs = EndpointSt (Source $ S.fromList vs) $ TimeConstraint (nextTick pu + 1 ... maxBound) (1 ... maxBound)
            receiveOpts = map (source . vars) receiveQueue

            target v = EndpointSt (Target v) $ TimeConstraint (nextTick pu ... maxBound) (I.singleton 1)
            sendOpts = map (target . head . vars) sendQueue
         in receiveOpts ++ sendOpts

    endpointDecision sio@SimpleIO{receiveQueue} d@EndpointSt{epRole = Source vs, epAt}
        | ([q@Q{function, vars = allVars}], receiveQueue') <-
            L.partition ((vs `S.isSubsetOf`) . S.fromList . vars) receiveQueue
        , let remainVars = allVars L.\\ S.elems vs
              process_ = execSchedule sio $ do
                void $ scheduleEndpoint d $ scheduleInstructionUnsafe epAt $ Receiving $ null remainVars
                when (null remainVars) $ void $ scheduleFunction epAt function
              receiveQueue'' =
                if null remainVars
                    then receiveQueue'
                    else q{vars = remainVars} : receiveQueue' =
            sio{receiveQueue = receiveQueue'', process_}
    endpointDecision sio@SimpleIO{sendQueue, sendN, receiveQueue, receiveN} d@EndpointSt{epRole = Target v, epAt}
        | ([Q{function}], sendQueue') <- L.partition ((v ==) . head . vars) sendQueue
        , let process_ = execSchedule sio $ do
                void $ scheduleEndpoint d $ scheduleInstructionUnsafe epAt Sending
                scheduleFunction epAt function =
            sio
                { sendQueue = sendQueue'
                , isReceiveOver = (sendN - length sendQueue) >= (receiveN - length receiveQueue)
                , process_
                }
    endpointDecision pu d = error [i|incorrect decision #{ d } for #{ pretty pu }|]

{- | Access to received data buffer was implemented like a queue. OE signal read
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

    zipSignalTagsAndValues SimpleIOPorts{..} Microcode{..} =
        [ (wr, Bool wrSignal)
        , (oe, Bool oeSignal)
        ]

    usedPortTags SimpleIOPorts{wr, oe} = [wr, oe]

    takePortTags (wr : oe : _) _ = SimpleIOPorts wr oe "stop"
    takePortTags _ _ = error "can not take port tags, tags are over"

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
        , -- \|this flag which indicates an end of the data transaction
          -- requires for stop computational process while data transferring
          -- to avoid loses
          stop :: String
        }
        deriving (Show)

instance Var v => Locks (SimpleIO i v x t) v where
    locks SimpleIO{} = []

data ProtocolDescription v = ProtocolDescription
    { description :: T.Text
    , interface :: T.Text
    , dataType :: T.Text
    , toNitta :: [v]
    , fromNitta :: [v]
    }
    deriving (Generic)

instance ToJSON v => ToJSON (ProtocolDescription v)

protocolDescription ::
    forall i v x t.
    (VarValTime v x t, SimpleIOInterface i, ToJSON v) =>
    T.Text ->
    SimpleIO i v x t ->
    T.Text ->
    Implementation
protocolDescription tag io d
    | not $ null $ endpointOptions io = error "EndpointProblem is not completed"
    | otherwise =
        let impFile = toString $ tag <> ".json"
            fbs = getIntermediates $ process_ io
         in Immediate impFile $
                toStrict $
                    toLazyText $
                        encodePrettyToTextBuilder $
                            toJSON
                                ProtocolDescription
                                    { description = d
                                    , interface = showText $ typeRep (Proxy :: Proxy i)
                                    , dataType = showText $ typeRep (Proxy :: Proxy x)
                                    , toNitta = map (oneOf . outputs) $ filter isReceive fbs
                                    , fromNitta = map (oneOf . inputs) $ filter isSend fbs
                                    }

isReceive :: (Typeable v, Typeable x) => F v x -> Bool
isReceive f
    | Just F.Receive{} <- castF f = True
    | otherwise = False

isSend :: (Typeable v, Typeable x) => F v x -> Bool
isSend f
    | Just F.Send{} <- castF f = True
    | otherwise = False
