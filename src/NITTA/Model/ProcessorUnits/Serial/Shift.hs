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
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Serial.Shift
  ( Shift
  , Ports(..), IOPorts(..)
  )
  where

import           Data.Bits ( finiteBitSize )
import           Data.Default
import           Data.List ( intersect, (\\) )
import           NITTA.Intermediate.Functions hiding ( remain )
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems
import           NITTA.Model.ProcessorUnits.Serial.Generic
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.Types
import           NITTA.Project
import           NITTA.Utils
import           NITTA.Utils.ProcessDescription
import           Numeric.Interval ( inf, singleton, sup, (...) )
import           Prelude hiding ( init )
import           Text.InterpolatedString.Perl6 ( qc )

import           Control.Monad ( when )
import           Data.Bits ( finiteBitSize )
import           Data.Default
import           Data.List ( find, partition, (\\) )
import           Data.Set ( elems, fromList, member )
import qualified NITTA.Intermediate.Functions as F
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.Types
import           NITTA.Project
import           NITTA.Utils
import           NITTA.Utils.ProcessDescription
import           Text.InterpolatedString.Perl6 ( qc )

data Shift v x t = Shift
    { remain               :: [ F v x ]
    , target               :: Maybe v
    , sources              :: [ v ]
    , sRight               :: Bool
    , shiftStep            :: Int
    , shiftByte            :: Bool
    , currentWork          :: Maybe ( t, F v x )
    , currentWorkEndpoints :: [ ProcessStepID ]
    , process_             :: Process v x t
    , tick                 :: t
    }

instance ( VarValTime v x t ) => Show (Shift v x t)

instance ( Var v ) => Locks (Shift v x t) v where
    locks Shift{ remain, sources, target=Just t } =
        [ Lock{ lockBy=t, locked }
        | locked <- sources
        ]
    locks Shift{ remain, sources, target=Nothing } = []

instance Default t => Default (Shift v x t) where
  def = Shift
        { remain=[]
        , target=Nothing
        , sources=[]
        , currentWork=Nothing
        , currentWorkEndpoints=[]
        , process_=def
        , tick=def
        , sRight=True
        , shiftStep=1
        , shiftByte=False
        }

instance RefactorProblem (Shift v x t) v x

instance ( VarValTime v x t
         ) => ProcessorUnit (Shift v x t) v x t where
    -- Binding to mUnit is carried out by this function.
    tryBind f pu@Shift{ remain }
        | Just f' <- castF f
            = case f' of
                ShiftL s (I a) (O cs) -> Right pu{ remain=f : remain }
                ShiftR s (I a) (O cs) -> Right pu{ remain=f : remain }

        | otherwise = Left $ "The function is unsupported by Shift: " ++ show f
    process = process_

-- |This function carry out actual take functional block to work.
execution pu@Shift{ target=Nothing, sources=[], remain, tick } f
    | Just f' <- castF f
        = case f' of
            ShiftL s (I i) (O o) -> toPU i o False s
            ShiftR s (I i) (O o) -> toPU i o True s

      where
          toPU inp out sRight step = pu
              { target=Just inp
              , currentWork=Just (tick, f)
              , sources=elems out
              , remain=remain \\ [ f ]
              , sRight=sRight
              , shiftStep=step
              }
execution _ _ = error "Shift: internal execution error."

instance ( VarValTime v x t
        ) => EndpointProblem (Shift v x t) v t
        where
    endpointOptions Shift{ target=Just t, tick }
        = [ EndpointSt (Target t) $ TimeConstrain (tick ... maxBound) (singleton 1) ]

    endpointOptions Shift{ sources, tick, shiftStep }
        | not $ null sources
        = [ EndpointSt (Source $ fromList sources) $ TimeConstrain (tick + fromIntegral shiftStep + 2 ... maxBound) (1 ... maxBound) ]

    endpointOptions pu@Shift{ remain } = concatMap (endpointOptions . execution pu) remain

    endpointDecision pu@Shift{ target=(Just _), currentWorkEndpoints, sRight, shiftByte, shiftStep } d@EndpointSt{ epRole=Target v, epAt } = let
            (newEndpoints, process_') = runSchedule pu $ do
                 updateTick (sup epAt)
                 scheduleEndpoint d $ do
                    scheduleInstruction epAt $ Init
                    scheduleInstruction (inf epAt + 1 ... sup epAt + (fromIntegral shiftStep) ) $ Work sRight shiftByte Logic
        in
            pu
                { process_=process_'
                , target=Nothing
                , currentWorkEndpoints=newEndpoints ++ currentWorkEndpoints
                , tick=sup epAt
                }

--	2. If model is waiting, that we will download variables from it.
    endpointDecision pu@Shift{ target=Nothing, sources, currentWork=Just (a, f), currentWorkEndpoints } d@EndpointSt{ epRole=Source v, epAt }
        | not $ null sources
        , let sources' = sources \\ elems v
        , sources' /= sources  = let
                (newEndpoints, process_') = runSchedule pu $ do
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
                    , tick=sup epAt
                    }

    --    3. If no function is executed at the moment, then we need to find function in the list
    --    of assigned function, executed it to work and only then make decision
    --    and plan a fragment of computation process with call recursion in situation 1.
    endpointDecision pu@Shift{ target=Nothing, sources=[], remain } d
        | let v = oneOf $ variables d
        , Just f <- find (\f -> v `member` variables f) remain
        = endpointDecision (execution pu f) d

    -- If smth went wrong.
    endpointDecision pu d = error $ "Shift decision error\npu: " ++ show pu ++ ";\n decison:" ++ show d

data Mode      = Logic | Arithmetic deriving ( Show, Eq )

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

                -- , .signal_mode( { signal mode } ), .signal_step( { signal step } )
    hardwareInstance _title _pu TargetEnvironment{ unitEnv=NetworkEnv{} } _ports _op
        = error "Should be defined in network."

instance IOTestBench (Shift v x t) v x
