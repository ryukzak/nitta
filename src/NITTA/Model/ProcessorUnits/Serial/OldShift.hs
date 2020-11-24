{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
import           Data.Set ( elems, fromList )
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems
import           NITTA.Model.ProcessorUnits.Serial.Generic
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.Types
import           NITTA.Project
import           NITTA.Utils
import           Numeric.Interval ( inf, singleton, sup, (...) )
import           Prelude hiding ( init )
import           Text.InterpolatedString.Perl6 ( qc )


type Shift v x t = SerialPU (State v x t) v x t

data State v x t  = State
  { sIn    :: Maybe v
  , sOut   :: [v]
  , sRight :: Bool
  }
  deriving ( Show )

instance Default (State v x t) where
  def = State def def False



instance ( VarValTime v x t ) => SerialPUState (State v x t) v x t where

  bindToState fb s@State{ sIn=Nothing, sOut=[] }
    | Just fb' <- castF fb
    = case fb' of
      ShiftL (I a) _ (O cs) -> Right s{ sIn=Just a, sOut=elems cs, sRight=False }
      ShiftR (I a) _ (O cs) -> Right s{ sIn=Just a, sOut=elems cs, sRight=True }
    | otherwise = Left $ "The functional block is unsupported by Shift: " ++ show fb
  bindToState _ _ = error "Try bind to non-zero state. (Shift)"

  -- very bad solution
  stateOptions State{ sIn=Just v } now
    = [ EndpointSt (Target v) (TimeConstrain (now ... maxBound) (singleton 2)) ]
  stateOptions State{ sOut=vs@(_:_) } now -- output
    = [ EndpointSt (Source $ fromList vs) $ TimeConstrain (now + 1 ... maxBound) (1 ... maxBound) ]
  stateOptions _ _ = []

  simpleSynthesis st@State{ sIn=Just v, sRight } act@EndpointSt{ epAt }
    | v `elem` variables act
    = let st' = st{ sIn=Nothing }
          work = do
            a <- serialSchedule @(Shift v x t) Init act{ epAt=singleton $ inf epAt }
            b <- serialSchedule @(Shift v x t) (Work sRight Bit Logic) act{ epAt=inf epAt + 1 ... sup epAt }
            return $ a ++ b
      in (st', work)
  simpleSynthesis st@State{ sOut=vs } act
    | not $ null $ vs `intersect` elems (variables act)
    = let st' = st{ sOut=vs \\ elems (variables act) }
          work = serialSchedule @(Shift v x t) Out $ shift (-1) act
      in (st', work)
  simpleSynthesis _ _ = error "Accum simpleSynthesis error!"


data StepSize  = Bit   | Byte       deriving ( Show, Eq )
data Mode      = Logic | Arithmetic deriving ( Show, Eq )

instance Controllable (Shift v x t) where
    data Instruction (Shift v x t)
        = Init
        | Work Bool StepSize Mode
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
                 , stepSignal=False
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
         , stepSignal=step == Byte
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
