{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.ProcessUnits.Accum
Description :
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.ProcessUnits.Accum
  ( Accum
  , Ports(..)
  ) where

import           Data.Bits                           (finiteBitSize)
import           Data.Default
import           Data.List                           (intersect, partition,
                                                      (\\))
import           Data.Set                            (elems, fromList)
import           NITTA.Functions
import           NITTA.ProcessUnits.Generic.SerialPU
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval                    (singleton, (...))
import           Prelude                             hiding (init)
import           Text.InterpolatedString.Perl6       (qc)



type Accum v x t = SerialPU (State v x t) v x t

data State v x t = Accum{ acIn :: [(Bool, v)], acOut :: [v] }
  deriving ( Show )

instance Default (State v x t) where
  def = Accum def def



instance ( VarValTime v x t
         ) => SerialPUState (State v x t) v x t where

  bindToState fb ac@Accum{ acIn=[], acOut=[] }
    | Just (Add (I a) (I b) (O cs)) <- castF fb = Right ac{ acIn=[(False, a), (False, b)], acOut=elems cs }
    | Just (Sub (I a) (I b) (O cs)) <- castF fb = Right ac{ acIn=[(False, a), (True, b)], acOut=elems cs }
    | otherwise = Left $ "The functional block is unsupported by Accum: " ++ show fb
  bindToState _ _ = error "Try bind to non-zero state. (Accum)"

  -- тихая ругань по поводу решения
  stateOptions Accum{ acIn=vs@(_:_) } now
    | length vs == 2 -- первый аргумент.
    = map (\(_, v) -> EndpointO (Target v) $ TimeConstrain (now ... maxBound) (singleton 1)) vs
    | otherwise -- второй аргумент
    = map (\(_, v) -> EndpointO (Target v) $ TimeConstrain (now ... maxBound) (singleton 1)) vs
  stateOptions Accum{ acOut=vs@(_:_) } now -- вывод
    = [ EndpointO (Source $ fromList vs) $ TimeConstrain (now + 2 ... maxBound) (1 ... maxBound) ]
  stateOptions _ _ = []

  simpleSynthesis st@Accum{ acIn=vs@(_:_) } act
    | let actV = oneOf $ variables act
    , ([(neg, _)], remain) <- partition ((== actV) . snd) vs
    = let i = if length vs == 2 then Init neg else Load neg
          work = serialSchedule @(Accum v x t) i act
      in (st{ acIn=remain }, work)
  simpleSynthesis st@Accum{ acIn=[], acOut=vs } act
    | not $ null $ vs `intersect` elems (variables act)
    = let st' = st{ acOut=vs \\ elems (variables act) }
          work = serialSchedule @(Accum v x t) Out $ shift (-1) act
      in (st', work)
  simpleSynthesis _ _ = error "Accum simpleSynthesis error!"


instance Controllable (Accum v x t) where
  data Instruction (Accum v x t)
    = Init Bool
    | Load Bool
    | Out
    deriving (Show)

  data Microcode (Accum v x t)
    = Microcode{ oeSignal :: Bool
               , initSignal :: Bool
               , loadSignal :: Bool
               , negSignal :: Maybe Bool
               } deriving ( Show, Eq, Ord )

  mapMicrocodeToPorts Microcode{..} Ports{..}
    = [ (init, Bool initSignal)
      , (load, Bool loadSignal)
      , (neg, maybe Undef Bool negSignal)
      , (oe, Bool oeSignal)
      ]


instance Default (Microcode (Accum v x t)) where
  def = Microcode{ oeSignal=False
                 , initSignal=False
                 , loadSignal=False
                 , negSignal=Nothing
                 }

instance UnambiguouslyDecode (Accum v x t) where
  decodeInstruction (Init neg) = def{ initSignal=True, loadSignal=True, negSignal=Just neg }
  decodeInstruction (Load neg) = def{ loadSignal=True, negSignal=Just neg }
  decodeInstruction Out        = def{ oeSignal=True }


instance ( VarValTime v x t
         , Num x
         ) => Simulatable (Accum v x t) v x where
  simulateOn cntx _ f
    | Just f'@Add{} <- castF f = simulate cntx f'
    | Just f'@Sub{} <- castF f = simulate cntx f'
    | otherwise = error $ "Can't simulate " ++ show f ++ " on Accum."


instance Connected (Accum v x t) where
  data Ports (Accum v x t)
    = Ports{ init, load, neg, oe :: SignalTag } deriving ( Show )


instance ( Val x ) => TargetSystemComponent (Accum v x t) where
    moduleName _ _ = "pu_accum"
    hardware title pu = FromLibrary $ moduleName title pu ++ ".v"
    software _ _ = Empty
    hardwareInstance title _pu TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk, signalRst } Ports{..}
        = fixIndent [qc|
|       pu_accum #
|               ( .DATA_WIDTH( { finiteBitSize (def :: x) } )
|               , .ATTR_WIDTH( { show parameterAttrWidth } )
|               ) { title }
|           ( .clk( { signalClk } )
|           , .rst( { signalRst } )
|           , .signal_init( { signal init } )
|           , .signal_load( { signal load } )
|           , .signal_neg( { signal neg } )
|           , .signal_oe( { signal oe } )
|           , .data_in( { dataIn } )
|           , .attr_in( { attrIn } )
|           , .data_out( { dataOut } )
|           , .attr_out( { attrOut } )
|           );
|       |]
    hardwareInstance _title _pu TargetEnvironment{ unitEnv=NetworkEnv{} } _bnPorts
        = error "Should be defined in network."

instance IOTest (Accum v x t) v x
