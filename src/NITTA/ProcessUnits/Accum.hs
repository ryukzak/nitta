{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.ProcessUnits.Accum where

import           Data.Default
import           Data.List                   (intersect, (\\))
import           Data.Typeable
import           NITTA.FunctionBlocks
import           NITTA.ProcessUnits.SerialPU
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval            (singleton, (...))
import           Prelude                     hiding (init)



type Accum v t = SerialPU (State v t) (Parcel v) v t

data State v t = Accum{ acIn :: [v], acOut :: [v] }
  deriving ( Show )

instance Default (State v t) where
  def = Accum def def



instance ( Var v, Time t ) => SerialPUState (State v t) (Parcel v) v t where

  bindToState (FB fb) ac@Accum{ acIn=[], acOut=[] }
    | Just (Add (I a) (I b) (O cs)) <- cast fb = Right ac{ acIn=[a, b], acOut = cs }
    | otherwise = Left $ "Unknown functional block: " ++ show fb
  bindToState _ _ = error "Try bind to non-zero state. (Accum)"

  -- тихая ругань по поводу решения
  stateOptions Accum{ acIn=vs@(_:_) } now
    | length vs == 2 -- первый аргумент.
    = map (\v -> EndpointO (Target v) $ TimeConstrain (now ... maxBound) (singleton 2)) vs
    | otherwise -- второй аргумент
    = map (\v -> EndpointO (Target v) $ TimeConstrain (now ... maxBound) (singleton 1)) vs
  stateOptions Accum{ acOut=vs@(_:_) } now -- вывод
    = [ EndpointO (Source vs) $ TimeConstrain (now + 1 ... maxBound) (1 ... maxBound) ]
  stateOptions _ _ = []

  schedule st@Accum{ acIn=vs@(_:_) } act
    | not $ null $ vs `intersect` variables act
    = let st' = st{ acIn=vs \\ variables act }
          work = serialSchedule (Proxy :: Proxy (Accum v t)) act
            $ if length vs == 2
              then Init False
              else Load False
      in (st', work)
  schedule st@Accum{ acIn=[], acOut=vs } act
    | not $ null $ vs `intersect` variables act
    = let st' = st{ acOut=vs \\ variables act }
          work = serialSchedule (Proxy :: Proxy (Accum v t)) act Out
      in (st', work)
  schedule _ _ = error "Accum schedule error!"



instance Controllable (Accum v t) where
  data Microcode (Accum v t)
    = Microcode{ oeSignal :: Bool
               , initSignal :: Bool
               , loadSignal :: Bool
               , negSignal :: Maybe Bool
               } deriving ( Show, Eq, Ord )

  data Instruction (Accum v t)
    = Nop
    | Init Bool
    | Load Bool
    | Out
    deriving (Show)

instance Default (Instruction (Accum v t)) where
  def = Nop

instance Default (Microcode (Accum v t)) where
  def = Microcode{ oeSignal=False
                 , initSignal=False
                 , loadSignal=False
                 , negSignal=Nothing
                 }

instance UnambiguouslyDecode (Accum v t) where
  decodeInstruction Nop        = def
  decodeInstruction (Init neg) = def{ initSignal=True, loadSignal=True, negSignal=Just neg }
  decodeInstruction (Load neg) = def{ loadSignal=True, negSignal=Just neg }
  decodeInstruction Out        = def{ oeSignal=True }


instance ( Var v ) => Simulatable (Accum v t) v Int where
  simulateOn cntx _ (FB fb)
    | Just (fb' :: Add (Parcel v)) <- cast fb = simulate cntx fb'
    | otherwise = error $ "Can't simulate " ++ show fb ++ " on Accum."


instance Connected (Accum v t) i where
  data Link (Accum v t) i
    = Link { init, load, neg, oe :: i } deriving ( Show )
  transmitToLink Microcode{..} Link{..}
    = [ (init, B initSignal)
      , (load, B loadSignal)
      , (neg, maybe X B negSignal)
      , (oe, B oeSignal)
      ]


instance DefinitionSynthesis (Accum v t) where
  moduleName _ = "pu_accum"
  hardware pu = FromLibrary $ moduleName pu ++ ".v"
  software _ = Empty


instance ( Time t, Var v
         ) => Synthesis (Accum v t) LinkId where
  hardwareInstance _ name NetworkLink{..} Link{..} = renderST
    [ "pu_accum "
    , "  #( .DATA_WIDTH( " ++ link dataWidth ++ " )"
    , "   , .ATTR_WIDTH( " ++ link attrWidth ++ " )"
    , "   ) $name$"
    , "  ( .clk( " ++ link clk ++ " )"
    , "  , .signal_init( " ++ ctrl init ++ " )"
    , "  , .signal_load( " ++ ctrl load ++ " )"
    , "  , .signal_neg( " ++ ctrl neg ++ " )"
    , "  , .signal_oe( " ++ ctrl oe ++ " )"
    , "  , .data_in( " ++ link dataIn ++ " )"
    , "  , .attr_in( " ++ link attrIn ++ " )"
    , "  , .data_out( " ++ link dataOut ++ " )"
    , "  , .attr_out( " ++ link attrOut ++ " )"
    , "  );"
    , "initial $name$.acc <= 0;"
    ] [("name", name)]
    where
      ctrl = link . controlBus
