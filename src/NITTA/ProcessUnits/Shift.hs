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

module NITTA.ProcessUnits.Shift where

import           Data.Default
import           Data.List                   (intersect, (\\))
import           Data.Typeable
import           NITTA.FunctionBlocks
import           NITTA.ProcessUnits.SerialPU
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Lens
import           Numeric.Interval            (singleton, (...))
import           Prelude                     hiding (init)


type Shift v t = SerialPU (State v t) (Parcel v) v t

data State v t = State{ sIn  :: Maybe v
                      , sOut :: [v]
                      }
  deriving ( Show )

instance Default (State v t) where
  def = State def def



instance ( Var v, Time t ) => SerialPUState (State v t) (Parcel v) v t where

  bindToState (FB fb) s@State{ sIn=Nothing, sOut=[] }
    | Just (ShiftL (I a) (O cs)) <- cast fb = Right s{ sIn=Just a, sOut = cs }
    | otherwise = Left $ "Unknown functional block: " ++ show fb
  bindToState _ _ = error "Try bind to non-zero state. (Accum)"

  -- тихая ругань по поводу решения
  stateOptions State{ sIn=Just v } now
    = [ EndpointO (Target v) (TimeConstrain (now ... maxBound) (singleton 2)) ]
  stateOptions State{ sOut=vs@(_:_) } now -- вывод
    = [ EndpointO (Source vs) $ TimeConstrain (now + 1 ... maxBound) (1 ... maxBound) ]
  stateOptions _ _ = []

  schedule st@State{ sIn=Just v } act
    | v `elem` variables act
    = let st' = st{ sIn=Nothing }
          work = do
            a <- serialSchedule (Proxy :: Proxy (Shift v t)) act{ epdAt=(act^.at.infimum) ... (act^.at.infimum) } Init
            b <- serialSchedule (Proxy :: Proxy (Shift v t)) act{ epdAt=act^.at.infimum + 1 ... act^.at.supremum } $ Work Right' Bit Logic
            return $ a ++ b
      in (st', work)
  schedule st@State{ sOut=vs } act
    | not $ null $ vs `intersect` variables act
    = let st' = st{ sOut=vs \\ variables act }
          work = serialSchedule (Proxy :: Proxy (Shift v t)) act Out
      in (st', work)
  schedule _ _ = error "Accum schedule error!"



data Direction = Left' | Right'     deriving ( Show, Eq )
data StepSize  = Bit   | Byte       deriving ( Show, Eq )
data Mode      = Logic | Arithmetic deriving ( Show, Eq )

instance Controllable (Shift v t) where
  data Microcode (Shift v t)
    = Microcode{ workSignal :: Bool
               , directionSignal :: Bool
               , modeSignal :: Bool
               , stepSignal :: Bool
               , initSignal :: Bool
               , oeSignal :: Bool
               } deriving ( Show, Eq, Ord )

  data Instruction (Shift v t)
    = Nop
    | Init
    | Work Direction StepSize Mode
    | Out
    deriving (Show)

instance Default (Instruction (Shift v t)) where
  def = Nop

instance Default (Microcode (Shift v t)) where
  def = Microcode{ workSignal=False
                 , directionSignal=False
                 , modeSignal=False
                 , stepSignal=False
                 , initSignal=False
                 , oeSignal=False
                 }

instance UnambiguouslyDecode (Shift v t) where
  decodeInstruction Nop = def
  decodeInstruction Init = def{ initSignal=True }
  decodeInstruction Out = def{ oeSignal=True }
  decodeInstruction (Work dir step mode)
    = def{ workSignal=True
         , directionSignal=dir == Left'
         , modeSignal=mode == Arithmetic
         , stepSignal=step == Byte
         }


instance Connected (Shift v t) i where
  data Link (Shift v t) i
    = Link { work, direction, mode, step, init, oe :: i } deriving ( Show )
  transmitToLink Microcode{..} Link{..}
    = [ (work, B workSignal)
      , (direction, B directionSignal)
      , (mode, B modeSignal)
      , (step, B stepSignal)
      , (init, B initSignal)
      , (oe, B oeSignal)
      ]


instance ( Var v ) => Simulatable (Shift v t) v Int where
  simulateOn cntx _ (FB fb)
    | Just (fb' :: ShiftL (Parcel v)) <- cast fb = simulate cntx fb'
    | otherwise = error $ "Can't simulate " ++ show fb ++ " on Shift."


instance DefinitionSynthesis (Shift v t) where
  moduleName _ = "pu_shift"
  hardware pu = FromLibrary $ moduleName pu ++ ".v"
  software _ = Empty

instance ( Time t, Var v
         ) => Synthesis (Shift v t) LinkId where
  hardwareInstance _ name NetworkLink{..} Link{..} = renderST
    [ "pu_shift #( .DATA_WIDTH( " ++ link dataWidth ++ " )"
    , "          , .ATTR_WIDTH( " ++ link attrWidth ++ " )"
    , "          ) $name$"
    , "  ( .clk( " ++ link clk ++ " )"
    , "  , .signal_work( " ++ control work ++ " ), .signal_direction( " ++ control direction ++ " )"
    , "  , .signal_mode( " ++ control mode ++ " ), .signal_step( " ++ control step ++ " )"
    , "  , .signal_init( " ++ control init ++ " ), .signal_oe( " ++ control oe ++ " )"
    , "  , .data_in( " ++ link dataIn ++ " )"
    , "  , .attr_in( " ++ link attrIn ++ " )"
    , "  , .data_out( " ++ link dataOut ++ " )"
    , "  , .attr_out( " ++ link attrOut ++ " )"
    , "  );"
    ] [("name", name)]
    where
      control = link . controlBus
