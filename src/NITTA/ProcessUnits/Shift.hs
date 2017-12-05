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

import           Data.Bits                   (shiftR)
import           Data.Default
import           Data.List                   (intersect, (\\))
import qualified Data.Map                    as M
import           Data.Typeable
import           NITTA.FunctionBlocks
import           NITTA.Lens
import           NITTA.ProcessUnits.SerialPU
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval            (singleton, (...))


type Shift v t = SerialPU (ShiftState v t) Parcel v t

data ShiftState v t = ShiftState{ sIn :: Maybe v, sOut :: [v] }
  deriving ( Show )

instance Default (ShiftState v t) where
  def = ShiftState def def



instance ( Var v, Time t ) => SerialPUState (ShiftState v t) Parcel v t where

  bindToState fb s@ShiftState{ sIn=Nothing, sOut=[] }
    | Just (ShiftL (I a) (O cs)) <- castFB fb = Right s{ sIn=Just a, sOut = cs }
    | otherwise = Left $ "Unknown functional block: " ++ show fb
  bindToState _ _ = error "Try bind to non-zero state. (Accum)"

  -- тихая ругань по поводу решения
  stateOptions ShiftState{ sIn=Just v } now
    = [ EndpointO (Target v) (TimeConstrain (now ... maxBound) (singleton 2)) ]
  stateOptions ShiftState{ sOut=vs@(_:_) } now -- вывод
    = [ EndpointO (Source vs) $ TimeConstrain (now + 1 ... maxBound) (1 ... maxBound) ]
  stateOptions _ _ = []

  schedule st@ShiftState{ sIn=Just v } act
    | v `elem` variables act
    = let st' = st{ sIn=Nothing }
          work = do
            -- serialSchedule (Proxy :: Proxy (Shift v t)) (act & at & supremum .~ act^.at.infimum) Init
            -- serialSchedule (Proxy :: Proxy (Shift v t)) (act & at & infimum .~ (act^.at.infimum + 1)) $ Work Right' Bit Logic
            a <- serialSchedule (Proxy :: Proxy (Shift v t)) act{ epdAt=(act^.at.infimum) ... (act^.at.infimum) } Init
            b <- serialSchedule (Proxy :: Proxy (Shift v t)) act{ epdAt=act^.at.infimum + 1 ... act^.at.supremum } $ Work Right' Bit Logic
            return $ a ++ b
      in (st', work)
  schedule st@ShiftState{ sOut=vs } act
    | not $ null $ vs `intersect` variables act
    = let st' = st{ sOut=vs \\ variables act }
          work = serialSchedule (Proxy :: Proxy (Shift v t)) act Out
      in (st', work)
  schedule _ _ = error "Accum schedule error!"



data Direction = Left' | Right'     deriving ( Show )
data StepSize  = Bit   | Byte       deriving ( Show )
data Mode      = Logic | Arithmetic deriving ( Show )

instance Controllable (Shift v t) where
  data Signal (Shift v t) = WORK | DIRECTION | MODE | STEP | INIT | OE deriving ( Show, Eq, Ord )
  data Instruction (Shift v t)
    = Nop
    | Init
    | Work Direction StepSize Mode
    | Out
    deriving (Show)

instance Default (Instruction (Shift v t)) where
  def = Nop

instance UnambiguouslyDecode (Shift v t) where
  decodeInstruction Nop  _                     = B False

  decodeInstruction Init INIT                  = B True
  decodeInstruction Init _                     = B False

  decodeInstruction (Work _ _ _) WORK          = B True
  decodeInstruction (Work Left' _ _) DIRECTION = B True
  decodeInstruction (Work _ Byte _) STEP       = B True
  decodeInstruction (Work _ _ Arithmetic) MODE = B True
  decodeInstruction (Work _ _ _) _             = B False

  decodeInstruction  Out     OE                = B True
  decodeInstruction  Out     _                 = B False



instance Simulatable (Shift v t) v Int where
  variableValue (FB fb) SerialPU{..} cntx (v, i)
    | Just (ShiftL (I a) _) <- cast fb, a == v           = cntx M.! (v, i)
    | Just (ShiftL (I a) (O cs)) <- cast fb, v `elem` cs = cntx M.! (a, i) `shiftR` 1
    | otherwise = error $ "Can't simulate " ++ show fb


instance Synthesis (Shift v t) where
  -- TODO: parameter
  hardwareInstance _pu n cntx
    = renderST
      [ "pu_shift #( .DATA_WIDTH(DATA_WIDTH)"
      , "          , .ATTR_WIDTH(ATTR_WIDTH)"
      , "          ) $name$"
      , "  ( .clk( $Clk$ )"
      , "  , .signal_work( $WORK$ ), .signal_direction( $DIRECTION$ )"
      , "  , .signal_mode( $MODE$ ), .signal_step( $STEP$ )"
      , "  , .signal_init( $INIT$ ), .signal_oe( $OE$ )"
      , "  , .data_in( $DataIn$ ), .attr_in( $AttrIn$ )"
      , "  , .data_out( $DataOut$ ), .attr_out( $AttrOut$ )"
      , "  );"
      ] $ ("name", n) : cntx
  name _ = "pu_shift"
  hardware pu = FromLibrary $ name pu ++ ".v"
  software _ = Empty
