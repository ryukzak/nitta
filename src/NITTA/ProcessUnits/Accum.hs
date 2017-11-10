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
import qualified Data.Map                    as M
import           Data.Typeable
import           NITTA.FunctionBlocks
import           NITTA.ProcessUnits.SerialPU
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval            (singleton, (...))



type Accum v t = SerialPU (AccumState v t) Parcel v t

data AccumState v t = Accum{ acIn :: [v], acOut :: [v] }
  deriving ( Show )

instance Default (AccumState v t) where
  def = Accum def def



instance ( Var v, Time t ) => SerialPUState (AccumState v t) Parcel v t where

  bindToState fb ac@Accum{ acIn=[], acOut=[] }
    | Just (Add (I a) (I b) (O cs)) <- castFB fb = Right ac{ acIn=[a, b], acOut = cs }
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
  data Signal (Accum v t) = OE | INIT | LOAD | NEG deriving ( Show, Eq, Ord )
  data Instruction (Accum v t)
    = Nop
    | Init Bool
    | Load Bool
    | Out
    deriving (Show)

instance Default (Instruction (Accum v t)) where
  def = Nop

instance UnambiguouslyDecode (Accum v t) where
  decodeInstruction  Nop     NEG  = X
  decodeInstruction  Nop     _    = B False

  decodeInstruction (Init _) INIT = B True
  decodeInstruction (Init _) LOAD = B True
  decodeInstruction (Init _) OE   = B False
  decodeInstruction (Init n) NEG  = B n

  decodeInstruction (Load _) INIT = B False
  decodeInstruction (Load _) LOAD = B True
  decodeInstruction (Load _) OE   = B False
  decodeInstruction (Load n) NEG  = B n

  decodeInstruction  Out     INIT = B False
  decodeInstruction  Out     LOAD = B False
  decodeInstruction  Out     OE   = B True
  decodeInstruction  Out     NEG  = X



instance Simulatable (Accum v t) v Int where
  variableValue (FB fb) SerialPU{..} cntx (v, i)
    | Just (Add (I a) _ _) <- cast fb, a == v               = cntx M.! (v, i)
    | Just (Add _ (I b) _) <- cast fb, b == v               = cntx M.! (v, i)
    | Just (Add (I a) (I b) (O cs)) <- cast fb, v `elem` cs = cntx M.! (a, i) + cntx M.! (b, i)
    | otherwise = error $ "Can't simulate " ++ show fb



instance Synthesis (Accum v t) where
  hardwareInstance _pu n cntx
    = renderST
      [ "pu_accum $name$ ("
      , "    .clk( $Clk$ ),"
      , ""
      , "    .signal_init( $INIT$ ),"
      , "    .signal_load( $LOAD$ ),"
      , "    .signal_neg( $NEG$ ),"
      , "    .data_in( $DataIn$ ),"
      , "    .attr_in( $AttrIn$ ),"
      , ""
      , "    .signal_oe( $OE$ ),"
      , "    .data_out( $DataOut$ ),"
      , "    .attr_out( $AttrOut$ )"
      , ");"
      , "initial $name$.acc <= 0;"
      ] $ ("name", n) : cntx
  name _ = "pu_accum"
  hardware pu = FromLibrary $ name pu ++ ".v"
  software _ = Empty
