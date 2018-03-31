{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.ProcessUnits.Mult where

import           Data.Default
import           Data.List                   (intersect, partition, (\\))
import           Data.Set                    (elems, fromList)
import           Data.Typeable
import           NITTA.FunctionBlocks
import           NITTA.ProcessUnits.SerialPU
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval            (inf, singleton, sup, (...))



type Mult v x t = SerialPU (State v x t) v x t

data State v x t = Mult{ acIn :: [v], acOut :: [v] }
  deriving ( Show )

instance Default (State v x t) where
  def = Mult def def



instance ( Var v
         , Time t
         , Typeable x
         ) => SerialPUState (State v x t) v x t where

  bindToState fb ac@Mult{ acIn=[], acOut=[] }
    | Just (Mul (I a) (I b) (O cs)) <- castFB fb = Right ac{ acIn=[a, b], acOut=elems cs }
    | otherwise = Left $ "Unknown functional block: " ++ show fb
  bindToState _ _ = error "Try bind to non-zero state. (Mult)"

  stateOptions Mult{ acIn } now
    | not $ null acIn
    = map (\v -> EndpointO (Target v) $ TimeConstrain (now ... maxBound) (singleton 1)) acIn
  stateOptions Mult{ acOut } now
    | not $ null acOut
    = [ EndpointO (Source $ fromList acOut) $ TimeConstrain (now + 2 ... maxBound) (1 ... maxBound) ]
  stateOptions _ _ = []

  schedule st@Mult{ acIn } act
    | not $ null acIn
    , let actV = oneOf $ variables act
    , ([_], remain) <- partition (== actV) acIn
    = let i = if length acIn == 2 then Load False else Load True
          work = serialSchedule @(Mult v x t) i act
      in ( st{ acIn=remain }, work )
  schedule st@Mult{ acIn=[], acOut } act@EndpointD{ epdAt }
    | let actVs = elems (variables act)
    , not $ null $ acOut `intersect` actVs
    = let work = serialSchedule @(Mult v x t) Out act
      in ( st{ acOut=acOut \\ actVs }, work )
  schedule _ _ = error "Mult schedule error!"


instance Controllable (Mult v x t) where
  data Microcode (Mult v x t)
    = Microcode{ oeSignal :: Bool
               , wrSignal :: Bool
               , selSignal :: Bool
               } deriving ( Show, Eq, Ord )

  data Instruction (Mult v x t)
    = Nop
    | Load Bool
    | Out
    deriving (Show)

instance Default (Instruction (Mult v x t)) where
  def = Nop

instance Default (Microcode (Mult v x t)) where
  def = Microcode{ oeSignal=False
                 , wrSignal=False
                 , selSignal=False
                 }

instance UnambiguouslyDecode (Mult v x t) where
  decodeInstruction Nop        = def
  decodeInstruction (Load sel) = def{ wrSignal=True, selSignal=sel }
  decodeInstruction Out        = def{ oeSignal=True }


instance ( Var v
         , Num x
         ) => Simulatable (Mult v x t) v x where
  simulateOn cntx _ fb
    | Just fb'@Mul{} <- castFB fb = simulate cntx fb'
    | otherwise = error $ "Can't simulate " ++ show fb ++ " on Mult."


instance Connected (Mult v x t) i where
  data Link (Mult v x t) i
    = Link { wr, sel, oe :: i } deriving ( Show )
  transmitToLink Microcode{..} Link{..}
    = [ (wr, B wrSignal)
      , (sel, B selSignal)
      , (oe, B oeSignal)
      ]


instance DefinitionSynthesis (Mult v x t) where
  moduleName _ = "pu_mult"
  hardware _pu = Project "" [ FromLibrary "mult/mult_inner.v"
                            , FromLibrary "mult/pu_mult.v"
                            ]
  software _ = Empty


instance ( Time t, Var v
         ) => Synthesis (Mult v x t) LinkId where
  hardwareInstance _ name NetworkLink{..} Link{..} = renderST
    [ "pu_mult "
    , "  #( .DATA_WIDTH( " ++ link dataWidth ++ " )"
    , "   , .ATTR_WIDTH( " ++ link attrWidth ++ " )"
    , "   ) $name$"
    , "  ( .clk( " ++ link clk ++ " )"
    , "  , .rst( " ++ link rst ++ " )"
    , "  , .signal_wr( " ++ ctrl wr ++ " )"
    , "  , .signal_sel( " ++ ctrl sel ++ " )"
    , "  , .signal_oe( " ++ ctrl oe ++ " )"
    , "  , .data_in( " ++ link dataIn ++ " )"
    , "  , .attr_in( " ++ link attrIn ++ " )"
    , "  , .data_out( " ++ link dataOut ++ " )"
    , "  , .attr_out( " ++ link attrOut ++ " )"
    , "  );"
    ] [("name", name)]
    where
      ctrl = link . controlBus
