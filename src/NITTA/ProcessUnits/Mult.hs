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
import           Numeric.Interval            (singleton, (...))


type Mult v x t = SerialPU (State v x t) v x t

data State v x t = Mult{ inputVs :: [v], outputVs :: [v] }
  deriving ( Show )

instance Default (State v x t) where
  def = Mult def def



instance ( Var v
         , Time t
         , Typeable x
         ) => SerialPUState (State v x t) v x t where
  bindToState fb st@Mult{ inputVs=[], outputVs=[] }
    | Just (Mul (I a) (I b) (O cs)) <- castFB fb = Right st{ inputVs=[a, b], outputVs=elems cs }
    | otherwise = Left $ "Unknown functional block: " ++ show fb
  bindToState _ _ = error "Try bind to non-zero state. (Mult)"

  stateOptions Mult{ inputVs } now
    | not $ null inputVs
    = map (\v -> EndpointO (Target v) $ TimeConstrain (now ... maxBound) (singleton 1)) inputVs
  stateOptions Mult{ outputVs } now
    | not $ null outputVs
    = [ EndpointO (Source $ fromList outputVs) $ TimeConstrain (now + 2 ... maxBound) (1 ... maxBound) ]
  stateOptions _ _ = []

  schedule st@Mult{ inputVs } d
    | not $ null inputVs
    , let v = oneOf $ variables d
    , ([_], remain) <- partition (== v) inputVs
    = let i = if length inputVs == 2
                then Load False
                else Load True
          work = serialSchedule @(Mult v x t) i d
      in ( st{ inputVs=remain }, work )
  schedule st@Mult{ inputVs=[], outputVs } d
    | let dVs = elems (variables d)
    , not $ null $ outputVs `intersect` dVs
    = let work = serialSchedule @(Mult v x t) Out d
      in ( st{ outputVs=outputVs \\ dVs }, work )
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


instance Connected (Mult v x t) where
  data PUPorts (Mult v x t)
    = PUPorts{ wr, sel, oe :: Signal } deriving ( Show )
  transmitToLink Microcode{..} PUPorts{..}
    = [ (wr, B wrSignal)
      , (sel, B selSignal)
      , (oe, B oeSignal)
      ]


instance TargetSystemComponent (Mult v x t) where
  moduleName _ _ = "pu_mult"
  hardware _ _
    = Aggregate Nothing
        [ FromLibrary "mult/mult_inner.v"
        , FromLibrary "mult/pu_mult.v"
        ]
  software _ _ = Empty
  hardwareInstance name _pu Enviroment{ net=NetEnv{..}, signalClk, signalRst } PUPorts{..} = renderMST
    [ "pu_mult "
    , "  #( .DATA_WIDTH( " ++ show parameterDataWidth ++ " )"
    , "   , .ATTR_WIDTH( " ++ show parameterAttrWidth ++ " )"
    , "   ) $name$"
    , "  ( .clk( " ++ signalClk ++ " )"
    , "  , .rst( " ++ signalRst ++ " )"
    , "  , .signal_wr( " ++ signal wr ++ " )"
    , "  , .signal_sel( " ++ signal sel ++ " )"
    , "  , .signal_oe( " ++ signal oe ++ " )"
    , "  , .data_in( " ++ dataIn ++ " )"
    , "  , .attr_in( " ++ attrIn ++ " )"
    , "  , .data_out( " ++ dataOut ++ " )"
    , "  , .attr_out( " ++ attrOut ++ " )"
    , "  );"
    ] [ ( "name", name ) ]
