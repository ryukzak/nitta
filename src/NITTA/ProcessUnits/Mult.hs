{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.ProcessUnits.Mult
  ( Mult(..)
  , PUPorts(..)
  ) where

import           Data.Default
import           Data.Typeable
import           NITTA.FunctionBlocks (castFB)
import qualified NITTA.FunctionBlocks as FB
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval     ((...))



data ArgumentSel
  = A1
  | A2
  deriving ( Show, Eq )

data Mult v x t
  = Mult
  { puTarget  :: [(ArgumentSel, v)]
  , puSource  :: [v]
  , puRemain  :: [FB (Parcel v x)]
  , puProcess :: Process (Parcel v x) t
  } deriving ( Show )

instance ( Time t, Var v ) => Default (Mult v x t) where
  def = Mult [] [] [] def



instance ( Var v, Time t
         , Typeable x
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (Mult v x t)
         where
  options _proxy Mult{} = []
  decision _proxy pu@Mult{} _act = undefined


instance ( Var v, Time t
         ) => ProcessUnit (Mult v x t) (Parcel v x) t where
  bind fb pu@Mult{ puRemain }
    | Just FB.Mul{} <- castFB fb = Right pu{ puRemain=fb : puRemain }
    | otherwise = Left $ "Unknown functional block: " ++ show fb
  process = puProcess
  setTime t pu@Mult{ puProcess } = pu{ puProcess=puProcess{ nextTick=t } }



instance Controllable (Mult v x t) where
  data Microcode (Mult v x t)
    = Microcode{ wrSignal :: Bool
               , selSignal :: Bool
               , oeSignal :: Bool
               } deriving ( Show, Eq, Ord )

  data Instruction (Mult v x t)
    = Nop
    | Load ArgumentSel
    | Out
    deriving (Show)
  nop = Nop


instance UnambiguouslyDecode (Mult v x t) where
  decodeInstruction Nop       = Microcode
    { wrSignal=False
    , selSignal=False
    , oeSignal=False
    }
  decodeInstruction (Load A1) = (decodeInstruction Nop){ wrSignal=True, selSignal=True }
  decodeInstruction (Load A2) = (decodeInstruction Nop){ wrSignal=True, selSignal=False }
  decodeInstruction Out       = (decodeInstruction Nop){ oeSignal=True }
  decodeInstruction Out       = (decodeInstruction Nop){ oeSignal=True }



instance Connected (Mult v x t) where
  data PUPorts (Mult v x t)
    = PUPorts{ wr, wrSel, oe :: Signal } deriving ( Show )
  transmitToLink Microcode{..} PUPorts{..}
    = [ (wr, B wrSignal)
      , (wrSel, B selSignal)
      ]


instance ( Var v
         , Integral x
         ) => Simulatable (Mult v x t) v x where
  simulateOn cntx _ fb
    | Just fb'@FB.Mul{} <- castFB fb = simulate cntx fb'
    | otherwise = error $ "Can't siMultate " ++ show fb ++ " on Shift."



instance ( Time t, Var v
         ) => TargetSystemComponent (Mult v x t) where
  moduleName _ _ = "pu_mult"
  software _ _ = Empty
  hardware title pu = Aggregate Nothing [ FromLibrary "mult/mult_placeholder.v"
                                        --  , FromLibrary "Mult/Mult.v"
                                        , FromLibrary $ "mult/" ++ moduleName title pu ++ ".v"
                                        ]
  hardwareInstance title _ Enviroment{ net=NetEnv{..}, signalClk, signalRst } PUPorts{..} = renderMST
    [ "pu_mult"
    , "  #( .DATA_WIDTH( " ++ show parameterDataWidth ++ " )"
    , "   , .ATTR_WIDTH( " ++ show parameterAttrWidth ++ " )"
    , "   , .INVALID( 0 )" -- FIXME:
    , "   ) $name$"
    , "  ( .clk( " ++ signalClk ++ " )"
    , "  , .rst( " ++ signalRst ++ " )"
    , "  , .signal_wr( " ++ signal wr ++ " )"
    , "  , .signal_sel( " ++ signal wrSel ++ " )"
    , "  , .data_in( " ++ dataIn ++ " )"
    , "  , .attr_in( " ++ attrIn ++ " )"
    , "  , .signal_oe( " ++ signal oe ++ " )"
    , "  , .data_out( " ++ dataOut ++ " )"
    , "  , .attr_out( " ++ attrOut ++ " )"
    , "  );"
    ] [("name", title)]
