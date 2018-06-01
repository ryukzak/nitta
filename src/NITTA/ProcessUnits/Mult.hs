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
  , Link(..)
  ) where

import           Control.Monad.State
import           Data.Default
import           Data.Either
import           Data.List            (find)
import qualified Data.Set             as S
import           Data.Typeable
import           NITTA.FunctionBlocks (castFB)
import qualified NITTA.FunctionBlocks as FB
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Lens
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
  decision _proxy pu@Mult{} act = undefined


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
               , resSelSignal :: Bool
               } deriving ( Show, Eq, Ord )

  data Instruction (Mult v x t)
    = Nop
    | Load ArgumentSel
    | Out
    deriving (Show)



instance Default (Instruction (Mult v x t)) where
  def = Nop



instance Default (Microcode (Mult v x t)) where
  def = Microcode{ wrSignal=False
                 , selSignal=False
                 , oeSignal=False
                 }


instance UnambiguouslyDecode (Mult v x t) where
  decodeInstruction Nop       = def
  decodeInstruction (Load A1) = def{ wrSignal=True, selSignal=True }
  decodeInstruction (Load A2) = def{ wrSignal=True, selSignal=False }
  decodeInstruction Out       = def{ oeSignal=True }
  decodeInstruction Out       = def{ oeSignal=True }



instance Connected (Mult v x t) i where
  data Link (Mult v x t) i
    = Link { wr, wrSel, oe :: i } deriving ( Show )
  transmitToLink Microcode{..} Link{..}
    = [ (wr, B wrSignal)
      , (wrSel, B selSignal)
      , (oe, B resSelSignal)
      ]


instance ( Var v
         , Integral x
         ) => Simulatable (Mult v x t) v x where
  simulateOn cntx _ fb
    | Just fb'@FB.Mul{} <- castFB fb = simulate cntx fb'
    | otherwise = error $ "Can't siMultate " ++ show fb ++ " on Shift."



instance ( Var v, Show t ) => DefinitionSynthesis (Mult v x t) where
  moduleName _ = "pu_mult"
  hardware pu = Project "" [ FromLibrary "mult/mult_placeholder.v"
                          --  , FromLibrary "Mult/Mult.v"
                           , FromLibrary $ "mult/" ++ moduleName pu ++ ".v"
                           ]
  software pu = Empty

instance ( Time t, Var v
         ) => Synthesis (Mult v x t) LinkId where
  hardwareInstance _ name NetworkLink{..} Link{..} = renderST
    [ "pu_mult"
    , "  #( .DATA_WIDTH( " ++ link dataWidth ++ " )"
    , "   , .ATTR_WIDTH( " ++ link attrWidth ++ " )"
    -- , "   , .INVALID( INVALID )" -- FIXME:
    , "   ) $name$"
    , "  ( .clk( " ++ link clk ++ " )"
    , "  , .rst( " ++ link rst ++ " )"
    , "  , .signal_wr( " ++ control wr ++ " )"
    , "  , .signal_sel( " ++ control wrSel ++ " )"
    , "  , .data_in( " ++ link dataIn ++ " )"
    , "  , .attr_in( " ++ link attrIn ++ " )"
    , "  , .signal_oe( " ++ control oe ++ " )"
    , "  , .data_out( " ++ link dataOut ++ " )"
    , "  , .attr_out( " ++ link attrOut ++ " )"
    , "  );"
    ] [("name", name)]
    where
      control = link . controlBus
