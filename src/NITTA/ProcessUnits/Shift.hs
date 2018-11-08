{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.ProcessUnits.Shift
  ( Shift
  , PUPorts(..)
  )
  where

import qualified Data.Bits                           as B
import           Data.Default
import           Data.List                           (intersect, (\\))
import           Data.Set                            (elems, fromList)
import           Data.Typeable
import           NITTA.Functions
import           NITTA.ProcessUnits.Generic.SerialPU
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Lens
import           Numeric.Interval                    (singleton, (...))
import           Prelude                             hiding (init)
import           Text.InterpolatedString.Perl6       (qc)


type Shift v x t = SerialPU (State v x t) v x t

data State v x t  = State
  { sIn    :: Maybe v
  , sOut   :: [v]
  , sRight :: Bool
  }
  deriving ( Show )

instance Default (State v x t) where
  def = State def def False



instance ( Var v, Time t, Typeable x ) => SerialPUState (State v x t) v x t where

  bindToState fb s@State{ sIn=Nothing, sOut=[] }
    | Just fb' <- castF fb
    = case fb' of
      ShiftL (I a) (O cs) -> Right s{ sIn=Just a, sOut=elems cs, sRight=False }
      ShiftR (I a) (O cs) -> Right s{ sIn=Just a, sOut=elems cs, sRight=True }
    | otherwise = Left $ "The functional block is unsupported by Shift: " ++ show fb
  bindToState _ _ = error "Try bind to non-zero state. (Shift)"

  -- тихая ругань по поводу решения
  stateOptions State{ sIn=Just v } now
    = [ EndpointO (Target v) (TimeConstrain (now ... maxBound) (singleton 2)) ]
  stateOptions State{ sOut=vs@(_:_) } now -- вывод
    = [ EndpointO (Source $ fromList vs) $ TimeConstrain (now + 1 ... maxBound) (1 ... maxBound) ]
  stateOptions _ _ = []

  simpleSynthesis st@State{ sIn=Just v, sRight } act
    | v `elem` variables act
    = let st' = st{ sIn=Nothing }
          work = do
            a <- serialSchedule @(Shift v x t) Init act{ epdAt=(act^.at.infimum) ... (act^.at.infimum) }
            b <- serialSchedule @(Shift v x t) (Work sRight Bit Logic) act{ epdAt=act^.at.infimum + 1 ... act^.at.supremum }
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
    = Microcode{ workSignal :: Bool
               , directionSignal :: Bool
               , modeSignal :: Bool
               , stepSignal :: Bool
               , initSignal :: Bool
               , oeSignal :: Bool
               } deriving ( Show, Eq, Ord )

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
  data PUPorts (Shift v x t)
    = PUPorts{ work, direction, mode, step, init, oe :: Signal } deriving ( Show )
  transmitToLink Microcode{..} PUPorts{..}
    = [ (work, Bool workSignal)
      , (direction, Bool directionSignal)
      , (mode, Bool modeSignal)
      , (step, Bool stepSignal)
      , (init, Bool initSignal)
      , (oe, Bool oeSignal)
      ]


instance ( Var v, B.Bits x, Typeable x ) => Simulatable (Shift v x t) v x where
  simulateOn cntx _ f
    | Just (f' :: ShiftLR v x) <- castF f = simulate cntx f'
    | otherwise = error $ "Can't simulate " ++ show f ++ " on Shift."


instance TargetSystemComponent (Shift v x t) where
  moduleName _ _ = "pu_shift"
  hardware title pu = FromLibrary $ moduleName title pu ++ ".v"
  software _ _ = Empty
  hardwareInstance title _pu Enviroment{ net=NetEnv{..}, signalClk } PUPorts{..} =
    [qc|pu_shift
    #( .DATA_WIDTH( { show parameterDataWidth } )
     , .ATTR_WIDTH( { show parameterAttrWidth } )
     ) { title }
    ( .clk( { signalClk } )
    , .signal_work( { signal work } ), .signal_direction( { signal direction } )
    , .signal_mode( { signal mode } ), .signal_step( { signal step } )
    , .signal_init( { signal init } ), .signal_oe( { signal oe } )
    , .data_in( { dataIn } )
    , .attr_in( { attrIn } )
    , .data_out( { dataOut } )
    , .attr_out( { attrOut } )
    );|]
