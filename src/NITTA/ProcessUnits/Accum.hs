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

module NITTA.ProcessUnits.Accum where

import           Data.Default
import           Data.List                   (intersect, partition, (\\))
import           Data.Set                    (elems, fromList)
import           Data.Typeable
import           NITTA.FunctionBlocks
import           NITTA.ProcessUnits.SerialPU
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval            (singleton, (...))
import           Prelude                     hiding (init)



type Accum v x t = SerialPU (State v x t) v x t

data State v x t = Accum{ acIn :: [(Bool, v)], acOut :: [v] }
  deriving ( Show )

instance Default (State v x t) where
  def = Accum def def



instance ( Var v
         , Time t
         , Typeable x
         ) => SerialPUState (State v x t) v x t where

  bindToState fb ac@Accum{ acIn=[], acOut=[] }
    | Just (Add (I a) (I b) (O cs)) <- castFB fb = Right ac{ acIn=[(False, a), (False, b)], acOut=elems cs }
    | Just (Sub (I a) (I b) (O cs)) <- castFB fb = Right ac{ acIn=[(False, a), (True, b)], acOut=elems cs }
    | otherwise = Left $ "Unknown functional block: " ++ show fb
  bindToState _ _ = error "Try bind to non-zero state. (Accum)"

  -- тихая ругань по поводу решения
  stateOptions Accum{ acIn=vs@(_:_) } now
    | length vs == 2 -- первый аргумент.
    -- TODO: Improve performance.
    = map (\(_, v) -> EndpointO (Target v) $ TimeConstrain (now ... maxBound) (singleton 2)) vs
    | otherwise -- второй аргумент
    = map (\(_, v) -> EndpointO (Target v) $ TimeConstrain (now ... maxBound) (singleton 1)) vs
  stateOptions Accum{ acOut=vs@(_:_) } now -- вывод
    = [ EndpointO (Source $ fromList vs) $ TimeConstrain (now + 2 ... maxBound) (1 ... maxBound) ]
  stateOptions _ _ = []

  schedule st@Accum{ acIn=vs@(_:_) } act
    | let actV = oneOf $ variables act
    , ([(neg, _)], remain) <- partition ((== actV) . snd) vs
    = let i = if length vs == 2 then Init neg else Load neg
          work = serialSchedule @(Accum v x t) i act
      in (st{ acIn=remain }, work)
  schedule st@Accum{ acIn=[], acOut=vs } act
    | not $ null $ vs `intersect` elems (variables act)
    = let st' = st{ acOut=vs \\ elems (variables act) }
          work = serialSchedule @(Accum v x t) Out $ shift (-1) act
      in (st', work)
  schedule _ _ = error "Accum schedule error!"


instance Controllable (Accum v x t) where
  data Microcode (Accum v x t)
    = Microcode{ oeSignal :: Bool
               , initSignal :: Bool
               , loadSignal :: Bool
               , negSignal :: Maybe Bool
               } deriving ( Show, Eq, Ord )

  data Instruction (Accum v x t)
    = Nop
    | Init Bool
    | Load Bool
    | Out
    deriving (Show)

instance Default (Instruction (Accum v x t)) where
  def = Nop

instance Default (Microcode (Accum v x t)) where
  def = Microcode{ oeSignal=False
                 , initSignal=False
                 , loadSignal=False
                 , negSignal=Nothing
                 }

instance UnambiguouslyDecode (Accum v x t) where
  decodeInstruction Nop        = def
  decodeInstruction (Init neg) = def{ initSignal=True, loadSignal=True, negSignal=Just neg }
  decodeInstruction (Load neg) = def{ loadSignal=True, negSignal=Just neg }
  decodeInstruction Out        = def{ oeSignal=True }


instance ( Var v
         , Num x
         ) => Simulatable (Accum v x t) v x where
  simulateOn cntx _ fb
    | Just fb'@Add{} <- castFB fb = simulate cntx fb'
    | Just fb'@Sub{} <- castFB fb = simulate cntx fb'
    | otherwise = error $ "Can't simulate " ++ show fb ++ " on Accum."


instance Connected (Accum v x t) where
  data PUPorts (Accum v x t)
    = PUPorts{ init, load, neg, oe :: Signal } deriving ( Show )
  transmitToLink Microcode{..} PUPorts{..}
    = [ (init, B initSignal)
      , (load, B loadSignal)
      , (neg, maybe Q B negSignal)
      , (oe, B oeSignal)
      ]


instance DefinitionSynthesis (Accum v x t) where
  moduleName _ = "pu_accum"
  hardware pu = FromLibrary $ moduleName pu ++ ".v"
  software _ = Empty


instance ( Time t, Var v
         ) => Synthesis (Accum v x t) where
  hardwareInstance _ name Enviroment{ net=NetEnv{..}, signalClk } PUPorts{..} = renderST
    [ "pu_accum "
    , "  #( .DATA_WIDTH( " ++ show parameterDataWidth ++ " )"
    , "   , .ATTR_WIDTH( " ++ show parameterAttrWidth ++ " )"
    , "   ) $name$"
    , "  ( .clk( " ++ signalClk ++ " )"
    , "  , .signal_init( " ++ signal init ++ " )"
    , "  , .signal_load( " ++ signal load ++ " )"
    , "  , .signal_neg( " ++ signal neg ++ " )"
    , "  , .signal_oe( " ++ signal oe ++ " )"
    , "  , .data_in( " ++ dataIn ++ " )"
    , "  , .attr_in( " ++ attrIn ++ " )"
    , "  , .data_out( " ++ dataOut ++ " )"
    , "  , .attr_out( " ++ attrOut ++ " )"
    , "  );"
    , "initial $name$.acc <= 0;"
    ] [("name", name)]
