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

-- slave / master / slave-master?
module NITTA.ProcessUnits.SPI
  ( Link(..)
  , SPI
  ) where

import           Data.Default
import           Data.Maybe                  (catMaybes)
import           Data.Set                    (elems, fromList, singleton)
import           Data.Typeable
import           NITTA.FunctionBlocks
import           NITTA.ProcessUnits.Generic.SerialPU
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval            ((...))



type SPI v x t = SerialPU (State v x t) v x t
data State v x t = State{ spiSend    :: ([v], [v])
                        , spiReceive :: ([[v]], [[v]])
                        }
  deriving ( Show )

instance Default (State v x t) where
  def = State def def



instance ( Var v, Time t, Typeable x ) => SerialPUState (State v x t) v x t where

  bindToState fb st@State{ .. }
    | Just (Send (I v)) <- castFB fb
    , let (ds, rs) = spiSend
    = Right st{ spiSend=(ds, v:rs) }

    | Just (Receive (O vs)) <- castFB fb
    , let (ds, rs) = spiReceive
    = Right st{ spiReceive=(ds, elems vs : rs) }

    | otherwise = Left $ "Unknown functional block: " ++ show fb

  stateOptions State{ spiSend, spiReceive } now = catMaybes [ send' spiSend, receive' spiReceive ]
    where
      send' (_, v:_) = Just $ EndpointO (Target v) $ TimeConstrain (now ... maxBound) (1 ... maxBound)
      send' _ = Nothing
      receive' (_, vs:_) = Just $ EndpointO (Source $ fromList vs) $ TimeConstrain (now ... maxBound) (1 ... maxBound)
      receive' _ = Nothing

  schedule st@State{ spiSend=(ds, v:rs) } act
    | singleton v == variables act
    = let st' = st{ spiSend=(v:ds, rs) }
          work = serialSchedule @(SPI v x t) Sending act
      in (st', work)

  schedule st@State{ spiReceive=(ds, vs:rs) } act
    -- FIXME: Ошибка, так как с точки зрения опции, передачу данных можно дробить на несколько шагов.
    | fromList vs == variables act
    = let st' = st{ spiReceive=(vs:ds, rs) }
          work = serialSchedule @(SPI v x t) Receiving act
      in (st', work)

  schedule _ _ = error "Schedule error! (SPI)"



instance Controllable (SPI v x t) where
  data Microcode (SPI v x t)
    = Microcode{ wrSignal :: Bool
               , oeSignal :: Bool
               } deriving ( Show, Eq, Ord )
  -- | Доступ к входному буферу осуществляется как к очереди. это сделано для
  -- того, что бы сократить колличество сигнальных линий (убрать адрес).
  -- Увеличение адреса производится по негативному фронту сигналов OE и WR для
  -- Receive и Send соответственно.
  --
  -- Управление передачей данных осуществляется полностью вычислительным блоком.
  --
  -- Пример:
  --
  -- 1. Nop - отдых
  -- 2. Send - В блок загружается с шины слово по адресу 0.
  -- 3. Send - В блок загружается с шины слово по адресу 0.
  -- 4. Nop - отдых
  -- 5. Receive - Из блока выгружается на шину слово по адресу 0.
  -- 6. Send - В блок загружается с шины слово по адресу 1.
  -- 7. Receive - Из блока выгружается на шину слово по адресу 1.
  data Instruction (SPI v x t)
    = Nop
    | Receiving
    | Sending
    deriving ( Show )

instance Default (Instruction (SPI v x t)) where
  def = Nop

instance Default (Microcode (SPI v x t)) where
  def = Microcode{ wrSignal=False
                 , oeSignal=False
                 }


instance UnambiguouslyDecode (SPI v x t) where
  decodeInstruction Nop       = def
  decodeInstruction Sending   = def{ wrSignal=True }
  decodeInstruction Receiving = def{ oeSignal=True }



instance ( Ord v ) => Simulatable (SPI v x t) v x where
  simulateOn cntx _ fb
    | Just fb'@Send{} <- castFB fb = simulate cntx fb'
    | Just fb'@Receive{} <- castFB fb = simulate cntx fb'
    | otherwise = error $ "Can't simulate " ++ show fb ++ " on SPI."

instance Connected (SPI v x t) i where
  data Link (SPI v x t) i
    = Link { wr, oe :: i
           , start, stop, mosi, miso, sclk, cs :: i
           } deriving ( Show )
  transmitToLink Microcode{..} Link{..}
    = [ (wr, B wrSignal)
      , (oe, B oeSignal)
      ]



instance ( Var v, Show t ) => DefinitionSynthesis (SPI v x t) where
  moduleName _ = "pu_slave_spi"
  hardware pu = Project "" [ FromLibrary "spi/spi_slave_driver.v"
                           , FromLibrary "spi/spi_buffer.v"
                           , FromLibrary $ "spi/" ++ moduleName pu ++ ".v"
                           ]
  software pu = Immidiate "transport.txt" $ show pu

instance ( Time t, Var v
         ) => Synthesis (SPI v x t) LinkId where
  hardwareInstance _ name NetworkLink{..} Link{..} = renderST
    [ "pu_slave_spi"
    , "  #( .DATA_WIDTH( " ++ link dataWidth ++ " )"
    , "   , .ATTR_WIDTH( " ++ link attrWidth ++ " )"
    , "   ) $name$"
    , "  ( .clk( " ++ link clk ++ " )"
    , "  , .rst( " ++ link rst ++ " )"
    , "  , .signal_cycle( " ++ link cycleStart ++ " )"
    , "  , .signal_oe( " ++ control oe ++ " )"
    , "  , .signal_wr( " ++ control wr ++ " )"
    , "  , .flag_start( " ++ link start ++ " )"
    , "  , .flag_stop( " ++ link stop ++ " )"
    , "  , .data_in( " ++ link dataIn ++ " )"
    , "  , .attr_in( " ++ link attrIn ++ " )"
    -- , "  , .data_out( " ++ link dataOut ++ " )"
    -- , "  , .attr_out( " ++ link attrOut ++ " )"
    , "  , .mosi( " ++ link mosi ++ " )"
    , "  , .miso( " ++ link miso ++ " )"
    , "  , .sclk( " ++ link sclk ++ " )"
    , "  , .cs( " ++ link cs ++ " )"
    , "  );"
    , "  assign " ++ link dataOut ++ " = 0;"
    , "  assign " ++ link attrOut ++ " = 0;"
    ] [("name", name)]
    where
      control = link . controlBus
