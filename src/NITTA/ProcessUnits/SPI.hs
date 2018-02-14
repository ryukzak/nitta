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

-- slave / master / slave-master?
module NITTA.ProcessUnits.SPI where

import           Data.Default
import           Data.List                   (sort)
import           Data.Maybe                  (catMaybes)
import           Data.Typeable
import           NITTA.FunctionBlocks
import           NITTA.ProcessUnits.SerialPU
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval            ((...))



type SPI v t = SerialPU (SPI_State v t) (Parcel v) v t

data SPI_State v t = SPI_State { spiSend    :: ([v], [v])
                               , spiReceive :: ([[v]], [[v]])
                               }
  deriving ( Show )

instance Default (SPI_State v t) where
  def = SPI_State def def



instance ( Var v, Time t ) => SerialPUState (SPI_State v t) (Parcel v) v t where

  bindToState (FB fb) st@SPI_State{ .. }
    | Just (Send (I v)) <- cast fb
    , let (ds, rs) = spiSend
    = Right st{ spiSend=(ds, v:rs) }

    | Just (Receive (O vs)) <- cast fb
    , let (ds, rs) = spiReceive
    = Right st{ spiReceive=(ds, vs:rs) }

    | otherwise = Left $ "Unknown functional block: " ++ show fb

  stateOptions SPI_State{ .. } now = catMaybes [ send' spiSend, receive' spiReceive ]
    where
      send' (_, v:_) = Just $ EndpointO (Target v) $ TimeConstrain (now ... maxBound) (1 ... maxBound)
      send' _ = Nothing
      receive' (_, vs:_) = Just $ EndpointO (Source vs) $ TimeConstrain (now ... maxBound) (1 ... maxBound)
      receive' _ = Nothing

  schedule st@SPI_State{ spiSend=(ds, v:rs) } act
    | [v] == variables act
    = let st' = st{ spiSend=(v:ds, rs) }
          work = serialSchedule (Proxy :: Proxy (SPI v t)) act Sending
      in (st', work)

  schedule st@SPI_State{ spiReceive=(ds, vs:rs) } act
    -- FIXME: Ошибка, так как с точки зрения опции, передачу данных можно дробить на несколько шагов.
    | sort vs == sort (variables act)
    = let st' = st{ spiReceive=(vs:ds, rs) }
          work = serialSchedule (Proxy :: Proxy (SPI v t)) act Receiving
      in (st', work)

  schedule _ _ = error "Schedule error! (SPI)"



instance Controllable (SPI v t) where
  data Microcode (SPI v t)
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
  data Instruction (SPI v t)
    = Nop
    | Receiving
    | Sending
    deriving ( Show )

instance Default (Instruction (SPI v t)) where
  def = Nop

instance Default (Microcode (SPI v t)) where
  def = Microcode{ wrSignal=False
                 , oeSignal=False
                 }


instance UnambiguouslyDecode (SPI v t) where
  decodeInstruction Nop       = def
  decodeInstruction Sending   = def{ wrSignal=True }
  decodeInstruction Receiving = def{ oeSignal=True }



instance Simulatable (SPI String t) String Int where
  simulateOn cntx _ (FB fb)
    | Just (fb' :: Send (Parcel String)) <- cast fb = simulate cntx fb'
    | Just (fb' :: Receive (Parcel String)) <- cast fb = simulate cntx fb'
    | otherwise = error $ "Can't simulate " ++ show fb ++ " on SPI."

instance Connected (SPI v t) i where
  data Link (SPI v t) i
    = Link { wr, oe :: i
           , start, stop, mosi, miso, sclk, cs :: i
           } deriving ( Show )
  transmitToLink Microcode{..} Link{..}
    = [ (wr, B wrSignal)
      , (oe, B oeSignal)
      ]



instance ( Show v, Show t ) => DefinitionSynthesis (SPI v t) where
  moduleName _ = "pu_slave_spi"
  hardware pu = Project "" [ FromLibrary $ "spi/spi_slave_driver.v"
                           , FromLibrary $ "spi/spi_buffer.v"
                           , FromLibrary $ "spi/" ++ moduleName pu ++ ".v"
                           ]
  software pu = Immidiate "transport.txt" $ show pu

instance ( Time t, Var v
         ) => Synthesis (SPI v t) LinkId where
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
    , "  , .data_out( " ++ link dataOut ++ " )"
    , "  , .attr_out( " ++ link attrOut ++ " )"
    , "  , .mosi( " ++ link mosi ++ " )"
    , "  , .miso( " ++ link miso ++ " )"
    , "  , .sclk( " ++ link sclk ++ " )"
    , "  , .cs( " ++ link cs ++ " )"
    , "  );"
    ] $ ("name", name) : []
    where
      control = link . controlBus
