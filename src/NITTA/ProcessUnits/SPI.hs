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

  stateOptions SPI_State{ .. } now = catMaybes [ send spiSend, receive spiReceive ]
    where
      send (_, v:_) = Just $ EndpointO (Target v) $ TimeConstrain (now ... maxBound) (1 ... maxBound)
      send _ = Nothing
      receive (_, vs:_) = Just $ EndpointO (Source vs) $ TimeConstrain (now ... maxBound) (1 ... maxBound)
      receive _ = Nothing

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
  data Signal (SPI v t)
    = WR -- ^ Запись данных в ВУ. По отрицательному фронту происходит инкрементирование адреса.
    | OE -- ^ Чтение данных из ВУ. По отрицательному фронту происходит инкрементирование адреса.
    | CYCLE -- ^ Сигнал о начале вычислительного цикла. Необходим для синхронизации и контроля целостности.
    deriving ( Show, Eq, Ord )
  data Flag (SPI v t)
    = START -- ^ Выходной сигнал о начале цикла передачи данных.
    | STOP -- ^ Выходной сигнал о конце цикла передачи данных.
    deriving ( Show, Eq, Ord )

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

instance UnambiguouslyDecode (SPI v t) where
  decodeInstruction Sending   WR = B True
  decodeInstruction Receiving OE = B True
  decodeInstruction  _        _  = B False



instance Simulatable (SPI v t) v Int where
  variableValue (FB fb) SerialPU{..} _cntx (_v, _i)
    | otherwise = 0
    -- | Just (Add (I a) (I b) (O cs)) <- cast fb, v `elem` cs = cntx M.! (a, i) + cntx M.! (b, i)
    | otherwise = error $ "Can't simulate " ++ show fb



instance ( Show v, Show t ) => Synthesis (SPI v t) where
  hardwareInstance _pu n cntx
    = renderST
      [ "pu_spi #( .DATA_WIDTH( $DATA_WIDTH$ )"
      , "        , .ATTR_WIDTH( $ATTR_WIDTH$ )"
      , "        ) $name$"
      , "  ( .clk( $Clk$ )"
      , "  , .rst( $Rst$ )"
      , ""
      , "  , .signal_wr( $WR$ )"
      , "  , .data_in( $DataIn$ )"
      , "  , .attr_in( $AttrIn$ )"
      , ""
      , "  , .signal_oe( $OE$ )"
      , "  , .data_out( $DataOut$ )"
      , "  , .attr_out( $AttrOut$ )"
      , ""
      , "  , .flag_cycle( $Cycle$ )"
      , "  , .flag_start( $START$ )"
      , "  , .flag_stop( $STOP$ )"
      , ""
      , "  , .mosi( $mosi$ )"
      , "  , .miso( $miso$ )"
      , "  , .sclk( $sclk$ )"
      , "  , .cs( $cs$ )"
      , ");"
      ] $ ("name", n) : cntx
  name _ = "pu_spi"
  hardware pu = FromLibrary $ "spi/" ++ name pu ++ ".v"
  software pu = Immidiate "transport.txt" $ show pu
