{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- slave / master / slave-master?
module NITTA.ProcessUnits.SPI
  ( PUPorts(..)
  , SPI
  , slaveSPI
  ) where

import           Data.Default
import           Data.Maybe                          (catMaybes)
import           Data.Set                            (elems, fromList,
                                                      singleton)
import           Data.Typeable
import           NITTA.FunctionBlocks
import           NITTA.ProcessUnits.Generic.SerialPU
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval                    ((...))



type SPI v x t = SerialPU (State v x t) v x t
data State v x t = State{ spiSend         :: ([v], [v])
                        , spiReceive      :: ([[v]], [[v]])
                        , spiBounceFilter :: Int
                        }
  deriving ( Show )

instance Default (State v x t) where
  def = State def def 20

slaveSPI :: ( Var v, Time t ) => Int -> SPI v x t
slaveSPI bounceFilter = SerialPU (State def def bounceFilter) def def def def



instance ( Var v, Time t, Typeable x ) => SerialPUState (State v x t) v x t where

  bindToState fb st@State{ .. }
    | Just (Send (I v)) <- castFB fb
    , let (ds, rs) = spiSend
    = Right st{ spiSend=(ds, v:rs) }

    | Just (Receive (O vs)) <- castFB fb
    , let (ds, rs) = spiReceive
    = Right st{ spiReceive=(ds, elems vs : rs) }

    | otherwise = Left $ "The functional block is unsupported by SPI: " ++ show fb

  stateOptions State{ spiSend, spiReceive } now = catMaybes [ send' spiSend, receive' spiReceive ]
    where
      -- FIXME: `+1`, ошибка находится в аппаратуре, тут надо просто убрать запас.
      send' (_, v:_) = Just $ EndpointO (Target v) $ TimeConstrain (now + 1 ... maxBound) (1 ... maxBound)
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
    = Receiving
    | Sending
    deriving ( Show )

  data Microcode (SPI v x t)
    = Microcode{ wrSignal :: Bool
               , oeSignal :: Bool
               } deriving ( Show, Eq, Ord )


instance Default (Microcode (SPI v x t)) where
  def = Microcode{ wrSignal=False
                 , oeSignal=False
                 }


instance UnambiguouslyDecode (SPI v x t) where
  decodeInstruction Sending   = def{ wrSignal=True }
  decodeInstruction Receiving = def{ oeSignal=True }



instance ( Ord v ) => Simulatable (SPI v x t) v x where
  simulateOn cntx _ fb
    | Just fb'@Send{} <- castFB fb = simulate cntx fb'
    | Just fb'@Receive{} <- castFB fb = simulate cntx fb'
    | otherwise = error $ "Can't simulate " ++ show fb ++ " on SPI."

instance Connected (SPI v x t) where
  data PUPorts (SPI v x t)
    = PUPorts{ wr, oe :: Signal
             , stop :: String -- FIXME: Что это такое и как этому быть?
             , mosi, sclk, cs :: InputPort
             , miso :: OutputPort
             } deriving ( Show )
  transmitToLink Microcode{..} PUPorts{..}
    = [ (wr, Bool wrSignal)
      , (oe, Bool oeSignal)
      ]



instance ( Var v, Show t ) => TargetSystemComponent (SPI v x t) where
  moduleName _ _ = "pu_slave_spi"
  hardware title pu
    = Aggregate Nothing
        [ FromLibrary "spi/pu_slave_spi_driver.v"
        , FromLibrary "spi/spi_slave_driver.v"
        , FromLibrary "spi/buffer.v"
        , FromLibrary "spi/bounce_filter.v"
        , FromLibrary "spi/spi_master_driver.v"
        , FromLibrary "spi/nitta_to_spi_splitter.v"
        , FromLibrary $ "spi/" ++ moduleName title pu ++ ".v"
        ]
  software _ pu = Immidiate "transport.txt" $ show pu
  hardwareInstance title SerialPU{ spuState=State{ spiBounceFilter } } Enviroment{ net=NetEnv{..}, signalClk, signalRst, signalCycle, inputPort, outputPort } PUPorts{..} = renderMST
    [ "pu_slave_spi"
    , "  #( .DATA_WIDTH( " ++ show parameterDataWidth ++ " )"
    , "   , .ATTR_WIDTH( " ++ show parameterAttrWidth ++ " )"
    , "   , .BOUNCE_FILTER( " ++ show spiBounceFilter ++ " )" -- FIXME: Must be configurable.
    , "   ) $name$"
    , "  ( .clk( " ++ signalClk ++ " )"
    , "  , .rst( " ++ signalRst ++ " )"
    , "  , .signal_cycle( " ++ signalCycle ++ " )"
    , "  , .signal_oe( " ++ signal oe ++ " )"
    , "  , .signal_wr( " ++ signal wr ++ " )"
    , "  , .flag_stop( " ++ stop ++ " )"
    , "  , .data_in( " ++ dataIn ++ " )"
    , "  , .attr_in( " ++ attrIn ++ " )"
    , "  , .data_out( " ++ dataOut ++ " )"
    , "  , .attr_out( " ++ attrOut ++ " )"
    , "  , .mosi( " ++ inputPort mosi ++ " )"
    , "  , .miso( " ++ outputPort miso ++ " )"
    , "  , .sclk( " ++ inputPort sclk ++ " )"
    , "  , .cs( " ++ inputPort cs ++ " )"
    , "  );"
    ] [ ( "name", title ) ]

  componentTestEnviroment title _pu Enviroment{ net=NetEnv{..}, signalClk, signalRst, inputPort, outputPort } PUPorts{..} = renderMST
    [ "reg $name$_start_transaction;"
    , "reg  [64-1:0] $name$_master_in;"
    , "wire [64-1:0] $name$_master_out;"
    , "wire $name$_ready;"
    , "spi_master_driver "
    , "  #( .DATA_WIDTH( 64 ) " -- FIXME: 32
    , "   , .SCLK_HALFPERIOD( 1 )"
    , "   ) $name$_master"
    , "  ( .clk( $clk$ )"
    , "  , .rst( $rst$ )"
    , "  , .start_transaction( $name$_start_transaction )"
    , "  , .data_in( $name$_master_in )"
    , "  , .data_out( $name$_master_out )"
    , "  , .ready( $name$_ready )"
    , "  , .mosi( " ++ inputPort mosi ++ " )"
    , "  , .miso( " ++ outputPort miso ++ " )"
    , "  , .sclk( " ++ inputPort sclk ++ " )"
    , "  , .cs( " ++ inputPort cs ++ " )"
    , "  );"
    , "initial $name$_master.inner.shiftreg <= 0;"
    , ""
    , "initial begin"
    , "  $name$_start_transaction <= 0; $name$_master_in <= 0;"
    , "  @(negedge $rst$);"
    , "  repeat(8) @(posedge $clk$); "
    , ""
    , "  $name$_master_in = 64'h0123456789ABCDEF;                @(posedge $clk$);"
    , "  $name$_start_transaction = 1;                           @(posedge $clk$);"
    , "  $name$_start_transaction = 0;                           @(posedge $clk$);"
    , "  repeat(200) @(posedge $clk$); "
    , ""
    , "  $name$_master_in = 64'h0123456789ABCDEF;                @(posedge $clk$);"
    , "  $name$_start_transaction = 1;                           @(posedge $clk$);"
    , "  $name$_start_transaction = 0;                           @(posedge $clk$);"
    , "  repeat(200) @(posedge $clk$); "
    , ""
    , "  $name$_master_in = 64'h0123456789ABCDEF;                @(posedge $clk$);"
    , "  $name$_start_transaction = 1;                           @(posedge $clk$);"
    , "  $name$_start_transaction = 0;                           @(posedge $clk$);"
    , "  repeat(200) @(posedge $clk$); "
    , ""
    , "  $name$_master_in = 64'h0123456789ABCDEF;                @(posedge $clk$);"
    , "  $name$_start_transaction = 1;                           @(posedge $clk$);"
    , "  $name$_start_transaction = 0;                           @(posedge $clk$);"
    , "  repeat(200) @(posedge $clk$); "
    , ""
    , "  $name$_master_in = 64'h0123456789ABCDEF;                @(posedge $clk$);"
    , "  $name$_start_transaction = 1;                           @(posedge $clk$);"
    , "  $name$_start_transaction = 0;                           @(posedge $clk$);"
    , "  repeat(200) @(posedge $clk$); "
    , ""
    , "  $name$_master_in = 64'h0123456789ABCDEF;                @(posedge $clk$);"
    , "  $name$_start_transaction = 1;                           @(posedge $clk$);"
    , "  $name$_start_transaction = 0;                           @(posedge $clk$);"
    , "  repeat(200) @(posedge $clk$); "
    , ""
    , "  $name$_master_in = 64'h0123456789ABCDEF;                @(posedge $clk$);"
    , "  $name$_start_transaction = 1;                           @(posedge $clk$);"
    , "  $name$_start_transaction = 0;                           @(posedge $clk$);"
    , "  repeat(200) @(posedge $clk$); "
    , ""
    , "  $name$_master_in = 64'h0123456789ABCDEF;                @(posedge $clk$);"
    , "  $name$_start_transaction = 1;                           @(posedge $clk$);"
    , "  $name$_start_transaction = 0;                           @(posedge $clk$);"
    , "  repeat(200) @(posedge $clk$); "
    , ""
    , "  $name$_master_in = 64'h0123456789ABCDEF;                @(posedge $clk$);"
    , "  $name$_start_transaction = 1;                           @(posedge $clk$);"
    , "  $name$_start_transaction = 0;                           @(posedge $clk$);"
    , "  repeat(200) @(posedge $clk$); "
    , ""
    , "  repeat(70) @(posedge $clk$); "
    , "end"
    , "                                                                                                          "
    ] [ ( "name", title )
      , ( "clk", signalClk )
      , ( "rst", signalRst )
      ]
