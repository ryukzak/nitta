{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.ProcessUnits.SPI
Description :
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

slave / master / slave-master?
-}
module NITTA.ProcessUnits.SPI
    ( PUPorts(..)
    , SPI
    , slaveSPI
    ) where

import           Data.Default
import           Data.Maybe                          (catMaybes)
import           Data.Set                            (elems, fromList,
                                                      singleton)
import qualified Data.String.Utils                   as S
import           Data.Typeable
import           NITTA.Functions
import           NITTA.ProcessUnits.Generic.SerialPU
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval                    ((...))
import           Text.InterpolatedString.Perl6       (qc)


type SPI v x t = SerialPU (State v x t) v x t
data State v x t
    = State
        { spiSend         :: ([v], [v])
        , spiReceive      :: ([[v]], [[v]])
        , spiBounceFilter :: Int
        }
    deriving ( Show )

instance Default (State v x t) where
    def = State def def 2

slaveSPI :: ( Time t ) => Int -> SPI v x t
slaveSPI bounceFilter = SerialPU (State def def bounceFilter) def def def{ nextTick = 1 } def



instance ( Var v, Time t, Typeable x ) => SerialPUState (State v x t) v x t where

    bindToState fb st@State{ .. }
        | Just (Send (I v)) <- castF fb
        , let (ds, rs) = spiSend
        = Right st{ spiSend=(ds, v:rs) }

        | Just (Receive (O vs)) <- castF fb
        , let (ds, rs) = spiReceive
        = Right st{ spiReceive=(ds, elems vs : rs) }

        | otherwise = Left $ "The functional block is unsupported by SPI: " ++ show fb

    stateOptions State{ spiSend, spiReceive } now = catMaybes [ send' spiSend, receive' spiReceive ]
        where
            send' (_, v:_) = Just $ EndpointO (Target v) $ TimeConstrain (now + 1 ... maxBound) (1 ... maxBound)
            send' _ = Nothing
            receive' (_, vs:_) = Just $ EndpointO (Source $ fromList vs) $ TimeConstrain (now ... maxBound) (1 ... maxBound)
            receive' _ = Nothing

    simpleSynthesis st@State{ spiSend=(ds, v:rs) } act
        | singleton v == variables act
        = let
            st' = st{ spiSend=(v:ds, rs) }
            work = serialSchedule @(SPI v x t) Sending act
        in (st', work)

    simpleSynthesis st@State{ spiReceive=(ds, vs:rs) } act
        -- FIXME: Выгрузка данных должна осуществляться в несколько шагов. Эту ошибку необходимо исправить здесь и в
        -- аппаратуре.
        | fromList vs == variables act
        = let
            st' = st{ spiReceive=(vs:ds, rs) }
            work = serialSchedule @(SPI v x t) Receiving act
        in (st', work)

    simpleSynthesis _ _ = error "Schedule error! (SPI)"



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
        = Microcode
            { wrSignal :: Bool
            , oeSignal :: Bool
            }
        deriving ( Show, Eq, Ord )


instance Default (Microcode (SPI v x t)) where
    def = Microcode
        { wrSignal=False
        , oeSignal=False
        }


instance UnambiguouslyDecode (SPI v x t) where
    decodeInstruction Sending   = def{ wrSignal=True }
    decodeInstruction Receiving = def{ oeSignal=True }



instance
        ( Ord v
        , Typeable v, Typeable x
        ) => Simulatable (SPI v x t) v x where
    simulateOn cntx _ f
        | Just f'@Send{} <- castF f = simulate cntx f'
        | Just f'@Receive{} <- castF f = simulate cntx f'
        | otherwise = error $ "Can't simulate " ++ show f ++ " on SPI."

instance Connected (SPI v x t) where
    data PUPorts (SPI v x t)
        = PUPorts
            { wr, oe :: Signal
             -- |Данный сигнал используется для оповещения процессора о завершении передачи данных. Необходимо для
             -- приостановки работы пока передача не будет завершена, так как в противном случае данные будут потеряны.
            , stop :: String
            , mosi, sclk, cs :: InputPort
            , miso :: OutputPort
            }
        deriving ( Show )

    transmitToLink Microcode{..} PUPorts{..} =
        [ (wr, Bool wrSignal)
        , (oe, Bool oeSignal)
        ]



instance ( Var v, Time t, Val x ) => TargetSystemComponent (SPI v x t) where
    moduleName _ _ = "pu_slave_spi"
    hardware title pu
        = Aggregate Nothing
            [ FromLibrary "spi/pu_slave_spi_driver.v"
            , FromLibrary "spi/spi_slave_driver.v"
            , FromLibrary "spi/spi_to_nitta_splitter.v"
            , FromLibrary "spi/buffer.v"
            , FromLibrary "spi/bounce_filter.v"
            , FromLibrary "spi/spi_master_driver.v"
            , FromLibrary "spi/nitta_to_spi_splitter.v"
            , FromLibrary "spi/spi_to_nitta_splitter.v"
            , FromLibrary $ "spi/" ++ moduleName title pu ++ ".v"
            ]
    software _ pu = Immidiate "transport.txt" $ show pu
    hardwareInstance
            title
            pu@SerialPU{ spuState=State{ spiBounceFilter } }
            Enviroment{ net=NetEnv{..}, signalClk, signalRst, signalCycle, inputPort, outputPort }
            PUPorts{..}
        = fixIndent [qc|
|           pu_slave_spi
|               #( .DATA_WIDTH( { widthX pu } )
|                , .ATTR_WIDTH( { show parameterAttrWidth } )
|                , .BOUNCE_FILTER( { show spiBounceFilter } )
|                ) { title }
|               ( .clk( { signalClk } )
|               , .rst( { signalRst } )
|               , .signal_cycle( { signalCycle } )
|               , .signal_oe( { signal oe } )
|               , .signal_wr( { signal wr } )
|               , .flag_stop( { stop } )
|               , .data_in( { dataIn } )
|               , .attr_in( { attrIn } )
|               , .data_out( { dataOut } )
|               , .attr_out( { attrOut } )
|               , .mosi( { inputPort mosi } )
|               , .miso( { outputPort miso } )
|               , .sclk( { inputPort sclk } )
|               , .cs( { inputPort cs } )
|               );
|           |]


receiveSequenece SerialPU{ spuState=State{ spiReceive } } = reverse $ map head $ fst spiReceive
sendSequenece SerialPU{ spuState=State{ spiSend } } = reverse $ fst spiSend
receiveData pu cntx = map (get' cntx) $ receiveSequenece pu

instance ( Var v, Show t, Show x, Enum x, Val x ) => IOTest (SPI v x t) v x where
    componentTestEnviroment
            title
            pu@SerialPU{ spuState=State{ spiBounceFilter } }
            Enviroment{ net=NetEnv{..}, signalClk, signalRst, inputPort, outputPort }
            PUPorts{..}
            cntxs
        | let
            wordWidth = fromIntegral $ widthX pu
            frameWordCount = max (length $ receiveSequenece pu) (length $ sendSequenece pu)
            frameWidth = frameWordCount * wordWidth
            ioCycle cntx = fixIndent [qc|
|
|                   { title }_master_in = \{ { dt' } }; // { dt }
|                   { title }_start_transaction = 1;                           @(posedge { signalClk });
|                   { title }_start_transaction = 0;                           @(posedge { signalClk });
|                   repeat( { frameWidth * 2 + spiBounceFilter + 2 } ) @(posedge { signalClk });
|               |]
                where
                    dt = receiveData pu cntx
                    dt' = S.join ", " $ map (\d -> [qc|{ wordWidth }'sd{ fromEnum d }|]) dt ++ replicate (frameWordCount - length dt) [qc|{ wordWidth }'d00|]
        , frameWordCount > 0
        = fixIndent [qc|
|           // { show pu }
|           reg { title }_start_transaction;
|           reg  [{ frameWidth }-1:0] { title }_master_in;
|           wire { title }_ready;
|           wire [{ frameWidth }-1:0] { title }_master_out;
|           spi_master_driver
|               #( .DATA_WIDTH( { frameWidth } )
|                , .SCLK_HALFPERIOD( 1 )
|                ) { title }_master
|               ( .clk( { signalClk } )
|               , .rst( { signalRst } )
|               , .start_transaction( { title }_start_transaction )
|               , .data_in( { title }_master_in )
|               , .data_out( { title }_master_out )
|               , .ready( { title }_ready )
|               , .mosi( { inputPort mosi } )
|               , .miso( { outputPort miso } )
|               , .sclk( { inputPort sclk } )
|               , .cs( { inputPort cs } )
|               );
|           initial { title }_master.inner.shiftreg <= 0;
|
|           initial begin
|               { title }_start_transaction <= 0; { title }_master_in <= 0;
|               @(negedge { signalRst });
|               repeat(8) @(posedge { signalClk });
|               { S.join "" $ map ioCycle cntxs }
|               repeat(70) @(posedge { signalClk });
|               // $finish; // DON'T DO THAT (with this line test can pass without data checking)
|           end
|           |]
        | otherwise = ""
