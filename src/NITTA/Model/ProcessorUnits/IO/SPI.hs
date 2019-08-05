{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Model.ProcessorUnits.IO.SPI
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

slave / master / slave-master?
-}
module NITTA.Model.ProcessorUnits.IO.SPI
    ( Ports(..)
    , ExternalPorts(..)
    , SPI
    , anySPI
    ) where

import           Data.Bits                                 (finiteBitSize)
import           Data.Default
import qualified Data.Map                                  as M
import           Data.Maybe                                (catMaybes,
                                                            fromMaybe)
import           Data.Set                                  (elems, fromList,
                                                            singleton)
import qualified Data.String.Utils                         as S
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.ProcessorUnits.Serial.Generic
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Snippets
import           NITTA.Utils
import           Numeric.Interval                          ((...))
import           Text.InterpolatedString.Perl6             (qc)


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

anySPI :: ( Time t ) => Int -> SPI v x t
anySPI bounceFilter = SerialPU (State def def bounceFilter) def def def{ nextTick = 1 } def



instance ( VarValTime v x t ) => SerialPUState (State v x t) v x t where

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

    mapMicrocodeToPorts Microcode{..} SPIPorts{..} =
        [ (wr, Bool wrSignal)
        , (oe, Bool oeSignal)
        ]


instance Default (Microcode (SPI v x t)) where
    def = Microcode
        { wrSignal=False
        , oeSignal=False
        }


instance UnambiguouslyDecode (SPI v x t) where
    decodeInstruction Sending   = def{ wrSignal=True }
    decodeInstruction Receiving = def{ oeSignal=True }



instance
        ( VarValTime v x t
        ) => Simulatable (SPI v x t) v x where
    simulateOn cntx _ f
        | Just f'@Send{} <- castF f = simulate cntx f'
        | Just f'@Receive{} <- castF f = simulate cntx f'
        | otherwise = error $ "Can't simulate " ++ show f ++ " on SPI."

data ExternalPorts
    = Master
        { master_mosi :: OutputPortTag
        , master_miso :: InputPortTag
        , master_sclk :: OutputPortTag
        , master_cs   :: OutputPortTag
        }
    | Slave
        { slave_mosi :: InputPortTag
        , slave_miso :: OutputPortTag
        , slave_sclk :: InputPortTag
        , slave_cs   :: InputPortTag
        }
    deriving ( Show )

instance Connected (SPI v x t) where
    data Ports (SPI v x t)
        = SPIPorts
            { wr, oe :: SignalTag
             -- |Данный сигнал используется для оповещения процессора о завершении передачи данных. Необходимо для
             -- приостановки работы пока передача не будет завершена, так как в противном случае данные будут потеряны.
            , stop :: String
            , externalPorts :: ExternalPorts
            }
        deriving ( Show )

    externalInputPorts SPIPorts{ externalPorts=Slave{..} } = [ slave_mosi, slave_sclk, slave_cs ]
    externalInputPorts SPIPorts{ externalPorts=Master{..} } = [ master_miso ]

    externalOutputPorts SPIPorts{ externalPorts=Slave{..} } = [ slave_miso ]
    externalOutputPorts SPIPorts{ externalPorts=Master{..} } = [ master_mosi, master_sclk, master_cs ]


instance ( VarValTime v x t ) => TargetSystemComponent (SPI v x t) where
    moduleName _ _ = "pu_spi"
    hardware _tag _pu
        = Aggregate Nothing
            [ FromLibrary "spi/pu_slave_spi_driver.v"
            , FromLibrary "spi/spi_slave_driver.v"
            , FromLibrary "spi/spi_to_nitta_splitter.v"
            , FromLibrary "spi/buffer.v"
            , FromLibrary "spi/bounce_filter.v"
            , FromLibrary "spi/spi_master_driver.v"
            , FromLibrary "spi/nitta_to_spi_splitter.v"
            , FromLibrary "spi/spi_to_nitta_splitter.v"
            , FromLibrary "spi/pu_slave_spi.v"
            , FromLibrary "spi/pu_master_spi.v"
            ]
    software _ pu = Immediate "transport.txt" $ show pu
    hardwareInstance _ _ TargetEnvironment{ unitEnv=NetworkEnv{} } _ = error "wrong environment type, for pu_spi it should be ProcessUnitEnv"
    hardwareInstance
            tag
            SerialPU{ spuState=State{ spiBounceFilter, spiSend, spiReceive } }
            TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk, signalRst, signalCycle, inputPort, outputPort }
            SPIPorts{ externalPorts, .. }
        = fixIndent [qc|
|           { module_ externalPorts }
|               #( .DATA_WIDTH( { finiteBitSize (def :: x) } )
|                , .ATTR_WIDTH( { show parameterAttrWidth } )
|                , .BOUNCE_FILTER( { show spiBounceFilter } )
|                ) { tag }
|               ( .clk( { signalClk } )
|               , .rst( { signalRst } )
|               , .flag_stop( { stop } )
|               , .signal_cycle( { signalCycle } )
|               , .signal_oe( { signal oe } )
|               , .signal_wr( { signal wr } )
|               , .data_in( { dataIn } ), .attr_in( { attrIn } )
|               , .data_out( { dataOut } ), .attr_out( { attrOut } )
|               { extIO externalPorts }
|               );
|           initial { tag }.disabled <= { if spiSend == ([], []) && spiReceive == ([], []) then (1 :: Int) else 0 };
|           |]
            where
                module_ Slave{}  = "pu_slave_spi"
                module_ Master{} = "pu_master_spi"
                extIO Slave{..} = fixIndent [qc|
|                   , .mosi( { inputPort slave_mosi } )
|                   , .miso( { outputPort slave_miso } )
|                   , .sclk( { inputPort slave_sclk } )
|                   , .cs( { inputPort slave_cs } )
|           |]
                extIO Master{..} = fixIndent [qc|
|                   , .mosi( { outputPort master_mosi } )
|                   , .miso( { inputPort master_miso } )
|                   , .sclk( { outputPort master_sclk } )
|                   , .cs( { outputPort master_cs } )
|           |]

instance ( VarValTime v x t, Num x ) => IOTestBench (SPI v x t) v x where
    testEnvironmentInitFlag tag _pu = Just $ tag ++ "_env_init_flag"

    testEnvironment _ _ TargetEnvironment{ unitEnv=NetworkEnv{} } _ _ = error "wrong environment type, for pu_spi it should be ProcessUnitEnv"
    testEnvironment
            tag
            pu@SerialPU{ spuState=State{ spiBounceFilter, spiReceive, spiSend } }
            TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk, signalRst, inputPort, outputPort }
            SPIPorts{ externalPorts, .. }
            cntx@Cntx{ cntxCycleNumber, cntxProcess }
        | let
            receivedVariablesSeq = reverse $ map head $ fst spiReceive
            receivedVarsValues = take cntxCycleNumber $ cntxReceivedBySlice cntx
            sendedVariableSeq = reverse $ fst spiSend
            sendedVarsValues = take cntxCycleNumber $ map cycleCntx cntxProcess

            wordWidth = finiteBitSize (def :: x)
            frameWordCount = max (length receivedVariablesSeq) (length $ sendedVariableSeq)
            frameWidth = frameWordCount * wordWidth
            timeLag = 10 :: Int

            toVerilogLiteral xs = let
                    xs' = map toVerilogLiteral' xs
                    placeholder = replicate (frameWordCount - length xs) [qc|{ wordWidth }'d00|]
                in S.join ", " (xs' ++ placeholder)
            toVerilogLiteral' x
                | abs x /= x = [qc|-{ wordWidth }'sd{ verilogInteger (-x) }|]
                | otherwise = [qc|{ wordWidth }'sd{ verilogInteger x }|]

            Just envInitFlagName = testEnvironmentInitFlag tag pu

            disable = codeBlock 0 [qc|
                initial begin
                    @(negedge { signalRst });
                    { envInitFlagName } <= 1;
                end
                |]

        = case externalPorts of
            Slave{..} -> let
                    receiveCycle transmit = let
                            xs = map (\v -> fromMaybe def $ transmit M.!? v) receivedVariablesSeq
                        in fixIndent [qc|
|
|                           { tag }_io_test_input = \{ { toVerilogLiteral xs } }; // { xs }
|                           { tag }_io_test_start_transaction = 1;                           @(posedge { signalClk });
|                           { tag }_io_test_start_transaction = 0;                           @(posedge { signalClk });
|                           repeat( { frameWidth * 2 + spiBounceFilter + 2 } ) @(posedge { signalClk });
|                   |]

                    sendingAssert transmit = let
                            xs = map (\v -> fromMaybe def $ transmit M.!? v) sendedVariableSeq
                        in fixIndent [qc|
|                           @(posedge { tag }_io_test_start_transaction);
|                               $display( "{ tag }_io_test_output except: %H (\{ { toVerilogLiteral xs } })", \{ { toVerilogLiteral xs } } );
|                               $display( "{ tag }_io_test_output actual: %H", { tag }_io_test_output );
|                               if ( { tag }_io_test_output !=  \{ { toVerilogLiteral xs } } )
|                                   $display("                       FAIL");
|                               $display();
|                       |]

                    envInstance = codeBlock 0 [qc|
                        // SPI Input/Output environment
                        // { show pu }
                        reg { tag }_io_test_start_transaction;
                        reg  [{ frameWidth }-1:0] { tag }_io_test_input;
                        wire { tag }_io_test_ready;
                        wire [{ frameWidth }-1:0] { tag }_io_test_output;
                        initial { envInitFlagName } <= 0; // should be defined on the testbench level.
                        spi_master_driver #
                                ( .DATA_WIDTH( { frameWidth } )
                                , .SCLK_HALFPERIOD( 1 )
                                ) { tag }_io_test
                            ( .clk( { signalClk } )
                            , .rst( { signalRst } )
                            , .start_transaction( { tag }_io_test_start_transaction )
                            , .data_in( { tag }_io_test_input )
                            , .data_out( { tag }_io_test_output )
                            , .ready( { tag }_io_test_ready )
                            , .mosi( { inputPort slave_mosi } )
                            , .miso( { outputPort slave_miso } )
                            , .sclk( { inputPort slave_sclk } )
                            , .cs( { inputPort slave_cs } )
                            );
                        initial { tag }_io_test.inner.shiftreg <= 0;
                        |]
                    interactions = codeBlock 0 [qc|
                        // SPI Input signal generation
                        initial begin
                            { tag }_io_test_start_transaction <= 0; { tag }_io_test_input <= 0;
                            @(negedge { signalRst });
                            repeat({ timeLag }) @(posedge { signalClk });
                            { envInitFlagName } <= 1;
                            { S.join "" $ map receiveCycle receivedVarsValues }
                            repeat(70) @(posedge { signalClk });
                            // $finish; // DON'T DO THAT (with this line test can pass without data checking)
                        end
                        // SPI Output signal checking
                        initial begin
                            @(negedge { signalRst });
                            repeat (3) @(posedge { tag }_io_test_start_transaction); // latency
                            { S.join "" $ map sendingAssert sendedVarsValues }
                        end
                        |]
                    -- FIXME: do not check output signals when we drop data
                in codeBlock 0 [qc|
                    { inline envInstance }
                    { inline $ if frameWordCount == 0 then disable else interactions }
                    |]

            Master{..} -> let
                    receiveCycle transmit = let
                            xs = map (\v -> fromMaybe def $ transmit M.!? v) receivedVariablesSeq
                        in fixIndent [qc|
|                           { tag }_io_test_input = \{ { toVerilogLiteral xs } }; // { xs }
|                           @(posedge { tag }_io_test_ready);
|                   |]

                    sendingAssert transmit = let
                            xs = map (\v -> fromMaybe def $ transmit M.!? v) sendedVariableSeq
                        in fixIndent [qc|
|
|                          @(posedge { tag }_io_test_ready);
|                                   $display( "{ tag }_io_test_output except: %H (\{ { toVerilogLiteral xs } })", \{ { toVerilogLiteral xs } } );
|                                   $display( "{ tag }_io_test_output actual: %H", { tag }_io_test_output );
|                                   if ( { tag }_io_test_output !=  \{ { toVerilogLiteral xs } } )
|                                       $display("                       FAIL");
|                                   $display();
|                       |]
                    interactions = codeBlock 0 [qc|
                        // SPI Input signal generation
                        initial begin
                            @(negedge { signalRst });
                            { receiveCycle $ head receivedVarsValues }
                            { envInitFlagName } <= 1;
                            { S.join "" $ map receiveCycle $ tail receivedVarsValues }
                            repeat(70) @(posedge { signalClk });
                            // $finish; // DON'T DO THAT (with this line test can pass without data checking)
                        end

                        // SPI Output signal checking
                        initial begin
                            @(negedge { signalRst });
                            repeat(2) @(posedge { tag }_io_test_ready);
                            { S.join "" $ map sendingAssert sendedVarsValues }
                        end
                        |]
                in codeBlock 0 [qc|
                    // SPI Input/Output environment
                    // { show pu }
                    reg { tag }_io_test_start_transaction;
                    reg  [{ frameWidth }-1:0] { tag }_io_test_input;
                    wire { tag }_io_test_ready;
                    wire [{ frameWidth }-1:0] { tag }_io_test_output;
                    initial { envInitFlagName } <= 0; // should be defined on the testbench level.
                    spi_slave_driver #
                            ( .DATA_WIDTH( { frameWidth } )
                            ) { tag }_io_test_slave
                        ( .clk( { signalClk } )
                        , .rst( { signalRst } )
                        , .data_in( { tag }_io_test_input )
                        , .data_out( { tag }_io_test_output )
                        , .ready( { tag }_io_test_ready )
                        , .mosi( { outputPort master_mosi } )
                        , .miso( { inputPort master_miso } )
                        , .sclk( { outputPort master_sclk } )
                        , .cs( { outputPort master_cs } )
                        );
                    { inline $ if frameWordCount == 0 then disable else interactions }
                |]
