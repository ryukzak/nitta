{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.Model.ProcessorUnits.IO.SPI
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.IO.SPI (
    SPI,
    anySPI,
    Ports (..),
    IOPorts (..),
) where

import Data.Default
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.String.Utils as S
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.IO.SimpleIO
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Types
import NITTA.Project
import NITTA.Utils
import Text.InterpolatedString.Perl6 (qc)

data SPIinterface

instance SimpleIOInterface SPIinterface

type SPI v x t = SimpleIO SPIinterface v x t

anySPI :: (Time t) => Int -> SPI v x t
anySPI bounceFilter =
    SimpleIO
        { bounceFilter
        , bufferSize = Just 6 -- FIXME:
        , receiveQueue = []
        , receiveN = 0
        , isReceiveOver = False
        , sendQueue = []
        , sendN = 0
        , process_ = def
        }

instance IOConnected (SPI v x t) where
    data IOPorts (SPI v x t)
        = SPIMaster
            { master_mosi :: OutputPortTag
            , master_miso :: InputPortTag
            , master_sclk :: OutputPortTag
            , master_cs :: OutputPortTag
            }
        | SPISlave
            { slave_mosi :: InputPortTag
            , slave_miso :: OutputPortTag
            , slave_sclk :: InputPortTag
            , slave_cs :: InputPortTag
            }
        deriving (Show)

    inputPorts SPISlave{..} = [slave_mosi, slave_sclk, slave_cs]
    inputPorts SPIMaster{..} = [master_miso]

    outputPorts SPISlave{..} = [slave_miso]
    outputPorts SPIMaster{..} = [master_mosi, master_sclk, master_cs]

instance (Time t) => Default (SPI v x t) where
    def = anySPI 0

instance (VarValTime v x t) => TargetSystemComponent (SPI v x t) where
    moduleName _ _ = "pu_spi"
    hardware _tag _pu =
        Aggregate
            Nothing
            [ FromLibrary "spi/pu_slave_spi_driver.v"
            , FromLibrary "spi/spi_slave_driver.v"
            , FromLibrary "spi/i2n_splitter.v"
            , FromLibrary "spi/buffer.v"
            , FromLibrary "spi/bounce_filter.v"
            , FromLibrary "spi/spi_master_driver.v"
            , FromLibrary "spi/n2i_splitter.v"
            , FromLibrary "spi/pu_slave_spi.v"
            , FromLibrary "spi/pu_master_spi.v"
            ]
    software _ pu = Immediate "transport.txt" $ show pu
    hardwareInstance _ _ TargetEnvironment{unitEnv = NetworkEnv{}} _ports _io = error "wrong environment type, for pu_spi it should be ProcessUnitEnv"
    hardwareInstance
        tag
        SimpleIO{bounceFilter, sendN, receiveN}
        TargetEnvironment
            { unitEnv = ProcessUnitEnv{..}
            , signalClk
            , signalRst
            , signalCycleBegin
            , signalInCycle
            , signalCycleEnd
            , inputPort
            , outputPort
            }
        SimpleIOPorts{..}
        ioPorts =
            codeBlock
                [qc|
            { module_ ioPorts } #
                    ( .DATA_WIDTH( { dataWidth (def :: x) } )
                    , .ATTR_WIDTH( { attrWidth (def :: x) } )
                    , .BOUNCE_FILTER( { show bounceFilter } )
                    , .DISABLED( { if sendN == 0 && receiveN == 0 then (1 :: Int) else 0 } )
                    ) { tag }
                ( .clk( { signalClk } )
                , .rst( { signalRst } )
                , .flag_stop( { stop } )
                , .signal_cycle_begin( { signalCycleBegin } )
                , .signal_in_cycle( { signalInCycle  } )
                , .signal_cycle_end( { signalCycleEnd } )
                , .signal_oe( { signal oe } )
                , .signal_wr( { signal wr } )
                , .data_in( { dataIn } ), .attr_in( { attrIn } )
                , .data_out( { dataOut } ), .attr_out( { attrOut } )
                { inline $ extIO ioPorts }
                );
            |]
            where
                module_ SPISlave{} = "pu_slave_spi"
                module_ SPIMaster{} = "pu_master_spi"
                extIO SPISlave{..} =
                    codeBlock
                        [qc|
                        , .mosi( { inputPort slave_mosi } )
                        , .miso( { outputPort slave_miso } )
                        , .sclk( { inputPort slave_sclk } )
                        , .cs( { inputPort slave_cs } )
                        |]
                extIO SPIMaster{..} =
                    codeBlock
                        [qc|
                        , .mosi( { outputPort master_mosi } )
                        , .miso( { inputPort master_miso } )
                        , .sclk( { outputPort master_sclk } )
                        , .cs( { outputPort master_cs } )
                        |]

instance (VarValTime v x t, Num x) => IOTestBench (SPI v x t) v x where
    testEnvironmentInitFlag tag _pu = Just $ tag ++ "_env_init_flag"

    testEnvironment _ _ TargetEnvironment{unitEnv = NetworkEnv{}} _ _ _ = error "wrong environment type, for pu_spi it should be ProcessUnitEnv"
    testEnvironment
        tag
        sio@SimpleIO{process_, bounceFilter}
        TargetEnvironment{unitEnv = ProcessUnitEnv{..}, signalClk, signalRst, inputPort, outputPort}
        SimpleIOPorts{..}
        ioPorts
        TestEnvironment{teCntx = cntx@Cntx{cntxCycleNumber, cntxProcess}, teComputationDuration} =
            let receivedVariablesSeq =
                    mapMaybe
                        ( \f -> case castF f of
                            Just Receive{} -> Just $ oneOf $ variables f
                            _ -> Nothing
                        )
                        $ functions process_
                receivedVarsValues = take cntxCycleNumber $ cntxReceivedBySlice cntx
                sendedVariableSeq =
                    mapMaybe
                        ( \case
                            (Target v) -> Just v
                            _ -> Nothing
                        )
                        $ getEndpoints process_
                sendedVarsValues = take cntxCycleNumber $ map cycleCntx cntxProcess
                wordWidth = dataWidth (def :: x)
                frameWordCount = max (length receivedVariablesSeq) $ length sendedVariableSeq
                frameWidth = frameWordCount * wordWidth
                timeLag = 10 :: Int
                sendingDuration =
                    max
                        (teComputationDuration + 2)
                        (frameWidth * 2 + bounceFilter + 2)

                toVerilogLiteral xs =
                    let xs' = map toVerilogLiteral' xs
                        placeholder = replicate (frameWordCount - length xs) [qc|{ wordWidth }'d00|]
                     in S.join ", " (xs' ++ placeholder)
                toVerilogLiteral' x
                    | abs x /= x = [qc|-{ wordWidth }'sd{ dataLiteral (-x) }|]
                    | otherwise = [qc|{ wordWidth }'sd{ dataLiteral x }|]

                disable =
                    codeBlock
                        [qc|
                initial begin
                    @(negedge { signalRst });
                    { envInitFlagName } <= 1;
                end
                |]

                Just envInitFlagName = testEnvironmentInitFlag tag sio
             in case ioPorts of
                    SPISlave{..} ->
                        let receiveCycle transmit =
                                let xs = map (\v -> fromMaybe def $ transmit M.!? v) receivedVariablesSeq
                                 in codeBlock
                                        [qc|
                            $display( "set data for sending { xs } by { tag }_io_test_input" );
                            { tag }_io_test_input = \{ { toVerilogLiteral xs } }; // { xs }
                            { tag }_io_test_start_transaction = 1;                           @(posedge { signalClk });
                            { tag }_io_test_start_transaction = 0;                           @(posedge { signalClk });
                            repeat( { sendingDuration } ) @(posedge { signalClk });

                            |]

                            sendingAssert transmit =
                                let xs = map (\v -> fromMaybe def $ transmit M.!? v) sendedVariableSeq
                                 in codeBlock
                                        [qc|
                            @(posedge { tag }_io_test_start_transaction);
                                $write( "{ tag }_io_test_output actual: %H except: %H (\{ { toVerilogLiteral xs } })",
                                    { tag }_io_test_output, \{ { toVerilogLiteral xs } } );
                                if ( { tag }_io_test_output != \{ { toVerilogLiteral xs } } ) $display("\t\tFAIL");
                                else $display();

                            |]

                            endDeviceInstance =
                                codeBlock
                                    [qc|
                        { inline $ comment $ show sio }
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

                            envDeviceControl =
                                codeBlock
                                    [qc|
                        initial begin
                            { tag }_io_test_start_transaction <= 0;
                            { tag }_io_test_input <= 0;
                            @(negedge { signalRst });
                            repeat({ timeLag }) @(posedge { signalClk });

                            { inline $ concat $ map receiveCycle receivedVarsValues }
                            repeat ( 5 ) begin
                                { inline $ receiveCycle def }
                            end

                            // $finish; // DON'T DO THAT (with this line test can pass without data checking)
                        end
                        |]
                            envDeviceCheck =
                                codeBlock
                                    [qc|
                        initial begin
                            @(negedge { signalRst });
                            repeat ( OUTPUT_LATENCY ) @(posedge { tag }_io_test_start_transaction); // latency

                            { inline $ concat $ map sendingAssert sendedVarsValues }
                            forever begin
                                @(posedge spi_io_test_start_transaction);
                                $display( "{ tag }_io_test_output actual: %H", { tag }_io_test_output );
                            end
                        end
                        |]
                         in -- FIXME: do not check output signals when we drop data
                            codeBlock
                                [qc|
                    ////////////////////////////////////////
                    // SPI test environment
                    localparam NITTA_LATENCY = 1;
                    localparam OUTPUT_LATENCY = 3;

                    // SPI device in test environment
                    { inline endDeviceInstance }

                    // SPI device in test environment control
                    { inline $ if frameWordCount == 0 then disable else envDeviceControl }

                    // SPI device in test environment check
                    { inline $ if frameWordCount == 0 then disable else envDeviceCheck }

                    // SPI environment initialization flag set
                    initial begin
                        repeat ( NITTA_LATENCY ) @(posedge spi_io_test_start_transaction);
                        spi_env_init_flag <= 1;
                    end
                    |]
                    SPIMaster{..} ->
                        let receiveCycle transmit =
                                let xs = map (\v -> fromMaybe def $ transmit M.!? v) receivedVariablesSeq
                                 in codeBlock
                                        [qc|
                            { tag }_io_test_input = \{ { toVerilogLiteral xs } }; // { xs }
                            @(posedge { tag }_io_test_ready);
                            |]

                            sendingAssert transmit =
                                let xs = map (\v -> fromMaybe def $ transmit M.!? v) sendedVariableSeq
                                 in codeBlock
                                        [qc|
                            @(posedge { tag }_io_test_ready);
                                $display( "{ tag }_io_test_output except: %H (\{ { toVerilogLiteral xs } })", \{ { toVerilogLiteral xs } } );
                                $display( "{ tag }_io_test_output actual: %H", { tag }_io_test_output );
                                if ( { tag }_io_test_output !=  \{ { toVerilogLiteral xs } } )
                                    $display("                       FAIL");
                                $display();
                            |]

                            envInstance =
                                codeBlock
                                    [qc|
                        { inline $ comment $ show sio }
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
                        |]

                            interactions =
                                codeBlock
                                    [qc|
                        // SPI Input signal generation
                        initial begin
                            @(negedge { signalRst });
                            { inline $ receiveCycle $ head receivedVarsValues }
                            { envInitFlagName } <= 1;

                            { inline $ concat $ map receiveCycle $ tail receivedVarsValues }
                            repeat(70) @(posedge { signalClk });
                            // $finish; // DON'T DO THAT (with this line test can pass without data checking)
                        end

                        // SPI Output signal checking
                        initial begin
                            @(negedge { signalRst });
                            repeat(2) @(posedge { tag }_io_test_ready);
                            { inline $ concat $ map sendingAssert sendedVarsValues }
                        end
                        |]
                         in codeBlock
                                [qc|
                    { inline envInstance }

                    { inline $ if frameWordCount == 0 then disable else interactions }
                    |]
