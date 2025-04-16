{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
    spiMasterPorts,
    spiSlavePorts,
) where

import Data.Aeson
import Data.Default
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S
import Data.String.Interpolate
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.IO.SimpleIO
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Time
import NITTA.Project
import NITTA.Utils
import Prettyprinter

data SPIinterface

instance SimpleIOInterface SPIinterface

type SPI v x t = SimpleIO SPIinterface v x t

anySPI :: Time t => Int -> Maybe Int -> SPI v x t
anySPI bounceFilter bufferSize =
    SimpleIO
        { bounceFilter
        , bufferSize
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

    inputPorts SPISlave{..} = S.fromList [slave_mosi, slave_sclk, slave_cs]
    inputPorts SPIMaster{..} = S.fromList [master_miso]

    outputPorts SPISlave{..} = S.fromList [slave_miso]
    outputPorts SPIMaster{..} = S.fromList [master_mosi, master_sclk, master_cs]

spiMasterPorts tag =
    SPIMaster
        { master_mosi = OutputPortTag $ tag <> "_mosi"
        , master_miso = InputPortTag $ tag <> "_miso"
        , master_sclk = OutputPortTag $ tag <> "_sclk"
        , master_cs = OutputPortTag $ tag <> "_cs"
        }

spiSlavePorts tag =
    SPISlave
        { slave_mosi = InputPortTag $ tag <> "_mosi"
        , slave_miso = OutputPortTag $ tag <> "_miso"
        , slave_sclk = InputPortTag $ tag <> "_sclk"
        , slave_cs = InputPortTag $ tag <> "_cs"
        }

instance Time t => Default (SPI v x t) where
    def = anySPI 0 $ Just 6

instance (ToJSON v, VarValTime v x t) => TargetSystemComponent (SPI v x t) where
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

    software tag pu = protocolDescription tag pu "SPI Processor Unit"

    hardwareInstance
        tag
        SimpleIO{bounceFilter, sendN, receiveN}
        UnitEnv
            { sigClk
            , sigRst
            , sigCycleBegin
            , sigInCycle
            , sigCycleEnd
            , valueIn = Just (dataIn, attrIn)
            , valueOut = Just (dataOut, attrOut)
            , ctrlPorts = Just SimpleIOPorts{..}
            , ioPorts = Just ioPorts
            } =
            [__i|
                #{ module_ ioPorts } \#
                        ( .DATA_WIDTH( #{ dataWidth (def :: x) } )
                        , .ATTR_WIDTH( #{ attrWidth (def :: x) } )
                        , .BOUNCE_FILTER( #{ show bounceFilter } )
                        , .DISABLED( #{ if sendN == 0 && receiveN == 0 then (1 :: Int) else 0 } )
                        ) #{ tag }
                    ( .clk( #{ sigClk } )
                    , .rst( #{ sigRst } )
                    , .flag_stop( #{ stop } )
                    , .signal_cycle_begin( #{ sigCycleBegin } )
                    , .signal_in_cycle( #{ sigInCycle  } )
                    , .signal_cycle_end( #{ sigCycleEnd } )
                    , .signal_oe( #{ oe } )
                    , .signal_wr( #{ wr } )
                    , .data_in( #{ dataIn } ), .attr_in( #{ attrIn } )
                    , .data_out( #{ dataOut } ), .attr_out( #{ attrOut } )
                    #{ nest 4 $ extIO ioPorts  }
                    );
            |]
            where
                module_ SPISlave{} = "pu_slave_spi" :: Verilog
                module_ SPIMaster{} = "pu_master_spi"
                extIO SPISlave{..} =
                    [__i|
                        , .mosi( #{ slave_mosi } )
                        , .miso( #{ slave_miso } )
                        , .sclk( #{ slave_sclk } )
                        , .cs( #{ slave_cs } )
                    |] ::
                        Verilog
                extIO SPIMaster{..} =
                    [__i|
                        , .mosi( #{ master_mosi } )
                        , .miso( #{ master_miso } )
                        , .sclk( #{ master_sclk } )
                        , .cs( #{ master_cs } )
                    |]
    hardwareInstance _title _pu _env = error "internal error"

instance VarValTime v x t => IOTestBench (SPI v x t) v x where
    testEnvironmentInitFlag tag _pu = Just $ tag <> "_env_init_flag"

    testEnvironment
        tag
        sio@SimpleIO{process_, bounceFilter}
        UnitEnv
            { sigClk
            , sigRst
            , ctrlPorts = Just SimpleIOPorts{}
            , ioPorts = Just ioPorts
            }
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
                frameWidth =
                    frameWordCount * wordWidth
                timeLag = 10 :: Int
                sendingDuration =
                    max
                        (teComputationDuration + 2)
                        (frameWidth * 2 + bounceFilter + 2)

                toVerilogLiteral xs =
                    let xs' = map toVerilogLiteral' xs
                        placeholder = replicate (frameWordCount - length xs) [i|#{ wordWidth }'d00|]
                     in hsep $ punctuate ", " (xs' <> placeholder)
                toVerilogLiteral' x
                    | abs x /= x = [i|-#{ wordWidth }'sd#{ dataLiteral (-x) }|]
                    | otherwise = [i|#{ wordWidth }'sd#{ dataLiteral x }|]

                disable =
                    [__i|
                        initial begin
                            @(negedge #{ sigRst });
                            #{ envInitFlagName } <= 1;
                        end
                    |]

                envInitFlagName =
                    fromMaybe (error "SPI: testEnvironment: internal error") $
                        testEnvironmentInitFlag tag sio
             in case ioPorts of
                    SPISlave{..} ->
                        let receiveCycle transmit =
                                let xs = map (\v -> fromMaybe def $ transmit M.!? v) receivedVariablesSeq
                                 in [__i|
                                        $display( "set data for sending #{ viaShow xs } by #{ tag }_io_test_input" );
                                        #{ tag }_io_test_input = { #{ toVerilogLiteral xs } }; // #{ viaShow xs }
                                        #{ tag }_io_test_start_transaction = 1;                           @(posedge #{ sigClk });
                                        #{ tag }_io_test_start_transaction = 0;                           @(posedge #{ sigClk });
                                        repeat( #{ sendingDuration } ) @(posedge #{ sigClk });

                                    |]

                            sendingAssert transmit =
                                let xs = map (\v -> fromMaybe def $ HM.lookup v transmit) sendedVariableSeq
                                 in [__i|
                                        @(posedge #{ tag }_io_test_start_transaction);
                                            $write( "#{ tag }_io_test_output actual: %H except: %H ({ #{ toVerilogLiteral xs } })",
                                                #{ tag }_io_test_output, { #{ toVerilogLiteral xs } } );
                                            if ( #{ tag }_io_test_output != { #{ toVerilogLiteral xs } } ) $display("\t\tFAIL");
                                            else $display();

                                    |]

                            endDeviceInstance =
                                [__i|
                                    /*
                                    #{ pretty sio }
                                    */
                                    reg #{ tag }_io_test_start_transaction;
                                    reg  [#{ frameWidth }-1:0] #{ tag }_io_test_input;
                                    wire #{ tag }_io_test_ready;
                                    wire [#{ frameWidth }-1:0] #{ tag }_io_test_output;
                                    initial #{ envInitFlagName } <= 0; // should be defined on the testbench level.
                                    spi_master_driver \#
                                            ( .DATA_WIDTH( #{ frameWidth } )
                                            , .SCLK_HALFPERIOD( 1 )
                                            ) #{ tag }_io_test
                                        ( .clk( #{ sigClk } )
                                        , .rst( #{ sigRst } )
                                        , .start_transaction( #{ tag }_io_test_start_transaction )
                                        , .data_in( #{ tag }_io_test_input )
                                        , .data_out( #{ tag }_io_test_output )
                                        , .ready( #{ tag }_io_test_ready )
                                        , .mosi( #{ slave_mosi } )
                                        , .miso( #{ slave_miso } )
                                        , .sclk( #{ slave_sclk } )
                                        , .cs( #{ slave_cs } )
                                        );
                                    initial #{ tag }_io_test.inner.shiftreg <= 0;
                                |]

                            envDeviceControl =
                                [__i|
                                    initial begin
                                        #{ tag }_io_test_start_transaction <= 0;
                                        #{ tag }_io_test_input <= 0;
                                        @(negedge #{ sigRst });
                                        repeat(#{ timeLag }) @(posedge #{ sigClk });

                                        #{ nest 4 $ vsep $ map receiveCycle receivedVarsValues }
                                        repeat ( 5 ) begin
                                            #{ nest 8 $ receiveCycle def }
                                        end

                                        // $finish; // DON'T DO THAT (with this line test can pass without data checking)
                                    end
                                |]
                            envDeviceCheck =
                                [__i|
                                    initial begin
                                        @(negedge #{ sigRst });
                                        repeat ( OUTPUT_LATENCY ) @(posedge #{ tag }_io_test_start_transaction); // latency

                                        #{ nest 4 $ vsep $ map sendingAssert sendedVarsValues }
                                        forever begin
                                            @(posedge spi_io_test_start_transaction);
                                            $display( "#{ tag }_io_test_output actual: %H", #{ tag }_io_test_output );
                                        end
                                    end
                                |]
                         in -- FIXME: do not check output signals when we drop data
                            Just
                                [__i|
                                    ////////////////////////////////////////
                                    // SPI test environment
                                    localparam NITTA_LATENCY = 1;
                                    localparam OUTPUT_LATENCY = 3;

                                    // SPI device in test environment
                                    #{ endDeviceInstance :: Verilog }

                                    // SPI device in test environment control
                                    #{ if frameWordCount == 0 then disable else envDeviceControl }

                                    // SPI device in test environment check
                                    #{ if frameWordCount == 0 then disable else envDeviceCheck }

                                    // SPI environment initialization flag set
                                    initial begin
                                        repeat ( NITTA_LATENCY ) @(posedge spi_io_test_start_transaction);
                                        spi_env_init_flag <= 1;
                                    end
                                |]
                    SPIMaster{..} ->
                        let receiveCycle transmit =
                                let xs = map (\v -> fromMaybe def $ transmit M.!? v) receivedVariablesSeq
                                 in [__i|
                                        #{ tag }_io_test_input = { #{ toVerilogLiteral xs } }; // #{ xs }
                                        @(posedge #{ tag }_io_test_ready);
                                    |]

                            sendingAssert transmit =
                                let xs = map (\v -> fromMaybe def $ HM.lookup v transmit) sendedVariableSeq
                                 in [__i|
                                        @(posedge #{ tag }_io_test_ready);
                                            $display( "#{ tag }_io_test_output except: %H ({ #{ toVerilogLiteral xs } })", { #{ toVerilogLiteral xs } } );
                                            $display( "#{ tag }_io_test_output actual: %H", #{ tag }_io_test_output );
                                            if ( #{ tag }_io_test_output !=  { #{ toVerilogLiteral xs } } )
                                                $display("                       FAIL");
                                            $display();
                                    |]

                            envInstance =
                                [__i|
                                    /*
                                    #{ pretty sio }
                                    */
                                    reg #{ tag }_io_test_start_transaction;
                                    reg  [#{ frameWidth }-1:0] #{ tag }_io_test_input;
                                    wire #{ tag }_io_test_ready;
                                    wire [#{ frameWidth }-1:0] #{ tag }_io_test_output;
                                    initial #{ envInitFlagName } <= 0; // should be defined on the testbench level.
                                    spi_slave_driver \#
                                            ( .DATA_WIDTH( #{ frameWidth } )
                                            ) #{ tag }_io_test_slave
                                        ( .clk( #{ sigClk } )
                                        , .rst( #{ sigRst } )
                                        , .data_in( #{ tag }_io_test_input )
                                        , .data_out( #{ tag }_io_test_output )
                                        , .ready( #{ tag }_io_test_ready )
                                        , .mosi( #{ master_mosi } )
                                        , .miso( #{ master_miso } )
                                        , .sclk( #{ master_sclk } )
                                        , .cs( #{ master_cs } )
                                        );
                                |]

                            interactions =
                                [__i|
                                    // SPI Input signal generation
                                    initial begin
                                        @(negedge #{ sigRst });
                                        #{ nest 4 $ receiveCycle $ head receivedVarsValues }
                                        #{ envInitFlagName } <= 1;

                                        #{ nest 4 $ vsep $ map receiveCycle $ tail receivedVarsValues }
                                        repeat(70) @(posedge #{ sigClk });
                                        // $finish; // DON'T DO THAT (with this line test can pass without data checking)
                                    end

                                    // SPI Output signal checking
                                    initial begin
                                        @(negedge #{ sigRst });
                                        repeat(2) @(posedge #{ tag }_io_test_ready);
                                        #{ nest 4 $ vsep $ map sendingAssert sendedVarsValues }
                                    end
                                |]
                         in Just (envInstance <> line <> line <> if frameWordCount == 0 then disable else interactions)
    testEnvironment _title _pu _env _tEnv = error "internal error"
