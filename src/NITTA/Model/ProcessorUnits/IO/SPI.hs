{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Model.ProcessorUnits.IO.SPI
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

-}
module NITTA.Model.ProcessorUnits.IO.SPI
    ( SPI
    , anySPI
    , Ports(..), IOPorts(..)
    ) where

import           Data.Bits                              (finiteBitSize)
import           Data.Default
import qualified Data.Map                               as M
import           Data.Maybe                             (fromMaybe, mapMaybe)
import qualified Data.Set                               as S
import qualified Data.String.Utils                      as S
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.ProcessorUnits.IO.SimpleIO
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Snippets
import           NITTA.Utils
import           Text.InterpolatedString.Perl6          (qc)


data SPIinterface

instance SimpleIOInterface SPIinterface

type SPI v x t = SimpleIO SPIinterface v x t

anySPI :: ( Time t ) => Int -> SPI v x t
anySPI bounceFilter = SimpleIO
    { bounceFilter
    , bufferSize=Just 6 -- FIXME:
    , receiveQueue=[]
    , receiveN=0
    , isReceiveOver=False
    , sendQueue=[]
    , sendN=0
    , process_=def
    }


instance IOConnected (SPI v x t) where
    data IOPorts (SPI v x t)
        = SPIMaster
            { master_mosi :: OutputPortTag
            , master_miso :: InputPortTag
            , master_sclk :: OutputPortTag
            , master_cs   :: OutputPortTag
            }
        | SPISlave
            { slave_mosi :: InputPortTag
            , slave_miso :: OutputPortTag
            , slave_sclk :: InputPortTag
            , slave_cs   :: InputPortTag
            }
        deriving ( Show )

    inputPorts SPISlave{..}  = [ slave_mosi, slave_sclk, slave_cs ]
    inputPorts SPIMaster{..} = [ master_miso ]

    outputPorts SPISlave{..}  = [ slave_miso ]
    outputPorts SPIMaster{..} = [ master_mosi, master_sclk, master_cs ]


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
    hardwareInstance _ _ TargetEnvironment{ unitEnv=NetworkEnv{} } _ports _io = error "wrong environment type, for pu_spi it should be ProcessUnitEnv"
    hardwareInstance
            tag
            SimpleIO{ bounceFilter, sendN, receiveN }
            TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk, signalRst, signalCycle, inputPort, outputPort }
            SimpleIOPorts{..}
            ioPorts
        = fixIndent [qc|
|           { module_ ioPorts }
|               #( .DATA_WIDTH( { finiteBitSize (def :: x) } )
|                , .ATTR_WIDTH( { show parameterAttrWidth } )
|                , .BOUNCE_FILTER( { show bounceFilter } )
|                ) { tag }
|               ( .clk( { signalClk } )
|               , .rst( { signalRst } )
|               , .flag_stop( { stop } )
|               , .signal_cycle( { signalCycle } )
|               , .signal_oe( { signal oe } )
|               , .signal_wr( { signal wr } )
|               , .data_in( { dataIn } ), .attr_in( { attrIn } )
|               , .data_out( { dataOut } ), .attr_out( { attrOut } )
|               { extIO ioPorts }
|               );
|           initial { tag }.disabled <= { if sendN == 0 && receiveN == 0 then (1 :: Int) else 0 };
|           |]
            where
                module_ SPISlave{}  = "pu_slave_spi"
                module_ SPIMaster{} = "pu_master_spi"
                extIO SPISlave{..} = fixIndent [qc|
|                   , .mosi( { inputPort slave_mosi } )
|                   , .miso( { outputPort slave_miso } )
|                   , .sclk( { inputPort slave_sclk } )
|                   , .cs( { inputPort slave_cs } )
|           |]
                extIO SPIMaster{..} = fixIndent [qc|
|                   , .mosi( { outputPort master_mosi } )
|                   , .miso( { inputPort master_miso } )
|                   , .sclk( { outputPort master_sclk } )
|                   , .cs( { outputPort master_cs } )
|           |]

instance ( VarValTime v x t, Num x ) => IOTestBench (SPI v x t) v x where
    testEnvironmentInitFlag tag _pu = Just $ tag ++ "_env_init_flag"

    testEnvironment _ _ TargetEnvironment{ unitEnv=NetworkEnv{} } _ _ _ = error "wrong environment type, for pu_spi it should be ProcessUnitEnv"
    testEnvironment
            tag
            sio@SimpleIO{ process_, bounceFilter }
            TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk, signalRst, inputPort, outputPort }
            SimpleIOPorts{..}
            ioPorts
            cntx@Cntx{ cntxCycleNumber, cntxProcess }
        | let
            receivedVariablesSeq = mapMaybe (\case
                    Source vs -> Just $ head $ S.elems vs
                    _ -> Nothing
                ) $ getEndpoints process_
            receivedVarsValues = take cntxCycleNumber $ cntxReceivedBySlice cntx
            sendedVariableSeq = mapMaybe (\case
                    (Target v) -> Just v
                    _ -> Nothing
                ) $ getEndpoints process_
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

            disable = codeBlock 0 [qc|
                initial begin
                    @(negedge { signalRst });
                    { envInitFlagName } <= 1;
                end
                |]

            Just envInitFlagName = testEnvironmentInitFlag tag sio
        = case ioPorts of
            SPISlave{..} -> let
                    receiveCycle transmit = let
                            xs = map (\v -> fromMaybe def $ transmit M.!? v) receivedVariablesSeq
                        in fixIndent [qc|
|
|                           { tag }_io_test_input = \{ { toVerilogLiteral xs } }; // { xs }
|                           { tag }_io_test_start_transaction = 1;                           @(posedge { signalClk });
|                           { tag }_io_test_start_transaction = 0;                           @(posedge { signalClk });
|                           repeat( { frameWidth * 2 + bounceFilter + 2 } ) @(posedge { signalClk });
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
                        // { show sio }
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
                            { inline $ concat $ map receiveCycle receivedVarsValues }
                            repeat(70) @(posedge { signalClk });
                            // $finish; // DON'T DO THAT (with this line test can pass without data checking)
                        end

                        // SPI Output signal checking
                        initial begin
                            @(negedge { signalRst });
                            repeat (3) @(posedge { tag }_io_test_start_transaction); // latency
                            { inline $ concat $ map sendingAssert sendedVarsValues }
                        end
                        |]
                    -- FIXME: do not check output signals when we drop data
                in codeBlock 0 [qc|
                    { inline envInstance }

                    { inline $ if frameWordCount == 0 then disable else interactions }
                    |]

            SPIMaster{..} -> let
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
                            { concat $ map receiveCycle $ tail receivedVarsValues }
                            repeat(70) @(posedge { signalClk });
                            // $finish; // DON'T DO THAT (with this line test can pass without data checking)
                        end

                        // SPI Output signal checking
                        initial begin
                            @(negedge { signalRst });
                            repeat(2) @(posedge { tag }_io_test_ready);
                            { concat $ map sendingAssert sendedVarsValues }
                        end
                        |]
                in codeBlock 0 [qc|
                    // SPI Input/Output environment
                    // { show sio }
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
