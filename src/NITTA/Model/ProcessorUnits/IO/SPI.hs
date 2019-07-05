{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
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

-}
module NITTA.Model.ProcessorUnits.IO.SPI
    ( SPI
    , anySPI
    , Ports(..), IOPorts(..)
    ) where

import           Data.Bits                        (finiteBitSize)
import           Data.Default
import           Data.List                        (partition)
import qualified Data.Map                         as M
import           Data.Maybe                       (fromMaybe, mapMaybe)
import qualified Data.Set                         as S
import qualified Data.String.Utils                as S
import qualified NITTA.Intermediate.Functions     as F
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.Problems.Types
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Parts.TestBench
import           NITTA.Utils
import           NITTA.Utils.ProcessDescription
import           Numeric.Interval                 (inf, sup, (...))
import           Text.InterpolatedString.Perl6    (qc)



data SPI v x t = SPI
        { bounceFilter  :: Int
        , bufferSize    :: Maybe Int -- ^if 'Nothing' then size should defined by algorithm
        , receiveQueue  :: [ Q v x ]
        , receiveN      :: Int
        , isReceiveOver :: Bool -- ^set if send buffer overlap receive buffer
        , sendQueue     :: [ Q v x ]
        , sendN         :: Int
        , process_      :: Process v x t
        }
    deriving ( Show )

data Q v x = Q{ vars :: [ v ], function :: F v x, cads :: [ ProcessUid ] }
    deriving ( Show )

anySPI :: ( Time t ) => Int -> SPI v x t
anySPI bounceFilter = SPI
    { bounceFilter
    , bufferSize=Just 6 -- FIXME:
    , receiveQueue=[]
    , receiveN=0
    , isReceiveOver=False
    , sendQueue=[]
    , sendN=0
    , process_=def
    }


instance ( VarValTime v x t
         ) => ProcessorUnit (SPI v x t) v x t where
    tryBind f spi@SPI{ sendQueue, receiveQueue, receiveN, sendN, bufferSize }

        | Just F.Receive{} <- castF f, fromMaybe maxBound bufferSize == receiveN
        = Left $ "SPI to small buffer size"

        | Just F.Send{} <- castF f, fromMaybe maxBound bufferSize == sendN
        = Left $ "SPI to small buffer size"

        | Just (F.Receive (O vs)) <- castF f
        , let ( cads, process_ ) = runSchedule spi $ scheduleFunctionBind f
        = Right spi
            { receiveQueue=Q{ vars=S.elems vs, function=f, cads } : receiveQueue
            , receiveN=receiveN + 1
            , process_
            }

        | Just (F.Send (I v)) <- castF f
        , let ( cads, process_ ) = runSchedule spi $ scheduleFunctionBind f
        = Right spi
            { sendQueue=Q{ vars=[v], function=f, cads } : sendQueue
            , sendN=sendN + 1
            , process_
            }

        | otherwise = Left $ "SPI processor unit do not support: " ++ show f

    process = process_

    setTime t spi@SPI{ process_ } = spi{ process_=process_{ nextTick=t } }


instance ( VarValTime v x t
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (SPI v x t)
        where
    options _proxy SPI{ receiveQueue, sendQueue, process_=Process{ nextTick } } = let
            source vs = EndpointO (Source $ S.fromList vs) $ TimeConstrain (nextTick ... maxBound) (1 ... maxBound)
            receiveOpts = map (source . vars) receiveQueue

            target v = EndpointO (Target v) $ TimeConstrain (nextTick ... maxBound) (1 ... 1)
            sendOpts = map (target . head . vars) sendQueue
        in receiveOpts ++ sendOpts

    decision _proxy spi@SPI{ receiveQueue } d@EndpointD{ epdRole=Source vs, epdAt }
        | ([ Q{ function } ], receiveQueue') <- partition ((vs ==) . S.fromList . vars) receiveQueue
        , let ( _, process_ ) = runSchedule spi $ do
                _ <- scheduleEndpoint d $ scheduleInstruction epdAt Receiving
                updateTick (sup epdAt + 1)
                scheduleFunction (inf epdAt) (sup epdAt) function
        = spi{ receiveQueue=receiveQueue', process_ }

    decision _proxy spi@SPI{ sendQueue, sendN, receiveQueue, receiveN } d@EndpointD{ epdRole=Target v, epdAt }
        | ([ Q{ function } ], sendQueue') <- partition ((v ==) . head . vars) sendQueue
        , let ( _, process_ ) = runSchedule spi $ do
                _ <- scheduleEndpoint d $ scheduleInstruction epdAt Sending
                updateTick (sup epdAt + 1)
                scheduleFunction (inf epdAt) (sup epdAt) function
        = spi
            { sendQueue=sendQueue'
            , isReceiveOver=(sendN - length sendQueue) >= (receiveN - length receiveQueue)
            , process_
            }

    decision _ spi d = error $ "SPI model internal error; decision: " ++ show d ++ "\nSPI model: \n" ++ show spi


instance ( Var v ) => Locks (SPI v x t) v where
    locks SPI{} = []


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
        | Just f'@F.Send{} <- castF f = simulate cntx f'
        | Just f'@F.Receive{} <- castF f = simulate cntx f'
        | otherwise = error $ "Can't simulate " ++ show f ++ " on SPI."

instance Connected (SPI v x t) where
    data Ports (SPI v x t)
        = SPIPorts
            { wr, oe :: SignalTag
             -- |Данный сигнал используется для оповещения процессора о завершении передачи данных. Необходимо для
             -- приостановки работы пока передача не будет завершена, так как в противном случае данные будут потеряны.
            , stop :: String
            }
        deriving ( Show )

instance IOConnected (SPI v x t) where
    data IOPorts (SPI v x t)
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

    externalInputPorts Slave{..} = [ slave_mosi, slave_sclk, slave_cs ]
    externalInputPorts Master{..} = [ master_miso ]

    externalOutputPorts Slave{..} = [ slave_miso ]
    externalOutputPorts Master{..} = [ master_mosi, master_sclk, master_cs ]


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
            SPI{ bounceFilter }
            TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk, signalRst, signalCycle, inputPort, outputPort }
            SPIPorts{..}
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

instance ( VarValTime v x t ) => IOTestBench (SPI v x t) v x where
    testEnvironmentInitFlag tag _pu = Just $ tag ++ "_env_init_flag"

    testEnvironment _ _ TargetEnvironment{ unitEnv=NetworkEnv{} } _ _ _ = error "wrong environment type, for pu_spi it should be ProcessUnitEnv"
    testEnvironment
            tag
            pu@SPI{ process_, bounceFilter }
            TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk, signalRst, inputPort, outputPort }
            SPIPorts{..}
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
                    xs' = map (\d -> [qc|{ wordWidth }'sd{ verilogInteger d }|]) xs
                    placeholder = replicate (frameWordCount - length xs) [qc|{ wordWidth }'d00|]
                in S.join ", " (xs' ++ placeholder)

            Just envInitFlagName = testEnvironmentInitFlag tag pu
        = case ioPorts of
            _ | frameWordCount == 0 -> ""
            Slave{..} -> let
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
                in fixIndent [qc|
|                   // SPI Input/Output environment
|                   // { show pu }
|                   reg { tag }_io_test_start_transaction;
|                   reg  [{ frameWidth }-1:0] { tag }_io_test_input;
|                   wire { tag }_io_test_ready;
|                   wire [{ frameWidth }-1:0] { tag }_io_test_output;
|                   initial { envInitFlagName } <= 0; // should be defined on the testbench level.
|                   spi_master_driver #
|                           ( .DATA_WIDTH( { frameWidth } )
|                           , .SCLK_HALFPERIOD( 1 )
|                           ) { tag }_io_test
|                       ( .clk( { signalClk } )
|                       , .rst( { signalRst } )
|                       , .start_transaction( { tag }_io_test_start_transaction )
|                       , .data_in( { tag }_io_test_input )
|                       , .data_out( { tag }_io_test_output )
|                       , .ready( { tag }_io_test_ready )
|                       , .mosi( { inputPort slave_mosi } )
|                       , .miso( { outputPort slave_miso } )
|                       , .sclk( { inputPort slave_sclk } )
|                       , .cs( { inputPort slave_cs } )
|                       );
|                   initial { tag }_io_test.inner.shiftreg <= 0;
|
|                   // SPI Input signal generation
|                   initial begin
|                       { tag }_io_test_start_transaction <= 0; { tag }_io_test_input <= 0;
|                       @(negedge { signalRst });
|                       repeat({ timeLag }) @(posedge { signalClk });
|                       { envInitFlagName } <= 1;
|                       { S.join "" $ map receiveCycle receivedVarsValues }
|                       repeat(70) @(posedge { signalClk });
|                       // $finish; // DON'T DO THAT (with this line test can pass without data checking)
|                   end
|
|                   // SPI Output signal checking
|                   initial begin
|                       @(negedge { signalRst });
|                       repeat (3) @(posedge { tag }_io_test_start_transaction); // latency
|                       { S.join "" $ map sendingAssert sendedVarsValues }
|                   end
|               |]
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
                in fixIndent [qc|
|                   // SPI Input/Output environment
|                   // { show pu }
|                   reg { tag }_io_test_start_transaction;
|                   reg  [{ frameWidth }-1:0] { tag }_io_test_input;
|                   wire { tag }_io_test_ready;
|                   wire [{ frameWidth }-1:0] { tag }_io_test_output;
|                   initial { envInitFlagName } <= 0; // should be defined on the testbench level.
|                   spi_slave_driver #
|                           ( .DATA_WIDTH( { frameWidth } )
|                           ) { tag }_io_test_slave
|                       ( .clk( { signalClk } )
|                       , .rst( { signalRst } )
|                       , .data_in( { tag }_io_test_input )
|                       , .data_out( { tag }_io_test_output )
|                       , .ready( { tag }_io_test_ready )
|                       , .mosi( { outputPort master_mosi } )
|                       , .miso( { inputPort master_miso } )
|                       , .sclk( { outputPort master_sclk } )
|                       , .cs( { outputPort master_cs } )
|                       );
|
|                   // SPI Input signal generation
|                   initial begin
|                       @(negedge { signalRst });
|               { receiveCycle $ head receivedVarsValues }
|                       { envInitFlagName } <= 1;
|               { S.join "" $ map receiveCycle $ tail receivedVarsValues }
|                       repeat(70) @(posedge { signalClk });
|                       // $finish; // DON'T DO THAT (with this line test can pass without data checking)
|                   end
|
|                   // SPI Output signal checking
|                   initial begin
|                       @(negedge { signalRst });
|                       repeat(2) @(posedge { tag }_io_test_ready);
|                       { S.join "" $ map sendingAssert sendedVarsValues }
|                   end
|               |]
