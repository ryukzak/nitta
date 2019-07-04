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
Module      : NITTA.Model.ProcessorUnits.IO.I2C
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

slave / master / slave-master?
-}
module NITTA.Model.ProcessorUnits.IO.I2C
    ( Ports(..)
    , ExternalPorts(..)
    , I2C
    , i2c
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
import           NITTA.Utils
import           Numeric.Interval                          ((...))
import           Text.InterpolatedString.Perl6             (qc)

data Q v x = Q{ remain, done :: [ v ], function :: F v x }

data I2C v x t = I2C
    { bounceFilter :: Int
    , sendQueue    :: [ Q v x ]
    , receiveQueue :: [ Q v x ]
    , process_     :: Process v x t
    }

i2c :: ( Time t ) => Int -> SPI v x t
i2c bounceFilter = I2C
    { bounceFilter
    , current=Nothing
    , remain=[]
    , process_=def{ nextTick = 1 }
    }


instance ( VarValTime v x t
         ) => ProcessorUnit (I2C v x t) v x t where
    -- Binding to mUnit is carried out by this function.
    tryBind f pu@Multiplier{ remain }
        -- To do this, it is checked whether the function type is reduced to one of the supported
        -- by ('NITTA.FunctionalBlocks.castF')  and in case of success model conditions is returned
        -- after binding with 'Right' mark.
        --
        -- Important to notice, that "binding" doesn't mean actually beginning of work, that
        -- allows firstly make bindings of all tasks and after plan computation process.
        | Just F.Multiply{} <- castF f = Right pu{ remain=f : remain }
        -- In case of impossibility of binding string with short description of renouncement
        --cause and 'Left' is returned.
        | otherwise = Left $ "The function is unsupported by Multiplier: " ++ show f
    -- Unificate interface for get computation process description.
    process = process_
    -- This method is used for set up mUnit time outside.
    -- At the time this is needed only for realisation
    -- of branching, which is on the prototyping stage.
    setTime t pu@Multiplier{} = pu{ tick=t }


-- |This function carry out actual take functional block to work.
assignment pu@Multiplier{ targets=[], sources=[], remain, tick } f
    | Just (F.Multiply (I a) (I b) (O c)) <- castF f
    = pu
        { targets=[a, b]
        , currentWork=Just (tick + 1, f)
        , sources=elems c, remain=remain \\ [ f ]
        }
assignment _ _ = error "Multiplier: internal assignment error."



{-
Result of planning is description of one computation cycle, which later can be translated to microcode,
directly control mUnit. From NITTA architecture point of view, process can be described as
consistent execution two roles by processoe:

- data source ('Source');
- data target ('Target');

The planning process itself consists of two operations performed in a cycle:
-}
instance ( VarValTime v x t
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (I2C v x t)
        where


    --1. Processors is asked about roles it can realise (in the other words, how computation
    --process can develop). It is realised by @options@ functions, result of which is
    --one of the further list:

    --list of variants of uploading to mUnit variables, which are needed to function
    --that is in work;
    options _proxy I2C{ targets=vs@(_:_), tick }
        = map (\v -> EndpointO (Target v) $ TimeConstrain (tick + 1 ... maxBound) (1 ... maxBound)) vs

     --   list of variants of downloading from mUnit variables;
    options _proxy I2C{ sources, doneAt=Just at, tick }
        | not $ null sources
        = [ EndpointO (Source $ fromList sources) $ TimeConstrain (max at (tick + 1) ... maxBound) (1 ... maxBound) ]

    -- list of variables of uploading to mUnit variables, upload any one of that
    -- will cause to actual start of working with mathched function.
    options proxy pu@I2C{ remain } = concatMap (options proxy . assignment pu) remain

    -- Note, that options provided by this function require clarification, because:

    --	1.	They point to not specific moment for work, but to available interval
    --		('NITTA.Types.Base.TimeConstrain'), that describe from and to which time
    -- 		uploading and downloading can be done, and how much time the process can continue.
    -- 	2.	One value can be download from mUnit as several different variables. This can
    --		be done either all at once (on the hardware level the value writed to the bus and
    -- 		read by several processors), as a consistent (firstly value on the bus can be writed for
    --		one mUnit, and after for next one), what should be specified too.


    -- 2. 	Process planning or making decision about compuatation process development to
    --	  	mUnit model state is carried out by @decision@. Variant transformation
    --		from got from @options@ is carried out by CAD outside the mUnit model.
    --		We can distinguish the following solutions:
    --
    --		1. If model wait variable uploading:
    decision _proxy pu@I2C{ targets=vs, currentWorkEndpoints } d@EndpointD{ epdRole=Target v, epdAt }
           -- From the list of uploading value we get a needed value, and remainder is saved
           -- for the next steps.
        | ([_], xs) <- partition (== v) vs
             -- @sel@ veriable is used for uploading queuing of variable to hardware block, that is
             -- requred because of realisation.
        , let sel = if null xs then B else A
             --  Computation process planning is carried out.
        , let (newEndpoints, process_') = runSchedule pu $ do
                -- this is required for correct work of automatically generated tests,
                -- that takes information about time from Process
                updateTick (sup epdAt)
                scheduleEndpoint d $ scheduleInstruction (inf epdAt) (sup epdAt) $ Load sel
        = pu
            { process_=process_'
            -- The remainder of the work is saved for the next loop
            , targets=xs
            -- We save information about events that describe sending or recieving data for
            -- current functionatl unit.
            , currentWorkEndpoints=newEndpoints ++ currentWorkEndpoints
            -- If all required arguments are upload (@null xs@), then the moment of time
            -- when we get a result is saved.
             , doneAt=if null xs
                then Just $ sup epdAt + 3
                else Nothing
            -- Model time is running
             , tick=sup epdAt
            }
--	2. If model is waiting, that we will download variables from it.
    decision _proxy pu@I2C{ targets=[], sources, doneAt, currentWork=Just (a, f), currentWorkEndpoints } d@EndpointD{ epdRole=Source v, epdAt }
        | not $ null sources
        , let sources' = sources \\ elems v
        , sources' /= sources
        -- Compututation process planning is carring on.
        , let (newEndpoints, process_') = runSchedule pu $ do
                endpoints <- scheduleEndpoint d $ scheduleInstruction (inf epdAt) (sup epdAt) Out
                when (null sources') $ do
                    high <- scheduleFunction a (sup epdAt) f
                    let low = endpoints ++ currentWorkEndpoints
                    -- Set up the vertical relantions between functional unit
                    -- and related to that data sending.
                    establishVerticalRelations high low
                -- this is needed to correct work of automatically generated tests
                -- that takes time about time from Process
                updateTick (sup epdAt)
                return endpoints
        = pu
            { process_=process_'
              -- In case if not all variables what asked - remaining are saved.
             , sources=sources'
              -- if all of works is done, then time when result is ready,
              -- current work and data transfering, what is done is the current function is reset.
            , doneAt=if null sources' then Nothing else doneAt
            , currentWork=if null sources' then Nothing else Just (a, f)
            , currentWorkEndpoints=if null sources' then [] else newEndpoints ++ currentWorkEndpoints
              -- Model time is running up
            , tick=sup epdAt
            }
    --    3. If no function is executed at the moment, then we need to find function in the list
    --    of assigned function, executed it to work and only then make decision
    --    and plan a fragment of computation process with call recursion in situation 1.
    decision proxy pu@I2C{ targets=[], sources=[], remain } d
        | let v = oneOf $ variables d
        , Just f <- find (\f -> v `member` variables f) remain
        = decision proxy (assignment pu f) d
    -- If smth went wrong.
    decision _ pu d = error $ "I2C decision error\npu: " ++ show pu ++ ";\n decison:" ++ show d



instance Controllable (I2C v x t) where
    data Instruction (I2C v x t)
        = Receiving
        | Sending
        deriving ( Show )

    data Microcode (I2C v x t) = Microcode
            { wrSignal :: Bool
            , oeSignal :: Bool
            }
        deriving ( Show, Eq, Ord )

    mapMicrocodeToPorts Microcode{..} I2CPorts{..} =
        [ (wr, Bool wrSignal)
        , (oe, Bool oeSignal)
        ]


instance Default (Microcode (I2C v x t)) where
    def = Microcode
        { wrSignal=False
        , oeSignal=False
        }


instance UnambiguouslyDecode (I2C v x t) where
    decodeInstruction Sending   = def{ wrSignal=True }
    decodeInstruction Receiving = def{ oeSignal=True }



instance
        ( VarValTime v x t
        ) => Simulatable (I2C v x t) v x where
    simulateOn cntx _ f
        | Just f'@Send{} <- castF f = simulate cntx f'
        | Just f'@Receive{} <- castF f = simulate cntx f'
        | otherwise = error $ "Can't simulate " ++ show f ++ " on I2C."

data I2CMode
    = I2CMaster
    | I2CSlave

instance Connected (I2C v x t) where
    data Ports (I2C v x t)
        = I2CPorts
            { wr, oe :: SignalTag
             -- |Данный сигнал используется для оповещения процессора о завершении передачи данных. Необходимо для
             -- приостановки работы пока передача не будет завершена, так как в противном случае данные будут потеряны.
            , stop :: String
            , scl :: InputPortTag
            , sda :: InoutPortTag
            , mode :: I2CMode
            }
        deriving ( Show )

    externalInputPorts I2CPorts{ mode=I2CSlave, .. } = [ slave_mosi, slave_sclk, slave_cs ]
    externalInputPorts I2CPorts{ mode=I2CMaster, .. } = [ master_miso ]

    externalOutputPorts I2CPorts{ mode=I2CSlave, .. } = [ slave_miso ]
    externalOutputPorts I2CPorts{ mode=I2CMaster, .. } = [ master_mosi, master_sclk, master_cs ]


-- instance ( VarValTime v x t ) => TargetSystemComponent (SPI v x t) where
--     moduleName _ _ = "pu_spi"
--     hardware _tag _pu
--         = Aggregate Nothing
--             [ FromLibrary "spi/pu_slave_spi_driver.v"
--             , FromLibrary "spi/spi_slave_driver.v"
--             , FromLibrary "spi/spi_to_nitta_splitter.v"
--             , FromLibrary "spi/buffer.v"
--             , FromLibrary "spi/bounce_filter.v"
--             , FromLibrary "spi/spi_master_driver.v"
--             , FromLibrary "spi/nitta_to_spi_splitter.v"
--             , FromLibrary "spi/spi_to_nitta_splitter.v"
--             , FromLibrary "spi/pu_slave_spi.v"
--             , FromLibrary "spi/pu_master_spi.v"
--             ]
--     software _ pu = Immediate "transport.txt" $ show pu
--     hardwareInstance _ _ TargetEnvironment{ unitEnv=NetworkEnv{} } _ = error "wrong environment type, for pu_spi it should be ProcessUnitEnv"
--     hardwareInstance
--             tag
--             SerialPU{ spuState=State{ spiBounceFilter } }
--             TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk, signalRst, signalCycle, inputPort, outputPort }
--             I2CPorts{ externalPorts, .. }
--         = fixIndent [qc|
-- |           { module_ externalPorts }
-- |               #( .DATA_WIDTH( { finiteBitSize (def :: x) } )
-- |                , .ATTR_WIDTH( { show parameterAttrWidth } )
-- |                , .BOUNCE_FILTER( { show spiBounceFilter } )
-- |                ) { tag }
-- |               ( .clk( { signalClk } )
-- |               , .rst( { signalRst } )
-- |               , .flag_stop( { stop } )
-- |               , .signal_cycle( { signalCycle } )
-- |               , .signal_oe( { signal oe } )
-- |               , .signal_wr( { signal wr } )
-- |               , .data_in( { dataIn } ), .attr_in( { attrIn } )
-- |               , .data_out( { dataOut } ), .attr_out( { attrOut } )
-- |               { extIO externalPorts }
-- |               );
-- |           |]
--             where
--                 module_ Slave{}  = "pu_slave_spi"
--                 module_ Master{} = "pu_master_spi"
--                 extIO Slave{..} = fixIndent [qc|
-- |                   , .mosi( { inputPort slave_mosi } )
-- |                   , .miso( { outputPort slave_miso } )
-- |                   , .sclk( { inputPort slave_sclk } )
-- |                   , .cs( { inputPort slave_cs } )
-- |           |]
--                 extIO Master{..} = fixIndent [qc|
-- |                   , .mosi( { outputPort master_mosi } )
-- |                   , .miso( { inputPort master_miso } )
-- |                   , .sclk( { outputPort master_sclk } )
-- |                   , .cs( { outputPort master_cs } )
-- |           |]


-- instance ( VarValTime v x t ) => IOTestBench (SPI v x t) v x where
--     testEnvironmentInitFlag tag _pu = Just $ tag ++ "_env_init_flag"

--     testEnvironment _ _ TargetEnvironment{ unitEnv=NetworkEnv{} } _ _ = error "wrong environment type, for pu_spi it should be ProcessUnitEnv"
--     testEnvironment
--             tag
--             pu@SerialPU{ spuState=State{ spiBounceFilter, spiReceive, spiSend } }
--             TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk, signalRst, inputPort, outputPort }
--             I2CPorts{ externalPorts, .. }
--             cntx@Cntx{ cntxCycleNumber, cntxProcess }
--         | let
--             receivedVariablesSeq = reverse $ map head $ fst spiReceive
--             receivedVarsValues = take cntxCycleNumber $ cntxReceivedBySlice cntx
--             sendedVariableSeq = reverse $ fst spiSend
--             sendedVarsValues = take cntxCycleNumber $ map cycleCntx cntxProcess

--             wordWidth = finiteBitSize (def :: x)
--             frameWordCount = max (length receivedVariablesSeq) (length $ sendedVariableSeq)
--             frameWidth = frameWordCount * wordWidth
--             timeLag = 10 :: Int

--             toVerilogLiteral xs = let
--                     xs' = map (\d -> [qc|{ wordWidth }'sd{ verilogInteger d }|]) xs
--                     placeholder = replicate (frameWordCount - length xs) [qc|{ wordWidth }'d00|]
--                 in S.join ", " (xs' ++ placeholder)

--             Just envInitFlagName = testEnvironmentInitFlag tag pu
--         = case externalPorts of
--             _ | frameWordCount == 0 -> ""
--             Slave{..} -> let
--                     receiveCycle transmit = let
--                             xs = map (\v -> fromMaybe def $ transmit M.!? v) receivedVariablesSeq
--                         in fixIndent [qc|
-- |
-- |                           { tag }_io_test_input = \{ { toVerilogLiteral xs } }; // { xs }
-- |                           { tag }_io_test_start_transaction = 1;                           @(posedge { signalClk });
-- |                           { tag }_io_test_start_transaction = 0;                           @(posedge { signalClk });
-- |                           repeat( { frameWidth * 2 + spiBounceFilter + 2 } ) @(posedge { signalClk });
-- |                   |]

--                     sendingAssert transmit = let
--                             xs = map (\v -> fromMaybe def $ transmit M.!? v) sendedVariableSeq
--                         in fixIndent [qc|
-- |                           @(posedge { tag }_io_test_start_transaction);
-- |                               $display( "{ tag }_io_test_output except: %H (\{ { toVerilogLiteral xs } })", \{ { toVerilogLiteral xs } } );
-- |                               $display( "{ tag }_io_test_output actual: %H", { tag }_io_test_output );
-- |                               if ( { tag }_io_test_output !=  \{ { toVerilogLiteral xs } } )
-- |                                   $display("                       FAIL");
-- |                               $display();
-- |                       |]
--                 in fixIndent [qc|
-- |                   // SPI Input/Output environment
-- |                   // { show pu }
-- |                   reg { tag }_io_test_start_transaction;
-- |                   reg  [{ frameWidth }-1:0] { tag }_io_test_input;
-- |                   wire { tag }_io_test_ready;
-- |                   wire [{ frameWidth }-1:0] { tag }_io_test_output;
-- |                   initial { envInitFlagName } <= 0; // should be defined on the testbench level.
-- |                   spi_master_driver #
-- |                           ( .DATA_WIDTH( { frameWidth } )
-- |                           , .SCLK_HALFPERIOD( 1 )
-- |                           ) { tag }_io_test
-- |                       ( .clk( { signalClk } )
-- |                       , .rst( { signalRst } )
-- |                       , .start_transaction( { tag }_io_test_start_transaction )
-- |                       , .data_in( { tag }_io_test_input )
-- |                       , .data_out( { tag }_io_test_output )
-- |                       , .ready( { tag }_io_test_ready )
-- |                       , .mosi( { inputPort slave_mosi } )
-- |                       , .miso( { outputPort slave_miso } )
-- |                       , .sclk( { inputPort slave_sclk } )
-- |                       , .cs( { inputPort slave_cs } )
-- |                       );
-- |                   initial { tag }_io_test.inner.shiftreg <= 0;
-- |
-- |                   // SPI Input signal generation
-- |                   initial begin
-- |                       { tag }_io_test_start_transaction <= 0; { tag }_io_test_input <= 0;
-- |                       @(negedge { signalRst });
-- |                       repeat({ timeLag }) @(posedge { signalClk });
-- |                       { envInitFlagName } <= 1;
-- |                       { S.join "" $ map receiveCycle receivedVarsValues }
-- |                       repeat(70) @(posedge { signalClk });
-- |                       // $finish; // DON'T DO THAT (with this line test can pass without data checking)
-- |                   end
-- |
-- |                   // SPI Output signal checking
-- |                   initial begin
-- |                       @(negedge { signalRst });
-- |                       repeat (3) @(posedge { tag }_io_test_start_transaction); // latency
-- |                       { S.join "" $ map sendingAssert sendedVarsValues }
-- |                   end
-- |               |]
--             Master{..} -> let
--                     receiveCycle transmit = let
--                             xs = map (\v -> fromMaybe def $ transmit M.!? v) receivedVariablesSeq
--                         in fixIndent [qc|
-- |                           { tag }_io_test_input = \{ { toVerilogLiteral xs } }; // { xs }
-- |                           @(posedge { tag }_io_test_ready);
-- |                   |]

--                     sendingAssert transmit = let
--                             xs = map (\v -> fromMaybe def $ transmit M.!? v) sendedVariableSeq
--                         in fixIndent [qc|
-- |
-- |                          @(posedge { tag }_io_test_ready);
-- |                                   $display( "{ tag }_io_test_output except: %H (\{ { toVerilogLiteral xs } })", \{ { toVerilogLiteral xs } } );
-- |                                   $display( "{ tag }_io_test_output actual: %H", { tag }_io_test_output );
-- |                                   if ( { tag }_io_test_output !=  \{ { toVerilogLiteral xs } } )
-- |                                       $display("                       FAIL");
-- |                                   $display();
-- |                       |]
--                 in fixIndent [qc|
-- |                   // SPI Input/Output environment
-- |                   // { show pu }
-- |                   reg { tag }_io_test_start_transaction;
-- |                   reg  [{ frameWidth }-1:0] { tag }_io_test_input;
-- |                   wire { tag }_io_test_ready;
-- |                   wire [{ frameWidth }-1:0] { tag }_io_test_output;
-- |                   initial { envInitFlagName } <= 0; // should be defined on the testbench level.
-- |                   spi_slave_driver #
-- |                           ( .DATA_WIDTH( { frameWidth } )
-- |                           ) { tag }_io_test_slave
-- |                       ( .clk( { signalClk } )
-- |                       , .rst( { signalRst } )
-- |                       , .data_in( { tag }_io_test_input )
-- |                       , .data_out( { tag }_io_test_output )
-- |                       , .ready( { tag }_io_test_ready )
-- |                       , .mosi( { outputPort master_mosi } )
-- |                       , .miso( { inputPort master_miso } )
-- |                       , .sclk( { outputPort master_sclk } )
-- |                       , .cs( { outputPort master_cs } )
-- |                       );
-- |
-- |                   // SPI Input signal generation
-- |                   initial begin
-- |                       @(negedge { signalRst });
-- |               { receiveCycle $ head receivedVarsValues }
-- |                       { envInitFlagName } <= 1;
-- |               { S.join "" $ map receiveCycle $ tail receivedVarsValues }
-- |                       repeat(70) @(posedge { signalClk });
-- |                       // $finish; // DON'T DO THAT (with this line test can pass without data checking)
-- |                   end
-- |
-- |                   // SPI Output signal checking
-- |                   initial begin
-- |                       @(negedge { signalRst });
-- |                       repeat(2) @(posedge { tag }_io_test_ready);
-- |                       { S.join "" $ map sendingAssert sendedVarsValues }
-- |                   end
-- |               |]
