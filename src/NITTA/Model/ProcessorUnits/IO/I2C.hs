{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.Model.ProcessorUnits.IO.I2C
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.IO.I2C (
    I2C,
    i2cUnit,
    Ports (..),
    IOPorts (..),
) where

import Data.Default
import NITTA.Intermediate.Value
import NITTA.Model.ProcessorUnits.IO.SimpleIO
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Types
import NITTA.Project
import NITTA.Utils
import Text.InterpolatedString.Perl6 (qc)

data I2Cinterface

instance SimpleIOInterface I2Cinterface

type I2C v x t = SimpleIO I2Cinterface v x t

i2cUnit :: (Time t) => Int -> I2C v x t
i2cUnit bounceFilter =
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

instance IOConnected (I2C v x t) where
    data IOPorts (I2C v x t)
        = I2CMaster
            { masterSDA :: InoutPortTag
            , masterSCL :: OutputPortTag
            }
        | I2CSlave
            { slaveSDA :: InoutPortTag
            , slaveSCL :: InputPortTag
            }
        deriving (Show)

    inputPorts I2CMaster{..} = []
    inputPorts I2CSlave{..} = [slaveSCL]

    outputPorts I2CMaster{..} = [masterSCL]
    outputPorts I2CSlave{..} = []

    inoutPorts I2CMaster{..} = [masterSDA]
    inoutPorts I2CSlave{..} = [slaveSDA]

instance (VarValTime v x t) => TargetSystemComponent (I2C v x t) where
    moduleName _ _ = "pu_spi"
    hardware _tag _pu =
        Aggregate
            Nothing
            [ FromLibrary "i2c/bounce_filter.v"
            , FromLibrary "i2c/buffer.v"
            , FromLibrary "i2c/i2c_master_driver.v"
            , FromLibrary "i2c/i2c_to_nitta_splitter.v"
            , FromLibrary "i2c/nitta_to_i2c_splitter.v"
            , FromLibrary "i2c/pu_i2c_master_driver.v"
            , FromLibrary "i2c/pu_i2c_slave_driver.v"
            , FromLibrary "i2c/pu_master_i2c.v"
            , FromLibrary "i2c/pu_slave_i2c.v"
            ]
    software _ pu = Immediate "transport.txt" $ show pu
    hardwareInstance _ _ TargetEnvironment{unitEnv = NetworkEnv{}} _ports _io = error "wrong environment type, for pu_i2c it should be ProcessUnitEnv"
    hardwareInstance
        tag
        SimpleIO{bounceFilter}
        TargetEnvironment{unitEnv = ProcessUnitEnv{..}, signalClk, signalRst, signalCycleBegin, inputPort, outputPort, inoutPort}
        SimpleIOPorts{..}
        ioPorts =
            codeBlock
                [qc|
            { module_ ioPorts } #
                    ( .DATA_WIDTH( { dataWidth (def :: x) } )
                    , .ATTR_WIDTH( { attrWidth (def :: x) } )
                    , .BOUNCE_FILTER( { show bounceFilter } )
                    ) { tag }
                ( .clk( { signalClk } )
                , .rst( { signalRst } )
                , .flag_stop( { stop } )
                , .signal_cycle( { signalCycleBegin } )
                , .signal_oe( { signal oe } )
                , .signal_wr( { signal wr } )
                , .data_in( { dataIn } ), .attr_in( { attrIn } )
                , .data_out( { dataOut } ), .attr_out( { attrOut } )
                { extIO ioPorts }
                );
            |]
            where
                module_ I2CMaster{} = "pu_master_i2c"
                module_ I2CSlave{} = "pu_slave_i2c"
                extIO I2CMaster{..} =
                    codeBlock
                        [qc|
                    , .scl( { outputPort masterSCL } )
                    , .sda( { inoutPort masterSDA } )
                    |]
                extIO I2CSlave{..} =
                    codeBlock
                        [qc|
                    , .scl( { inputPort slaveSCL } )
                    , .sda( { inoutPort slaveSDA } )
                    |]
