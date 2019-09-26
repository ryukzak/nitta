{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-cse #-}

{-|
Module      : NITTA.Model.MicroArchitecture
Description : Create micro architecture functions
Copyright   : (c) Daniil Prohorov, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.Model.MicroArchitecture
    ( add
    , addS
    , addSIO
    , addManual
    , evalNetwork
    ) where

import           Control.Monad.State.Lazy
import           Data.Default                     (def)
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Networks.Types
import           NITTA.Model.ProcessorUnits
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Project.Implementation

-- |Eval state and create microarch

evalNetwork ioSync net = flip evalState ([], []) $ do 
    _ <- net
    busNetworkS ioSync <$> get


-- |Check intersections in ports numbers
intersPortsError ports usedPorts tag
    | any (`elem` usedPorts) ports = error $ "intersection in " ++ tag ++ " ports with used ports"
    | otherwise                    = ports

-- |Create environment for PU
puEnv tag = 
    TargetEnvironment
        { signalClk   = "clk"
        , signalRst   = "rst"
        , signalCycle = "cycle"
        , inputPort   = inputPortTag
        , outputPort  = outputPortTag
        , inoutPort   = inoutPortTag
        , unitEnv     = 
            ProcessUnitEnv
                { parameterAttrWidth = InlineParam "ATTR_WIDTH"
                , dataIn             = "data_bus"
                , dataOut            = tag ++ "_data_out"
                , attrIn             = "attr_bus"
                , attrOut            = tag ++ "_attr_out"
                , signal = \(SignalTag i) -> "control_bus[" ++ show i ++ "]"
                }
        }

-- |Get free pins from infinity list of nums
freePins used = let
        infSignals = map SignalTag [0 ..]
        freeSignals = filter (not . (`elem` used)) infSignals
    in freeSignals

-- |Special version of busNetwork function ( for easier get from State )
busNetworkS ioSync (lstPorts, pu) = busNetwork (length lstPorts) ioSync pu

-- |__Add PU automatic__
add tag io = do
    (usedPorts, pus) <- get
    let signals = freePins usedPorts
        ports = signalsToPorts signals
        pu = (tag, PU def def ports io)
        used = portsToSignals ports
        pus'= pu : pus
        usedPorts' = usedPorts ++ used
    put (usedPorts', pus')

-- |__Add PU automatic with String data type__
addS tag "fram"  = add tag FramIO
addS tag "shift" = add tag ShiftIO
addS tag "accum" = add tag AccumIO
addS tag "div"   = add tag DividerIO
addS tag "mul"   = add tag MultiplierIO
addS _ _         = error "Can't match type PU with existing PU types"

-- |__Add SimpleIO PU automatic with String data type__
addSIO tag "spi" [mode, mosi, miso, sclk, cs] = add tag $ 
        case mode of
            "slave" -> SPISlave
                { slave_mosi = InputPortTag mosi
                , slave_miso = OutputPortTag miso
                , slave_sclk = InputPortTag sclk
                , slave_cs   = InputPortTag cs
                }
            "master" -> SPIMaster
                { master_mosi = OutputPortTag mosi
                , master_miso = InputPortTag miso
                , master_sclk = OutputPortTag sclk
                , master_cs   = OutputPortTag cs
                }
            _        -> error "Error while configure SPI! Set 'master' or 'slave'"

addSIO _ _ _ = error "Error while configure SimpleIO uncorrect parameters"

-- |__Add manual PU__
addManual tag mkPU = do
    (usedPorts, pus) <- get
    let pu         = mkPU $ puEnv tag
        puPorts    = (\PU { ports } -> portsToSignals ports) pu
        usedPorts' = usedPorts ++ intersPortsError puPorts usedPorts tag
    put (usedPorts', (tag, mkPU) : pus)
