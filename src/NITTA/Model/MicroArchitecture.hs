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
    , addCustom
    , addS
    , addSIO
    , addManual
    , evalNetwork
    , example
    ) where

import           Control.Monad.State.Lazy
import           Data.Default                     (def)
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Networks.Types
import           NITTA.Model.ProcessorUnits
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Project.Implementation

-- |__Eval state and create microarch__
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
add tag = addCustom tag def

-- |__Add PU automatic, using custom pu__
addCustom tag pu io = do
    (usedPorts, puBlocks) <- get
    let signals = freePins usedPorts
        ports = signalsToPorts signals pu
        puBlock = (tag, PU def pu ports io)
        used = portsToSignals ports
        puBlocks'= puBlock : puBlocks
        usedPorts' = usedPorts ++ used
    put (usedPorts', puBlocks')

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

-- |__Example for architecture configuration__
example ioSync = evalNetwork ioSync $ do
    addManual "fram1_tag" (PU def def FramPorts{ oe=SignalTag 0, wr=SignalTag 1, addr=map SignalTag [2, 3, 4, 5] } FramIO )
    addManual "accum_tag" (PU def def AccumPorts{ init=SignalTag 18, load=SignalTag 19, neg=SignalTag 20, oe=SignalTag 21 } AccumIO )
    addCustom "fram2_tag" (memoryWidth 32) FramIO
    add "div_tag2" DividerIO
    add "mul_tag2" MultiplierIO
    addS "div_tag" "div"
    addS "mul_tag" "mul"
    addSIO "spi_slave" "spi" ["slave", "mosi", "miso", "sclk", "cs"]
    addManual "spi_master"  (PU def
        (anySPI 0)
            SimpleIOPorts
                { wr=SignalTag 22, oe=SignalTag 23
                , stop="stop"
                }
                SPIMaster
                    { master_mosi=OutputPortTag "mosi"
                    , master_miso=InputPortTag "miso"
                    , master_sclk=OutputPortTag "sclk"
                    , master_cs=OutputPortTag "cs"
                    }
            )
