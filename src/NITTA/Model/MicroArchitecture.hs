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
--
-- __Configure microarch with one network using auto IOPorts type pins selector:__
--
-- @
-- microarch = evalNetwork $ do
--     add "fram_tag" FramIO
--     add "accum_tag" AccumIO
--     add "div_tag" DividerIO
--     add "mul_tag" MultiplierIO
--     add "spi_tag" $ SPISlave
--         { slave_mosi = InputPortTag "mosi"
--         , slave_miso = OutputPortTag "miso"
--         , slave_sclk = InputPortTag "sclk"
--         , slave_cs   = InputPortTag "cs"
--         }
-- @
-- __Configure microarch with one network using auto String pins selector:__
--
-- @
-- microarch = evalNetwork $ do
--     addS "fram_tag" "fram"
--     addS "accum_tag" "accum"
--     addS "div_tag" "div"
--     addS "mul_tag" "mul"
--     addSIO "spi_tag" "spi" ["slave", "mosi", "miso", "sclk", "cs"]
-- @
--
--
-- __Configure microarch with manual pins:__
--
-- @
-- microarch = evalNetwork $ do
--     addManual "acum_tag" (PU def def AccumPorts{ init=SignalTag 18, load=SignalTag 19, neg=SignalTag 20, oe=SignalTag 21 } AccumIO )
--     addManual "mul_tag"  (PU def (multiplier True) MultiplierPorts{ wr=SignalTag 24, wrSel=SignalTag 25, oe=SignalTag 26 } MultiplierIO )
--     addManual "div_tag"  (PU def (divider 4 True) DividerPorts{ wr=SignalTag 27, wrSel=SignalTag 28, oe=SignalTag 29, oeSel=SignalTag 30 } DividerIO)
--     addManual "spi_tag"  (PU def
--         (anySPI 0)
--             SimpleIOPorts
--                 { wr=SignalTag 22, oe=SignalTag 23
--                 , stop="stop"
--                 }
--                 SPISlave
--                     { slave_mosi=InputPortTag "mosi"
--                     , slave_miso=OutputPortTag "miso"
--                     , slave_sclk=InputPortTag "sclk"
--                     , slave_cs=InputPortTag "cs"
--                     }
--             )
--
-- @
--
-- __Configure microarch with manual and auto pins:__
--
-- @
--
-- microarch = evalNetwork $ do
--     addManual "fram1_tag" (PU def def FramPorts{ oe=SignalTag 0, wr=SignalTag 1, addr=map SignalTag [2, 3, 4, 5] } FramIO )
--     addManual "fram2_tag" (PU def def FramPorts{ oe=SignalTag 6, wr=SignalTag 7, addr=map SignalTag [8, 9, 10, 11] } FramIO )
--     addManual "accum_tag" (PU def def AccumPorts{ init=SignalTag 18, load=SignalTag 19, neg=SignalTag 20, oe=SignalTag 21 } AccumIO )
--     add "div_tag2" DividerIO
--     add "mul_tag2" MultiplierIO
--     addS "div_tag" "div"
--     addS "mul_tag" "mul"
--     addSIO "spi_tag" "spi" ["slave", "mosi", "miso", "sclk", "cs"]
--
-- @
evalNetwork net ioSync = let
        net' = net >> busNetworkS ioSync <$> get
    in evalState net' ([], [])

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
--
-- @
-- add "fram_tag" FramIO
-- @
--
-- @
-- add "accum_tag" AccumIO
-- @
--
-- @
-- add "spi_tag" $ SPISlave
--              { slave_mosi = InputPortTag "mosi"
--              , slave_miso = OutputPortTag "miso"
--              , slave_sclk = InputPortTag "sclk"
--              , slave_cs   = InputPortTag "cs"
--              }
-- @
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
--
-- @
-- addS "fram_tag" "fram"
-- @
--
-- @
-- addS "accum_tag" "accum"
-- @
addS tag "fram"  = add tag FramIO
addS tag "accum" = add tag AccumIO
addS tag "div"   = add tag DividerIO
addS tag "mul"   = add tag MultiplierIO
addS _ _         = error "Can't match type PU with existing PU types"

-- |__Add SimpleIO PU automatic with String data type__
--
-- @
-- addSIO "spi_tag" "spi" ["slave", "mosi", "miso", "sclk", "cs"]
-- @
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
--
-- @
-- addManual "fram_tag" (PU def def FramPorts{ oe=SignalTag 0, wr=SignalTag 1, addr=map SignalTag [2, 3, 4, 5] } FramIO )
-- @
--
-- @
-- addManual "accum_tag" (PU def def AccumPorts{ init=SignalTag 18, load=SignalTag 19, neg=SignalTag 20, oe=SignalTag 21 } AccumIO )
-- @
addManual tag mkPU = do
    (usedPorts, pus) <- get
    let pu         = mkPU $ puEnv tag
        puPorts    = (\PU { ports } -> portsToSignals ports) pu
        usedPorts' = usedPorts ++ intersPortsError puPorts usedPorts tag
    put (usedPorts', (tag, mkPU) : pus)
