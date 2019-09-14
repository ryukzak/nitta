{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS -fno-warn-overlapping-patterns -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-cse #-}

{-|
Module      : NITTA.Model.MicroArchitecture
Description : NITTA CAD executable
Copyright   : (c) Daniil Prohorov, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.Model.MicroArchitecture
  ( add 
  , addIO
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
import           Data.Proxy



divPortConst = 4
anySpiConst = 0

-- |Eval state and create microarch 
--
-- __Configure microarch with one network using auto pins selector:__
--
-- @
-- microarch = evalNetwork $ do
--         add "fram" "fram"
--         add "accum" "accum"
--         add "div" "div"
--         add "mul" "mul"
--         add "spi" "spi" "slave" "mosi" "miso" "sclk" "cs"
-- @
--
-- __Configure microarch with manual pins:__
--
-- @
-- microarch = evalNetwork $ do
--         addManual "acum" (PU def def AccumPorts{ init=SignalTag 18, load=SignalTag 19, neg=SignalTag 20, oe=SignalTag 21 } AccumIO )
--         addManual "mul"  (PU def (multiplier True) MultiplierPorts{ wr=SignalTag 24, wrSel=SignalTag 25, oe=SignalTag 26 } MultiplierIO )
--         addManual "div"  (PU def (divider 4 True) DividerPorts{ wr=SignalTag 27, wrSel=SignalTag 28, oe=SignalTag 29, oeSel=SignalTag 30 } DividerIO)
--         addManual "spi"  (PU def
--             (anySPI 0)
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
--         addManual "fram1" (PU def def FramPorts{ oe=SignalTag 0, wr=SignalTag 1, addr=map SignalTag [2, 3, 4, 5] } FramIO )
--         addManual "fram2" (PU def def FramPorts{ oe=SignalTag 6, wr=SignalTag 7, addr=map SignalTag [8, 9, 10, 11] } FramIO )
--         addManual "accum" (PU def def AccumPorts{ init=SignalTag 18, load=SignalTag 19, neg=SignalTag 20, oe=SignalTag 21 } AccumIO )
--         add "div" "div"
--         add "mul" "mul"
--         add "spi" "spi" "slave" "mosi" "miso" "sclk" "cs"
--       
-- @
--
evalNetwork net ioSync =
    let 
        net' = net >> busNetworkS ioSync <$> get 
    in 
        evalState net' ([], [])

-- |Check intersections in ports nums
intersPortsError ports usedPorts tag
    | any (`elem` usedPorts) ports
    = error $ "intersection in " ++ tag ++ " ports with used ports"
    | otherwise
    = ports

-- |__Add manual PU__
--
-- @
-- addManual "fram" (PU def def FramPorts{ oe=SignalTag 0, wr=SignalTag 1, addr=map SignalTag [2, 3, 4, 5] } FramIO )
-- @
--
-- @
-- addManual "accum" (PU def def AccumPorts{ init=SignalTag 18, load=SignalTag 19, neg=SignalTag 20, oe=SignalTag 21 } AccumIO )
-- @

addManual tag mkPU = do
    (usedPorts, pus) <- get
    let pu         = mkPU $ puEnv tag
        puPorts    = (\PU { ports } -> portsToSignals ports) pu
        usedPorts' = usedPorts ++ intersPortsError puPorts usedPorts tag
    put (usedPorts', (tag, mkPU) : pus)


-- |Create environment for PU
puEnv tag =  
    let
        bnEnv =
            TargetEnvironment 
                { signalClk   = "clk"
                , signalRst   = "rst"
                , signalCycle = "cycle"
                , inputPort   = inputPortTag
                , outputPort  = outputPortTag
                , inoutPort   = inoutPortTag
                , unitEnv     = NetworkEnv
                }
    in
        bnEnv 
            { unitEnv = 
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
freePins used count = 
    let 
        freeSignals = filter (not . (`elem` used)) infSignals
        infSignals = map SignalTag [0 ..]
    in
        take count freeSignals 

-- |Special version of busNetwork function ( for easier get from State )
busNetworkS ioSync (lstPorts, pu) = busNetwork (length lstPorts) ioSync pu

add' tag proxy io f2 = do
    (usedPorts, pus) <- get
    let
        pins = freePins usedPorts 6
        ports = signalsToPorts proxy pins
        pusOut =
            pus ++ [( tag, PU def f2 ports io)]

        usedPortsOut = usedPorts ++ pins
    put (usedPortsOut, pusOut)



add "fram" tag 
  = add' tag proxy io def
    where 
        proxy = Proxy :: Proxy (Fram v x t)
        io    = FramIO

add "accum" tag 
  = add' tag proxy io def
    where
        proxy = Proxy :: Proxy (Accum v x t)
        io    = AccumIO

add "div" tag 
  = add' tag proxy io (divider divPortConst True) 
    where
        proxy = Proxy :: Proxy (Divider v x t)
        io    = DividerIO

add "mul" tag 
  = add' tag proxy io (multiplier True)
    where
        proxy = Proxy :: Proxy (Multiplier v x t)
        io    = MultiplierIO

add _ _ = error "Can't match type PU with existing PU types"


addIO "spi" tag args     
  = add' tag proxy io (anySPI anySpiConst) 
    where
        pos   = head args
        mosi  = args !! 1
        miso  = args !! 2
        sclk  = args !! 3
        cs    = args !! 4
        proxy = Proxy :: Proxy (simpleIO v x t)
        io    = case pos of
                   "slave" -> SPISlave { slave_mosi = InputPortTag mosi
                                       , slave_miso = OutputPortTag miso
                                       , slave_sclk = InputPortTag sclk
                                       , slave_cs   = InputPortTag cs
                                       }
                   "master" -> SPIMaster { master_mosi = OutputPortTag mosi
                                         , master_miso = InputPortTag miso
                                         , master_sclk = OutputPortTag sclk
                                         , master_cs   = OutputPortTag cs
                                         }
                   _        -> error "Error while configure SPI! Set 'master' or 'slave'"

addIO _ _ _ = error "Can't match type PU with existing PU types"

