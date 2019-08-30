{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module NITTA.Conf (addFram, addAccum, addMul, addDiv, addSpi, addManual, evalNetwork, evalNetworkM, endManual, end ) where

import           Control.Monad.State.Lazy
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Networks.Types
import           NITTA.Model.ProcessorUnits
import           NITTA.Model.ProcessorUnits.Types
import           Data.Default                     (def)

divPortConst = 4 
anySpiConst = 0 

-- manual add PU
addManual el = get >>= \lst -> put $ lst ++ [el]

-- manual create BusNetwork with width parameter counter
endManual counter ioSync = busNetwork counter ioSync <$> get  

-- eval state and create microarch auto
evalNetwork net = evalState net (0, [])  

-- eval state and create microarch manual
evalNetworkM net = evalState net (0, [])  

-- add fram auto
addFram name = do
             networkState <- get
             let (i, pu) = networkState
                 puListOut = pu ++ [(name, PU def def FramPorts{ oe=SignalTag (i+5), wr=SignalTag (i+4), addr=map SignalTag [i+3, i+2, i+1, i] } FramIO)]
                 counterOut = i + 6
             put (counterOut, puListOut)

-- add accum auto 
addAccum name = do
             networkState <- get
             let (i, pu) = networkState
                 puListOut = pu ++ [(name, PU def def AccumPorts{ init=SignalTag i, load=SignalTag (i+1), neg=SignalTag (i+2), oe=SignalTag (i+3) } AccumIO )]
                 counterOut = i + 4 
             put (counterOut, puListOut)

-- add mul auto
addMul name = do
             networkState <- get
             let (i, pu) = networkState
                 puListOut = pu ++ [(name, PU def (multiplier True) MultiplierPorts{ wr=SignalTag i, wrSel=SignalTag (i+1), oe=SignalTag (i+2) } MultiplierIO )]
                 counterOut = i + 3 
             put (counterOut, puListOut)

-- add div auto
addDiv name = do
             networkState <- get
             let (i, pu) = networkState
                 puListOut = pu ++ [(name, PU def (divider divPortConst True) DividerPorts{ wr=SignalTag i, wrSel=SignalTag (i+1), oe=SignalTag (i+2), oeSel=SignalTag (i+3) } DividerIO )]
                 counterOut = i + 4 
             put (counterOut, puListOut)

-- add spi auto
addSpi name pos = do
             networkState <- get
             let (i, pu) = networkState
                 puListOut = pu ++ [(name, PU def
                    (anySPI anySpiConst)
                    SimpleIOPorts
                        { wr=SignalTag i, oe=SignalTag (i+1) 
                        , stop="stop"
                        }
                    $ case pos of
                        "slave" -> SPISlave
                            { slave_mosi=InputPortTag "mosi"
                            , slave_miso=OutputPortTag "miso"
                            , slave_sclk=InputPortTag "sclk"
                            , slave_cs=InputPortTag "cs"
                            }
                        "master" -> SPIMaster
                            { master_mosi=OutputPortTag "mosi"
                            , master_miso=InputPortTag "miso"
                            , master_sclk=OutputPortTag "sclk"
                            , master_cs=OutputPortTag "cs"
                            }
                        _        -> error "error while configure SPI! Set 'master' or 'slave'"
                    )]
                 counterOut = i + 2 
             put (counterOut, puListOut)

-- end ioSync 
end ioSync = busNetworkS ioSync <$> get 
           
busNetworkS ioSync (counter, pu) = busNetwork counter ioSync pu          
