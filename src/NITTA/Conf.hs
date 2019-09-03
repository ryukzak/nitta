{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module NITTA.Conf (addManual, addFram, addAccum, addMul, addDiv, addSPI, evalNetwork) where

import           Control.Monad.State.Lazy
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Networks.Types
import           NITTA.Project.Implementation
import           NITTA.Model.ProcessorUnits
import           NITTA.Model.ProcessorUnits.Types
import           Data.Default                     (def)

divPortConst = 4 
anySpiConst = 0 

-- eval state and create microarch
evalNetwork net ioSync = 
    let 
        net' = net >> busNetworkS ioSync <$> get
    in
        evalState (net') ([], [])  

-- check intersections in ports nums
intersPortsError ports usedPorts tag
  | any (\x -> elem x usedPorts) ports 
  = error $ "intersection in " ++ tag ++ " ports with used ports"
  | otherwise = ports 

-- add manual PU
addManual tag mkPU = do
    (usedPorts, pus) <- get
    let
        pu = mkPU $ puEnv tag
        ports = (\PU{ ports } -> getSignalList ports) pu
        usedPorts' = usedPorts ++ intersPortsError ports usedPorts tag
    put (usedPorts', (tag, mkPU) : pus)


-- create environment for PU
puEnv tag = bnEnv
    { unitEnv=ProcessUnitEnv
        { parameterAttrWidth=InlineParam "ATTR_WIDTH"
        , dataIn="data_bus"
        , dataOut=tag ++ "_data_out"
        , attrIn="attr_bus"
        , attrOut=tag ++ "_attr_out"
        , signal= \(SignalTag i) -> "control_bus[" ++ show i ++ "]"
        }
    }
      where
          bnEnv = TargetEnvironment
              { signalClk="clk"
              , signalRst="rst"
              , signalCycle="cycle"
              , inputPort= \(InputPortTag n) -> n
              , outputPort= \(OutputPortTag n) -> n
              , inoutPort= \(InoutPortTag n) -> n
              , unitEnv=NetworkEnv
              }

-- get free pins from infinity list of nums
freePins used count = take count $ filter (not . \x -> elem x used) [0..]

-- add fram auto
addFram tag = do
             (usedPorts, pus) <- get 
             let 
                 pins = freePins usedPorts 6 
                 pusOut = pus ++ [(tag, PU def def FramPorts{ oe=SignalTag (pins !! 5), wr=SignalTag (pins !! 4), addr=map SignalTag [pins !! 3, pins !! 2, pins !! 1, pins !! 0] } FramIO)]
                 usedPortsOut = usedPorts ++ pins 
             put (usedPortsOut, pusOut)

-- add accum auto 
addAccum tag = do
             (usedPorts, pus) <- get 
             let 
                 pins = freePins usedPorts 4 
                 pusOut = pus ++ [(tag, PU def def AccumPorts{ init=SignalTag (pins !! 0), load=SignalTag (pins !! 1), neg=SignalTag (pins !! 2), oe=SignalTag (pins !! 3) } AccumIO )]
                 usedPortsOut = usedPorts ++ pins 
             put (usedPortsOut, pusOut)

-- add mul auto
addMul tag = do
             (usedPorts, pus) <- get 
             let                  
                 pins = freePins usedPorts 3 
                 pusOut = pus ++ [(tag, PU def (multiplier True) MultiplierPorts{ wr=SignalTag (pins !! 0), wrSel=SignalTag (pins !! 1), oe=SignalTag (pins !! 2)} MultiplierIO )]
                 usedPortsOut = usedPorts ++ pins 
             put (usedPortsOut, pusOut)

-- add div auto
addDiv tag = do
             (usedPorts, pus) <- get 
             let 
                 pins = freePins usedPorts 4 
                 pusOut = pus ++ [(tag, PU def (divider divPortConst True) DividerPorts{ wr=SignalTag (pins !! 0), wrSel=SignalTag (pins !! 1), oe=SignalTag (pins !! 2), oeSel=SignalTag (pins !! 3) } DividerIO )]
                 usedPortsOut = usedPorts ++ pins 
             put (usedPortsOut, pusOut)
--
-- add SPI auto
addSPI tag pos mosi miso sclk cs = do
             (usedPorts, pus) <- get 
             let 
                 pins = freePins usedPorts 2 
                 pusOut = pus ++ [(tag, PU def
                    (anySPI anySpiConst)
                    SimpleIOPorts
                        { wr=SignalTag (pins !! 0), oe=SignalTag (pins !! 1)
                        , stop="stop"
                        }
                    $ case pos of
                        "slave" -> SPISlave
                            { slave_mosi=InputPortTag mosi
                            , slave_miso=OutputPortTag miso
                            , slave_sclk=InputPortTag sclk
                            , slave_cs=InputPortTag cs
                            }
                        "master" -> SPIMaster
                            { master_mosi=OutputPortTag mosi
                            , master_miso=InputPortTag miso
                            , master_sclk=OutputPortTag sclk
                            , master_cs=OutputPortTag cs
                            }
                        _        -> error "error while configure SPI! Set 'master' or 'slave'"
                    )]
                 usedPortsOut = usedPorts ++ pins 
             put (usedPortsOut, pusOut)

-- special version of busNetwork function ( for easier get from State )
busNetworkS ioSync (lstPorts, pu) = busNetwork (length lstPorts) ioSync pu          

