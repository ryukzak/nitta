{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module NITTA.Conf
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

-- | Eval state and create microarch 
--
-- __Configure microarch with one network using auto pins selector:__
--
-- @
-- microarch = evalNetwork $ do
--         addFram \"fram\"
--         addAccum \"accum\"
--         addDiv \"div\"
--         addMul \"mul\"
--         addSPI \"spi\" \"slave\" \"mosi\" \"miso\" \"sclk\" \"cs\"
-- @
--
-- __Configure microarch with manual pins:__
--
-- @
-- microarch = evalNetwork $ do
--         addFram \"fram1\"
--         addFram \"fram2\"
--         addManual \"acum\" (PU def def AccumPorts{ init=SignalTag 18, load=SignalTag 19, neg=SignalTag 20, oe=SignalTag 21 } AccumIO )
--         addManual \"mul\"  (PU def (multiplier True) MultiplierPorts{ wr=SignalTag 24, wrSel=SignalTag 25, oe=SignalTag 26 } MultiplierIO )
--         addManual \"div\"  (PU def (divider 4 True) DividerPorts{ wr=SignalTag 27, wrSel=SignalTag 28, oe=SignalTag 29, oeSel=SignalTag 30 } DividerIO)
--         addManual \"spi\"  (PU def
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
--         addManual \"fram1\" (PU def def FramPorts{ oe=SignalTag 0, wr=SignalTag 1, addr=map SignalTag [2, 3, 4, 5] } FramIO )
--         addManual \"fram2\" (PU def def FramPorts{ oe=SignalTag 6, wr=SignalTag 7, addr=map SignalTag [8, 9, 10, 11] } FramIO )
--         addManual \"acum\" (PU def def AccumPorts{ init=SignalTag 18, load=SignalTag 19, neg=SignalTag 20, oe=SignalTag 21 } AccumIO )
--         addManual \"mul\"  (PU def (multiplier True) MultiplierPorts{ wr=SignalTag 24, wrSel=SignalTag 25, oe=SignalTag 26 } MultiplierIO )
--         addManual \"div\"  (PU def (divider 4 True) DividerPorts{ wr=SignalTag 27, wrSel=SignalTag 28, oe=SignalTag 29, oeSel=SignalTag 30 } DividerIO)
--         addSPI \"spi\" \"slave\" \"mosi\" \"miso\" \"sclk\" \"cs\"
--       
-- @
--
evalNetwork net ioSync =
  let net' = net >> busNetworkS ioSync <$> get in evalState net' ([], [])

-- | Check intersections in ports nums
intersPortsError ports usedPorts tag
  | any (`elem` usedPorts) ports
  = error $ "intersection in " ++ tag ++ " ports with used ports"
  | otherwise
  = ports

-- | __Add manual PU__
--
-- @
-- addManual \"fram\" (PU def def FramPorts{ oe=SignalTag 0, wr=SignalTag 1, addr=map SignalTag [2, 3, 4, 5] } FramIO )
-- @
--
-- @
-- addManual \"accum\" (PU def def AccumPorts{ init=SignalTag 18, load=SignalTag 19, neg=SignalTag 20, oe=SignalTag 21 } AccumIO )
-- @

addManual tag mkPU = do
  (usedPorts, pus) <- get
  let pu         = mkPU $ puEnv tag
      ports      = (\PU { ports } -> getSignalList ports) pu
      usedPorts' = usedPorts ++ intersPortsError ports usedPorts tag
  put (usedPorts', (tag, mkPU) : pus)


-- | Create environment for PU
puEnv tag = bnEnv
  { unitEnv = ProcessUnitEnv
                { parameterAttrWidth = InlineParam "ATTR_WIDTH"
                , dataIn             = "data_bus"
                , dataOut            = tag ++ "_data_out"
                , attrIn             = "attr_bus"
                , attrOut            = tag ++ "_attr_out"
                , signal = \(SignalTag i) -> "control_bus[" ++ show i ++ "]"
                }
  }
 where
  bnEnv = TargetEnvironment { signalClk   = "clk"
                            , signalRst   = "rst"
                            , signalCycle = "cycle"
                            , inputPort   = \(InputPortTag n) -> n
                            , outputPort  = \(OutputPortTag n) -> n
                            , inoutPort   = \(InoutPortTag n) -> n
                            , unitEnv     = NetworkEnv
                            }

-- | Get free pins from infinity list of nums
freePins used count = take count $ filter (not . (`elem` used)) [0 ..]

-- | __Add fram processor unit with automatic pin selector__
--
-- @
-- addFram \"fram\"
-- @
--
addFram tag = do
  (usedPorts, pus) <- get
  let
    pins = freePins usedPorts 6
    pusOut =
      pus
        ++ [ ( tag
             , PU
               def
               def
               FramPorts
                 { oe   = SignalTag (pins !! 5)
                 , wr   = SignalTag (pins !! 4)
                 , addr = map SignalTag $ take 4 pins 
                 }
               FramIO
             )
           ]
    usedPortsOut = usedPorts ++ pins
  put (usedPortsOut, pusOut)

-- | __Add accumulator processor unit with automatic pin selector__
--
-- @
-- addAccum \"accum\"
-- @
addAccum tag = do
  (usedPorts, pus) <- get
  let pins = freePins usedPorts 4
      pusOut =
        pus
          ++ [ ( tag
               , PU
                 def
                 def
                 AccumPorts { init = SignalTag (pins !! 0)
                            , load = SignalTag (pins !! 1)
                            , neg  = SignalTag (pins !! 2)
                            , oe   = SignalTag (pins !! 3)
                            }
                 AccumIO
               )
             ]
      usedPortsOut = usedPorts ++ pins
  put (usedPortsOut, pusOut)

-- | __Add multiplier processor unit with automatic pin selector__
--
-- @
-- addMul \"mul\"
-- @
addMul tag = do
  (usedPorts, pus) <- get
  let pins = freePins usedPorts 3
      pusOut =
        pus
          ++ [ ( tag
               , PU
                 def
                 (multiplier True)
                 MultiplierPorts { wr    = SignalTag (pins !! 0)
                                 , wrSel = SignalTag (pins !! 1)
                                 , oe    = SignalTag (pins !! 2)
                                 }
                 MultiplierIO
               )
             ]
      usedPortsOut = usedPorts ++ pins
  put (usedPortsOut, pusOut)

-- | __Add divider processor unit with automatic pin selector__
--
-- @
-- addDiv \"div\"
-- @
addDiv tag = do
  (usedPorts, pus) <- get
  let pins = freePins usedPorts 4
      pusOut =
        pus
          ++ [ ( tag
               , PU
                 def
                 (divider divPortConst True)
                 DividerPorts { wr    = SignalTag (pins !! 0)
                              , wrSel = SignalTag (pins !! 1)
                              , oe    = SignalTag (pins !! 2)
                              , oeSel = SignalTag (pins !! 3)
                              }
                 DividerIO
               )
             ]
      usedPortsOut = usedPorts ++ pins
  put (usedPortsOut, pusOut)
--
-- | __Add SPI processor unit with automatic pin selector__
--
-- @
-- addSPI \"spi\" \"slave\" \"mosi\" \"miso\" \"sclk\" \"cs\"
-- @
--
-- @
-- addSPI \"spi\" \"master\" \"mosi\" \"miso\" \"sclk\" \"cs\"
-- @
--
addSPI tag pos mosi miso sclk cs = do
  (usedPorts, pus) <- get
  let
    pins = freePins usedPorts 2
    pusOut =
      pus
        ++ [ ( tag
             , PU
                 def
                 (anySPI anySpiConst)
                 SimpleIOPorts { wr   = SignalTag (pins !! 0)
                               , oe   = SignalTag (pins !! 1)
                               , stop = "stop"
                               }
               $ case pos of
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
                   _ -> error
                     "error while configure SPI! Set 'master' or 'slave'"
             )
           ]
    usedPortsOut = usedPorts ++ pins
  put (usedPortsOut, pusOut)

-- | Special version of busNetwork function ( for easier get from State )
busNetworkS ioSync (lstPorts, pu) = busNetwork (length lstPorts) ioSync pu

add' tag proxy io f2 = do
  (usedPorts, pus) <- get
  let
    pins = freePins usedPorts 6
    pusOut =
      pus
        ++ [ ( tag
             , PU
               def
               f2 
               (getPorts proxy pins)
               io
             )
           ]
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

