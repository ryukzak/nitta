{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}

{-|
Module      : NITTA.Test.Microarchitectures
Description :
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Test.Microarchitectures
    ( march
    , marchSPI
    , marchSPIDropData
    , proxyInt
    , proxyIntX32
    , algTestCase
    , externalTestCntr
    , runTargetSynthesis'
    ) where

import           Control.Monad                 (void)
import           Data.Atomics.Counter          (incrCounter, newCounter)
import           Data.Default
import           Data.Proxy
import           NITTA.BusNetwork
import           NITTA.Model
import qualified NITTA.ProcessUnits.Accum      as A
import qualified NITTA.ProcessUnits.Divider    as D
import qualified NITTA.ProcessUnits.Fram       as FR
import qualified NITTA.ProcessUnits.Multiplier as M
import qualified NITTA.ProcessUnits.Shift      as S
import qualified NITTA.ProcessUnits.SPI        as SPI
import           NITTA.TargetSynthesis
import           NITTA.Types
import           System.IO.Unsafe              (unsafePerformIO)
import           Test.Tasty.HUnit


proxyInt = Proxy :: Proxy Int
proxyIntX32 = Proxy :: Proxy (IntX 32)


march :: BusNetwork String String Int Int
march = busNetwork 31 (Just True)
    [ ("fram1", PU def def FR.Ports{ FR.oe=SignalTag 0, FR.wr=SignalTag 1, FR.addr=map SignalTag [2, 3, 4, 5] } )
    , ("fram2", PU def def FR.Ports{ FR.oe=SignalTag 6, FR.wr=SignalTag 7, FR.addr=map SignalTag [8, 9, 10, 11] } )
    , ("shift", PU def def S.Ports{ S.work=SignalTag 12, S.direction=SignalTag 13, S.mode=SignalTag 14, S.step=SignalTag 15, S.init=SignalTag 16, S.oe=SignalTag 17 })
    , ("accum", PU def def A.Ports{ A.init=SignalTag 18, A.load=SignalTag 19, A.neg=SignalTag 20, A.oe=SignalTag 21 } )
    , ("mul", PU def (M.multiplier True) M.Ports{ M.wr=SignalTag 22, M.wrSel=SignalTag 23, M.oe=SignalTag 24 } )
    , ("div", PU def (D.divider 8 True) D.Ports{ D.wr=SignalTag 25, D.wrSel=SignalTag 26, D.oe=SignalTag 27, D.oeSel=SignalTag 28 } )
    ]


marchSPI ::
    ( Integral x, Val x
    ) => Proxy x -> BusNetwork String String x Int
marchSPI _proxy = busNetwork 31 (Just False)
    [ ("fram1", PU def def FR.Ports{ FR.oe=SignalTag 11, FR.wr=SignalTag 10, FR.addr=map SignalTag [9, 8, 7, 6] } )
    , ("fram2", PU def def FR.Ports{ FR.oe=SignalTag 5, FR.wr=SignalTag 4, FR.addr=map SignalTag [3, 2, 1, 0] } )
    , ("shift", PU def def S.Ports{ S.work=SignalTag 12, S.direction=SignalTag 13, S.mode=SignalTag 14, S.step=SignalTag 15, S.init=SignalTag 16, S.oe=SignalTag 17 })
    , ("accum", PU def def A.Ports{ A.init=SignalTag 18, A.load=SignalTag 19, A.neg=SignalTag 20, A.oe=SignalTag 21 } )
    , ("spi", PU def (SPI.slaveSPI 0) SPI.Ports
        { SPI.wr=SignalTag 22, SPI.oe=SignalTag 23
        , SPI.stop="stop"
        , SPI.externalPorts=SPI.Slave
            { SPI.slave_mosi=InputPortTag "mosi"
            , SPI.slave_miso=OutputPortTag "miso"
            , SPI.slave_sclk=InputPortTag "sclk"
            , SPI.slave_cs=InputPortTag "cs"
            }
        })
    , ("div", PU def (D.divider 8 True) D.Ports{ D.wr=SignalTag 24, D.wrSel=SignalTag 25, D.oe=SignalTag 26, D.oeSel=SignalTag 27 } )
    , ("mul", PU def (M.multiplier True) M.Ports{ M.wr=SignalTag 28, M.wrSel=SignalTag 29, M.oe=SignalTag 30 } )
    ]


marchSPIDropData proxy = (marchSPI proxy){ bnAllowDrop=Just True }


-----------------------------------------------------------


-- |Dirty hack to avoid collision with parallel QuickCheck.
externalTestCntr = unsafePerformIO $ newCounter 0
{-# NOINLINE externalTestCntr #-}

runTargetSynthesis' t@TargetSynthesis{ tName } = do
    i <- incrCounter 1 externalTestCntr
    runTargetSynthesis t{ tName=tName ++ "_" ++ show i }

algTestCase n tMicroArch alg
    = testCase n $ void $ runTargetSynthesis' (def :: TargetSynthesis _ _ _ Int)
        { tName=n
        , tMicroArch
        , tDFG=fsToDataFlowGraph alg
        }
