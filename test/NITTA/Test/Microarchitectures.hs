{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -Wredundant-constraints -fno-warn-missing-signatures #-}

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
    , algTestCaseWithInput
    , externalTestCntr
    ) where

import           Data.Atomics.Counter          (incrCounter, newCounter)
import           Data.Bits
import           Data.Default
import           Data.Either                   (isRight)
import           Data.Proxy
import           NITTA.BusNetwork
import qualified NITTA.ProcessUnits.Accum      as A
import qualified NITTA.ProcessUnits.Divider    as D
import qualified NITTA.ProcessUnits.Fram       as FR
import qualified NITTA.ProcessUnits.Multiplier as M
import qualified NITTA.ProcessUnits.Shift      as S
import qualified NITTA.ProcessUnits.SPI        as SPI
import           NITTA.Types
import           NITTA.Utils.Test
import           System.IO.Unsafe              (unsafePerformIO)
import           Test.Tasty.HUnit


proxyInt = Proxy :: Proxy Int
proxyIntX32 = Proxy :: Proxy (IntX 32)


march :: BusNetwork String String Int Int
march = busNetwork 31 (Just True) [] []
    [ ("fram1", PU def FR.PUPorts{ FR.oe=Signal 0, FR.wr=Signal 1, FR.addr=map Signal [2, 3, 4, 5] } )
    , ("fram2", PU def FR.PUPorts{ FR.oe=Signal 6, FR.wr=Signal 7, FR.addr=map Signal [8, 9, 10, 11] } )
    , ("shift", PU def S.PUPorts{ S.work=Signal 12, S.direction=Signal 13, S.mode=Signal 14, S.step=Signal 15, S.init=Signal 16, S.oe=Signal 17 })
    , ("accum", PU def A.PUPorts{ A.init=Signal 18, A.load=Signal 19, A.neg=Signal 20, A.oe=Signal 21 } )
    , ("mul", PU (M.multiplier True) M.PUPorts{ M.wr=Signal 22, M.wrSel=Signal 23, M.oe=Signal 24 } )
    , ("div", PU (D.divider 8 True) D.PUPorts{ D.wr=Signal 25, D.wrSel=Signal 26, D.oe=Signal 27, D.oeSel=Signal 28 } )
    ]


marchSPI ::
    ( Integral x, Bits x, Default x, Show x, Val x
    ) => Proxy x -> BusNetwork String String x Int
marchSPI _proxy = busNetwork 31 (Just False)
    [ InputPort "mosi", InputPort "sclk", InputPort "cs" ]
    [ OutputPort "miso" ]
    [ ("fram1", PU def FR.PUPorts{ FR.oe=Signal 11, FR.wr=Signal 10, FR.addr=map Signal [9, 8, 7, 6] } )
    , ("fram2", PU def FR.PUPorts{ FR.oe=Signal 5, FR.wr=Signal 4, FR.addr=map Signal [3, 2, 1, 0] } )
    , ("shift", PU def S.PUPorts{ S.work=Signal 12, S.direction=Signal 13, S.mode=Signal 14, S.step=Signal 15, S.init=Signal 16, S.oe=Signal 17 })
    , ("accum", PU def A.PUPorts{ A.init=Signal 18, A.load=Signal 19, A.neg=Signal 20, A.oe=Signal 21 } )
    , ("spi", PU (SPI.slaveSPI 0) SPI.PUPorts
        { SPI.wr=Signal 22, SPI.oe=Signal 23
        , SPI.stop="stop"
        , SPI.mosi=InputPort "mosi", SPI.miso=OutputPort "miso", SPI.sclk=InputPort "sclk", SPI.cs=InputPort "cs"
        })
    , ("div", PU (D.divider 8 True) D.PUPorts{ D.wr=Signal 24, D.wrSel=Signal 25, D.oe=Signal 26, D.oeSel=Signal 27 } )
    , ("mul", PU (M.multiplier True) M.PUPorts{ M.wr=Signal 28, M.wrSel=Signal 29, M.oe=Signal 30 } )
    ]


marchSPIDropData proxy = (marchSPI proxy){ bnAllowDrop=Just True }


-----------------------------------------------------------


-- |Dirty hack to avoid collision with parallel QuickCheck.
externalTestCntr = unsafePerformIO $ newCounter 0
{-# NOINLINE externalTestCntr #-}


algTestCase name arch alg = algTestCaseWithInput name [] arch alg


algTestCaseWithInput name is arch alg
    = testCase (name ++ " <" ++ fn ++ "_*>") $ do
        i <- incrCounter 1 externalTestCntr
        res <- testWithInput (fn ++ "_" ++ show i) is arch alg
        isRight res @? show res
    where
        fn = "bn_" ++ name
