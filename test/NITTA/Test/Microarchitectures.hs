{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Test.Microarchitectures
    ( march
    , marchSPI
    , marchSPIDropData
    , algTestCase
    , externalTestCntr
    , runTargetSynthesis'
    , microarch, IOUnit(..)
    , pInt, pIntX32, pIntX48, pIntX64, pIntX128, pFX22_32, pFX42_64
    ) where

import           Control.Monad                    (void)
import           Data.Atomics.Counter             (incrCounter, newCounter)
import           Data.Default
import           Data.Proxy
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Networks.Types
import           NITTA.Model.ProcessorUnits
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.TargetSystem
import           NITTA.Project
import           System.IO.Unsafe                 (unsafePerformIO)
import           Test.Tasty.HUnit


pInt = Proxy :: Proxy Int
pIntX32 = Proxy :: Proxy (IntX 32)
pIntX48 = Proxy :: Proxy (IntX 48)
pIntX64 = Proxy :: Proxy (IntX 64)
pIntX128 = Proxy :: Proxy (IntX 128)
pFX22_32 = Proxy :: Proxy (FX 22 32)
pFX42_64 = Proxy :: Proxy (FX 42 64)


march :: BusNetwork String String Int Int
march = busNetwork 31 (Just True)
    [ ("fram1", PU def def FramPorts{ oe=SignalTag 0, wr=SignalTag 1, addr=map SignalTag [2, 3, 4, 5] } FramIO)
    , ("fram2", PU def def FramPorts{ oe=SignalTag 6, wr=SignalTag 7, addr=map SignalTag [8, 9, 10, 11] } FramIO)
    , ("shift", PU def def ShiftPorts{ work=SignalTag 12, direction=SignalTag 13, mode=SignalTag 14, step=SignalTag 15, init=SignalTag 16, oe=SignalTag 17 } ShiftIO)
    , ("accum", PU def def AccumPorts{ init=SignalTag 18, load=SignalTag 19, neg=SignalTag 20, oe=SignalTag 21 } AccumIO)
    , ("mul", PU def (multiplier True) MultiplierPorts{ wr=SignalTag 22, wrSel=SignalTag 23, oe=SignalTag 24 } MultiplierIO)
    , ("div", PU def (divider 8 True) DividerPorts{ wr=SignalTag 25, wrSel=SignalTag 26, oe=SignalTag 27, oeSel=SignalTag 28 } DividerIO)
    ]


marchSPI ::
    ( Integral x, Val x
    ) => Bool -> Proxy x -> BusNetwork String String x Int
marchSPI isSlave _proxy = busNetwork 31 (Just False)
    [ ("fram1", PU def def FramPorts{ oe=SignalTag 11, wr=SignalTag 10, addr=map SignalTag [9, 8, 7, 6] } FramIO)
    , ("fram2", PU def def FramPorts{ oe=SignalTag 5, wr=SignalTag 4, addr=map SignalTag [3, 2, 1, 0] } FramIO)
    , ("shift", PU def def ShiftPorts{ work=SignalTag 12, direction=SignalTag 13, mode=SignalTag 14, step=SignalTag 15, init=SignalTag 16, oe=SignalTag 17 } ShiftIO)
    , ("accum", PU def def AccumPorts{ init=SignalTag 18, load=SignalTag 19, neg=SignalTag 20, oe=SignalTag 21 } AccumIO)
    , ("spi", PU def (anySPI 0) SimpleIOPorts
        { wr=SignalTag 22, oe=SignalTag 23
        , stop="stop"
        }
        $ if isSlave
        then SPISlave
            { slave_mosi=InputPortTag "mosi"
            , slave_miso=OutputPortTag "miso"
            , slave_sclk=InputPortTag "sclk"
            , slave_cs=InputPortTag "cs"
            }
        else SPIMaster
            { master_mosi=OutputPortTag "mosi"
            , master_miso=InputPortTag "miso"
            , master_sclk=OutputPortTag "sclk"
            , master_cs=OutputPortTag "cs"
            })
    , ("mul", PU def (multiplier True) MultiplierPorts{ wr=SignalTag 28, wrSel=SignalTag 29, oe=SignalTag 30 } MultiplierIO)
    , ("div", PU def (divider 8 True) DividerPorts{ wr=SignalTag 24, wrSel=SignalTag 25, oe=SignalTag 26, oeSel=SignalTag 27 } DividerIO)
    ]


marchSPIDropData isSlave proxy = (marchSPI isSlave proxy){ bnAllowDrop=Just True }


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


data IOUnit
    = MasterSPI
    | SlaveSPI


microarch ioMode ioUnit = busNetwork 31 (Just ioMode)
    [ ("fram1", PU def def FramPorts{ oe=SignalTag 11, wr=SignalTag 10, addr=map SignalTag [9, 8, 7, 6] } FramIO )
    , ("fram2", PU def def FramPorts{ oe=SignalTag 5, wr=SignalTag 4, addr=map SignalTag [3, 2, 1, 0] } FramIO )
    -- , ("shift", PU def S.Ports{ S.work=SignalTag 12, S.direction=SignalTag 13, S.mode=SignalTag 14, S.step=SignalTag 15, S.init=SignalTag 16, S.oe=SignalTag 17 })
    , ("accum", PU def def AccumPorts{ init=SignalTag 18, load=SignalTag 19, neg=SignalTag 20, oe=SignalTag 21 } AccumIO )
    , ("mul", PU def (multiplier True) MultiplierPorts{ wr=SignalTag 24, wrSel=SignalTag 25, oe=SignalTag 26 } MultiplierIO )
    , ("div", PU def (divider 4 True) DividerPorts{ wr=SignalTag 27, wrSel=SignalTag 28, oe=SignalTag 29, oeSel=SignalTag 30 } DividerIO )
    ,   ( "io"
        , case ioUnit of
            SlaveSPI -> PU def (anySPI 0)
                SimpleIOPorts
                    { wr=SignalTag 22, oe=SignalTag 23
                    , stop="stop"
                    }
                SPISlave
                    { slave_mosi=InputPortTag "mosi"
                    , slave_miso=OutputPortTag "miso"
                    , slave_sclk=InputPortTag "sclk"
                    , slave_cs=InputPortTag "cs"
                    }
            MasterSPI -> PU def (anySPI 0) 
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
    ]
