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
Module      : NITTA.Model.Tests.Microarchitecture
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Tests.Microarchitecture
    ( march
    , marchSPI
    , marchSPIDropData
    , algTestCase
    , externalTestCntr
    , runTargetSynthesisWithUniqName
    , microarch, IOUnit(..)
    , pInt, pIntX32, pIntX48, pIntX64, pIntX128, pFX32_32, pFX22_32, pFX42_64
    ) where

import           Control.Monad                   (void)
import           Data.Atomics.Counter            (incrCounter, newCounter)
import           Data.Default
import           Data.Proxy
import           NITTA.Intermediate.Types
import           NITTA.Model.MicroArchitecture
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Networks.Types
import           NITTA.Model.ProcessorUnits
import           NITTA.Model.ProcessorUnits.Time
import           NITTA.Model.TargetSystem
import           NITTA.TargetSynthesis
import           System.IO.Unsafe                (unsafePerformIO)
import           Test.Tasty.HUnit


pInt = Proxy :: Proxy Int
pIntX32 = Proxy :: Proxy (IntX 32)
pIntX48 = Proxy :: Proxy (IntX 48)
pIntX64 = Proxy :: Proxy (IntX 64)
pIntX128 = Proxy :: Proxy (IntX 128)
pFX22_32 = Proxy :: Proxy (FX 22 32)
pFX32_32 = Proxy :: Proxy (FX 32 32)
pFX42_64 = Proxy :: Proxy (FX 42 64)


march :: BusNetwork String String Int Int
march = evalNetwork Sync $ do
    add "fram1" FramIO
    add "fram2" FramIO
    add "shift" ShiftIO
    add "accum" AccumIO
    add "mul" MultiplierIO
    add "div" DividerIO


marchSPI ::
    ( Integral x, Val x
    ) => Bool -> Proxy x -> BusNetwork String String x Int
marchSPI isSlave _proxy = evalNetwork Sync $ do
    add "fram1" FramIO
    add "fram2" FramIO
    add "shift" ShiftIO
    add "accum" AccumIO
    add "spi" $ if isSlave
        then SPISlave
                { slave_mosi = InputPortTag "mosi"
                , slave_miso = OutputPortTag "miso"
                , slave_sclk = InputPortTag "sclk"
                , slave_cs   = InputPortTag "cs"
                }
        else SPIMaster
                { master_mosi = OutputPortTag "mosi"
                , master_miso = InputPortTag "miso"
                , master_sclk = OutputPortTag "sclk"
                , master_cs   = OutputPortTag "cs"
                }


marchSPIDropData isSlave proxy = (marchSPI isSlave proxy){ ioSync=ASync }


-----------------------------------------------------------


-- |Dirty hack to avoid collision with parallel QuickCheck.
externalTestCntr = unsafePerformIO $ newCounter 0
{-# NOINLINE externalTestCntr #-}

runTargetSynthesisWithUniqName t@TargetSynthesis{ tName } = do
    i <- incrCounter 1 externalTestCntr
    runTargetSynthesis t{ tName=tName ++ "_" ++ show i }

algTestCase n tMicroArch alg
    = testCase n $ void $ runTargetSynthesisWithUniqName (def :: TargetSynthesis _ _ _ Int)
        { tName=n
        , tMicroArch
        , tDFG=fsToDataFlowGraph alg
        }


data IOUnit
    = MasterSPI
    | SlaveSPI


microarch ioSync ioUnit = evalNetwork ioSync $ do
    add "fram1" FramIO
    add "fram2" FramIO
    add "shift" ShiftIO
    add "accum" AccumIO
    add "mul" MultiplierIO
    add "div" DividerIO
    add "spi" $ case ioUnit of
        SlaveSPI -> SPISlave
                { slave_mosi = InputPortTag "mosi"
                , slave_miso = OutputPortTag "miso"
                , slave_sclk = InputPortTag "sclk"
                , slave_cs   = InputPortTag "cs"
                }
        MasterSPI -> SPIMaster
                { master_mosi = OutputPortTag "mosi"
                , master_miso = InputPortTag "miso"
                , master_sclk = OutputPortTag "sclk"
                , master_cs   = OutputPortTag "cs"
                }
