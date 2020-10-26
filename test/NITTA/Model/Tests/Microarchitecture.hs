{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-partial-type-signatures #-}

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

import           Control.Monad ( void )
import           Data.Atomics.Counter ( incrCounter, newCounter )
import           Data.Default
import           Data.Proxy
import           NITTA.Intermediate.Types
import           NITTA.Model.Microarchitecture
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Networks.Types
import           NITTA.Model.ProcessorUnits
import           NITTA.Model.ProcessorUnits.Time
import           NITTA.Model.Types
import           NITTA.TargetSynthesis
import           System.IO.Unsafe ( unsafePerformIO )
import           Test.Tasty.HUnit

{-# ANN module "HLint: ignore Reduce duplication" #-}

pInt = Proxy :: Proxy Int
pIntX32 = Proxy :: Proxy (IntX 32)
pIntX48 = Proxy :: Proxy (IntX 48)
pIntX64 = Proxy :: Proxy (IntX 64)
pIntX128 = Proxy :: Proxy (IntX 128)
pFX22_32 = Proxy :: Proxy (FX 22 32)
pFX32_32 = Proxy :: Proxy (FX 32 32)
pFX42_64 = Proxy :: Proxy (FX 42 64)


basic :: ( Integral x, Val x ) => Proxy x -> BusNetwork String String x Int
basic _proxy = evalNetwork Sync $ do
    add "fram1" FramIO
    add "fram2" FramIO
    add "shift" ShiftIO
    add "accum" AccumIO
    add "mul" MultiplierIO
    add "div" DividerIO


march = basic pInt


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


-- FIXME: Support code like this in NITTA.Model.Microarchitecture. Such
-- functions should apply to modification of the target processor model at
-- synthesis-time, so it should keep the model state.

-- withSlaveSPI net = modifyNetwork net $ do
--     add "spi" SPISlave
--         { slave_mosi = InputPortTag "mosi"
--         , slave_miso = OutputPortTag "miso"
--         , slave_sclk = InputPortTag "sclk"
--         , slave_cs   = InputPortTag "cs"
--         }

-- withMasterSPI net = modifyNetwork net $ do
--     add "spi" SPIMaster
--         { master_mosi = OutputPortTag "mosi"
--         , master_miso = InputPortTag "miso"
--         , master_sclk = OutputPortTag "sclk"
--         , master_cs   = OutputPortTag "cs"
--         }

-- marchSPI True proxy = withSlaveSPI $ basic proxy
-- marchSPI False proxy = withMasterSPI $ basic proxy


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
