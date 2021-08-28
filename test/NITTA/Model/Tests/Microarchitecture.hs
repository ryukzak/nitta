{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -fno-warn-partial-type-signatures #-}

{- |
Module      : NITTA.Model.Tests.Microarchitecture
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Tests.Microarchitecture (
    march,
    marchSPI,
    marchSPIDropData,
    maBroken,
    microarch,
    IOUnit (..),
    pInt,
    pIntX32,
    pIntX48,
    pIntX64,
    pIntX128,
    pFX32_32,
    pFX22_32,
    pFX42_64,
    pFX48_64,
    pAttrIntX32,
    pAttrFX22_32,
    module NITTA.Model.Networks.Types,
) where

import Data.Proxy
import qualified Data.Text as T
import NITTA.Intermediate.Types
import NITTA.Model.Microarchitecture.Builder
import NITTA.Model.Networks.Bus
import NITTA.Model.Networks.Types
import NITTA.Model.ProcessorUnits

pInt = Proxy :: Proxy Int

pAttrIntX32 = Proxy :: Proxy (Attr (IntX 32))

pAttrFX22_32 = Proxy :: Proxy (Attr (FX 22 32))

pIntX32 = Proxy :: Proxy (IntX 32)

pIntX48 = Proxy :: Proxy (IntX 48)

pIntX64 = Proxy :: Proxy (IntX 64)

pIntX128 = Proxy :: Proxy (IntX 128)

pFX22_32 = Proxy :: Proxy (FX 22 32)

pFX32_32 = Proxy :: Proxy (FX 32 32)

pFX42_64 = Proxy :: Proxy (FX 42 64)

pFX48_64 = Proxy :: Proxy (FX 48 64)

basic :: (Integral x, Val x) => Proxy x -> BusNetwork T.Text T.Text x Int
basic _proxy = defineNetwork "net1" ASync $ do
    add "fram1" FramIO
    add "fram2" FramIO
    add "shift" ShiftIO
    add "accum" AccumIO
    add "mul" MultiplierIO
    add "div" DividerIO

march = basic pInt

-- |Simple microarchitecture with broken PU for negative tests
maBroken :: (Integral x, Val x) => Broken T.Text x Int -> BusNetwork T.Text T.Text x Int
maBroken brokenPU = defineNetwork "net1" ASync $ do
    add "fram1" FramIO
    add "fram2" FramIO
    add "accum" AccumIO
    addCustom "broken" brokenPU BrokenIO

withSlaveSPI net = modifyNetwork net $ do
    add
        "spi"
        SPISlave
            { slave_mosi = InputPortTag "mosi"
            , slave_miso = OutputPortTag "miso"
            , slave_sclk = InputPortTag "sclk"
            , slave_cs = InputPortTag "cs"
            }

withMasterSPI net = modifyNetwork net $ do
    add
        "spi"
        SPIMaster
            { master_mosi = OutputPortTag "mosi"
            , master_miso = InputPortTag "miso"
            , master_sclk = OutputPortTag "sclk"
            , master_cs = OutputPortTag "cs"
            }

marchSPI True proxy = withSlaveSPI $ basic proxy
marchSPI False proxy = withMasterSPI $ basic proxy

marchSPIDropData isSlave proxy = (marchSPI isSlave proxy){ioSync = ASync}

-----------------------------------------------------------

data IOUnit
    = MasterSPI
    | SlaveSPI

microarch ioSync' ioUnit =
    let withSPI net SlaveSPI = withSlaveSPI net
        withSPI net MasterSPI = withMasterSPI net
     in defineNetwork
            "net1"
            ioSync'
            ( do
                add "fram1" FramIO
                add "fram2" FramIO
                add "shift" ShiftIO
                add "accum" AccumIO
                add "mul" MultiplierIO
                add "div" DividerIO
            )
            `withSPI` ioUnit
