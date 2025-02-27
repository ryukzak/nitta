{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

{- |
Module      : NITTA.Model.Config.Tests
Description :
Copyright   : (c) Valerii Butorin, 2024
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Config.Tests (
    tests,
) where

import Data.Map as M
import Data.Maybe
import Data.Text qualified as T
import NITTA.Model.Microarchitecture.Config as Conf
import Test.Tasty (testGroup)
import Test.Tasty.HUnit

tests =
    testGroup
        "Configuration parsing"
        [ testCase "parse config common" $ do
            conf <- parseConfig "examples/microarch.yml"
            let
                lib = fromJust $ library conf
                fram = fromJust $ M.lookup (T.pack "fram{x}") lib
                nets = networks conf
                net = fromJust $ M.lookup (T.pack "net1") nets
                spi = fromJust $ M.lookup (T.pack "spi") (fromJust $ pus net)
                protos_ = fromJust $ protos net
                shift = fromJust $ M.lookup (T.pack "shift{x}") protos_
                div_ = fromJust $ M.lookup (T.pack "div{x}") protos_
            "Sync" @=? show (ioSync conf)
            True @=? mock conf
            "fx32.32" @=? T.unpack (valueType conf)
            2 @=? M.size (fromJust $ library conf)
            32 @=? Conf.size fram
            Just True @=? Conf.sRight shift
            4 @=? Conf.pipeline div_
            "mosi" @=? T.unpack (mosi spi)
            "miso" @=? T.unpack (miso spi)
            "sclk" @=? T.unpack (sclk spi)
            "cs" @=? T.unpack (cs spi)
            True @=? isSlave spi
            Just 6 @=? bufferSize spi
            0 @=? bounceFilter spi
        ]
