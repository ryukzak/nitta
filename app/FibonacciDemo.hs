{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : Main
Description : Fibonacci demo
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

The Fibonacci sequency calcutation is the one of classical examples of
algoritms: 0, 1, 2, 3, 5, 8, 13...|

The program that described below realize 2 independent processes:
- fibonacci sequence calculation;
- natural counter.

Every element of the sequences sends to the external interface, definable by the
processor configuration. It is SPI Interface in this example
('NITTA.ProcessUnit.SPI').

To execute the demo:
1. Build NITTA project by @stack build --fast@.
2. Generate target system demo project for the demo by @stack exec
   nitta-fibonacci-demo@.
3. Execute the target system demo project on a hardware test bench.
-}
module Main ( main ) where

import           Data.Default
import           Data.FileEmbed           (embedStringFile)
import           NITTA.BusNetwork
import           NITTA.Frontend
import qualified NITTA.ProcessUnits.Accum as A
import qualified NITTA.ProcessUnits.Fram  as FR
import qualified NITTA.ProcessUnits.SPI   as SPI
import           NITTA.Types
import           NITTA.Types.Project
import           NITTA.Utils.Test         (demo, mkModelWithOneNetwork)

nittaArch = busNetwork 20 Nothing
    [ InputPort "mosi", InputPort "sclk", InputPort "cs" ]
    [ OutputPort "miso" ]
    [ ("fram1", PU def FR.PUPorts{ FR.oe=Signal 1, FR.wr=Signal 2, FR.addr=map Signal [3, 4, 5, 6] } )
    , ("fram2", PU def FR.PUPorts{ FR.oe=Signal 7, FR.wr=Signal 8, FR.addr=map Signal [9, 10, 11, 12] } )
    , ("accum", PU def A.PUPorts{ A.init=Signal 13, A.load=Signal 14, A.neg=Signal 15, A.oe=Signal 16 } )
    , ("spi", PU
        (SPI.slaveSPI 4)
        SPI.PUPorts{ SPI.wr=Signal 18, SPI.oe=Signal 19
                    , SPI.stop="stop"
                    , SPI.mosi=InputPort "mosi", SPI.miso=OutputPort "miso", SPI.sclk=InputPort "sclk", SPI.cs=InputPort "cs"
                    })
    ] :: BusNetwork String String (IntX 32) Int

fibonacciLua = $(embedStringFile "examples/fibonacci.lua")

-- |
-- @
-- fibonacciAlg = [ F.loop 0 "a_new" ["a", "a_send"]
--                , F.loop 1 "b_new" ["b", "a_new"]
--                , F.add "a" "b" ["b_new"]
--                , F.send "a_send"

--                , F.loop 0  "i_new" ["i", "i_send"]
--                , F.constant 1 ["one"]
--                , F.add "i" "one" ["i_new"]
--                , F.send "i_send"
--                ]
-- @
-- fibonacciAlg :: [F String (IntX 42)]
fibonacciAlg = lua2functions fibonacciLua

main = demo Project
    { projectName="fibonacciDemo"
    , libraryPath="../.."
    , projectPath="hdl/gen/fibonacciDemo"
    , processorModel=mkModelWithOneNetwork nittaArch fibonacciAlg
    , testCntx=Nothing
    , targetPlatforms=[ Makefile, DE0Nano ]
    }
