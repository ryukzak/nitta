{-|
Module      : Main
Description : Fibonacci demo
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

Одним из классических примеров алгоритмов является расчёт последовательности Фибоначчи: 0, 1, 2, 3, 5, 8, 13.

Описанная ниже программа реализует два независимых процесса:

- расчёт последовательности Фибоначчи;
- расчёт последовательности целых чисел (номера элемента).

Каждый элемент этих последовательностей отправляется на внешний интерфейс, определяемый
конфигурацией процессора. В данном примере это интерфейс SPI ('NITTA.ProcessUnit.SPI').
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module Main ( main ) where

import           Data.Default
import           Data.FileEmbed                (embedStringFile)
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.Frontend
import qualified NITTA.ProcessUnits.Accum      as A
import qualified NITTA.ProcessUnits.Fram       as FR
import qualified NITTA.ProcessUnits.SPI        as SPI
import           NITTA.Types
import           NITTA.Types.Project
import           NITTA.Utils.Test              (demo)

nittaArch = busNetwork 20 Nothing
    [ InputPort "mosi", InputPort "sclk", InputPort "cs" ]
    [ OutputPort "miso" ]
    [ ("fram1", PU def FR.PUPorts{ FR.oe=Signal 1, FR.wr=Signal 2, FR.addr=map Signal [3, 4, 5, 6] } )
    , ("fram2", PU def FR.PUPorts{ FR.oe=Signal 7, FR.wr=Signal 8, FR.addr=map Signal [9, 10, 11, 12] } )
    , ("accum", PU def A.PUPorts{ A.init=Signal 13, A.load=Signal 14, A.neg=Signal 15, A.oe=Signal 16 } )
    , ("spi", PU
        (SPI.slaveSPI 17)
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
