{-|
Module      : Main
Description : Teacup demo
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

Классический пример из области системной динамики. Описание самой модели приведено здесь:
<https://pysd-cookbook.readthedocs.io/en/latest/analyses/getting_started/Hello_World_Teacup.html>.
Вычисления производятся в числах с фиксированной запятой. Перевод в десятичные дроби не
осуществляется.

Выходные данные модели:

- температура чашки;
- время с начала эксперимента.

Каждый элемент этих последовательностей отправляется на внешний интерфейс, определяемый
конфигурацией процессора. В данном примере это интерфейс SPI (`NITTA.ProcessUnit.SPI`).
-}

{-
 This is the classical example of system dinamic. The model description presented here:
 <https://pysd-cookbook.readthedocs.io/en/latest/analyses/getting_started/Hello_World_Teacup.html>.
 Calculations are performed in fixed-point numbers. Decimals are not converted.

 Model's outputs:

- cups temperature;
- time from experiment beginning.

Every element of the sequences sends to the external interface, definable by the processor configuration. 
It is SPI Interface in this example ('NITTA.ProcessUnit.SPI').
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
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
import qualified NITTA.ProcessUnits.Divider    as D
import qualified NITTA.ProcessUnits.Fram       as FR
import qualified NITTA.ProcessUnits.Multiplier as M
import qualified NITTA.ProcessUnits.Shift      as S
import qualified NITTA.ProcessUnits.SPI        as SPI
import           NITTA.Types
import           NITTA.Types.Project
import           NITTA.Utils.Test              (demo)

nittaArch = busNetwork 31 Nothing
    [ InputPort "mosi", InputPort "sclk", InputPort "cs" ]
    [ OutputPort "miso" ]
    [ ("fram1", PU def FR.PUPorts{ FR.oe=Signal 11, FR.wr=Signal 10, FR.addr=map Signal [9, 8, 7, 6] } )
    , ("fram2", PU def FR.PUPorts{ FR.oe=Signal 5, FR.wr=Signal 4, FR.addr=map Signal [3, 2, 1, 0] } )
    , ("shift", PU def S.PUPorts{ S.work=Signal 12, S.direction=Signal 13, S.mode=Signal 14, S.step=Signal 15, S.init=Signal 16, S.oe=Signal 17 })
    , ("accum", PU def A.PUPorts{ A.init=Signal 18, A.load=Signal 19, A.neg=Signal 20, A.oe=Signal 21 } )
    , ("spi", PU
        (SPI.slaveSPI 4)
        SPI.PUPorts{ SPI.wr=Signal 22, SPI.oe=Signal 23
                    , SPI.stop="stop"
                    , SPI.mosi=InputPort "mosi", SPI.miso=OutputPort "miso", SPI.sclk=InputPort "sclk", SPI.cs=InputPort "cs"
                    })
    , ("mul", PU (M.multiplier True) M.PUPorts{ M.wr=Signal 24, M.wrSel=Signal 25, M.oe=Signal 26 } )
    , ("div", PU (D.divider 4 True) D.PUPorts{ D.wr=Signal 27, D.wrSel=Signal 28, D.oe=Signal 29, D.oeSel=Signal 30 } )
    ] :: BusNetwork String String (IntX 32) Int

teacupLua = $(embedStringFile "examples/teacup.lua")

-- |
-- @
-- teacupAlg =
--     [ F.loop 0 "time_new" ["time", "time_send"]
--     , F.constant 125 ["time_step_1", "time_step_2"]
--     , F.add "time" "time_step_1" ["time_new"]
--     , F.send "time_send"
--
--     , F.constant 70000 ["temp_room"]
--     , F.constant 10000 ["temp_ch"]
--     , F.loop 180000 "temp_cup_new" ["temp_cup_1", "temp_cup_2", "temp_cup_send"]
--     -- (Teacup Temperature - temp_room) / temp_ch
--     , F.sub "temp_room" "temp_cup_1" ["acc"]
--     , F.division "acc" "temp_ch" ["temp_loss"] []
--
--     -- INTEG ( -temp_loss to Room
--     , F.multiply "temp_loss" "time_step_2" ["delta"]
--     , F.add "temp_cup_2" "delta" ["temp_cup_new"]
--     , F.send "temp_cup_send"
--     ]
-- @
teacupAlg = lua2functions teacupLua

main = demo Project
    { projectName="teacupDemo"
    , libraryPath="../.."
    , projectPath="hdl/gen/teacupDemo"
    , processorModel=mkModelWithOneNetwork nittaArch teacupAlg
    , testCntx=Nothing
    , targetPlatforms=[ Makefile, DE0Nano ]
    }
