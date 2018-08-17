{-|
В данном модуле описано несколько демо для вычислительной платформы NITTA.

= Test bench organisation

Test bench was designed with the following components:

1.  The tool computer. It provides control of a modelling process and data transmission. There is
    using a single-board computer by Electric imp (imp001 right now).
2.  A NITTA processor. It evaluates system dynamics model in the real-time and provides modelling
    data by its interfaces as inputs for a system under test. For realising this component DE-0 Nano
    board with Cyclone IV FPGA have been used.

The controller and the NITTA processor communicate throughout SPI (for data transmission)
interfaces. Those interfaces were chosen because they are ubiquitous.

A user and the test bench communicate throughout electricimp cloud-based IDE
(https://impcentral.electricimp.com).

@
+---------------------------------------------+
| Browser: https://impcentral.electricimp.com |
+---------------------------------------------+
    |
    |
    |
+----------+
| Internet |
+----------+
    |
    | Wi-Fi (old version, maybe you need to use Wi-Fi tethering)
    |
+--------+   SPI   +----------+
| imp001 |---------| DE0-nano |
+--------+         +----------+
@

= Схема подключение интерфейса SPI к DE0-nano

В скобках указан рекомендованный цвет провода. Пропорции нарушены.

@
           +--------------------------------------- GND (черный)
+----------|---------------------+
|    ooooo *oooo ooooo ooooo     |
|    ooooo ooooo ooooo o****     |
|                       |||+------------------------ CS   (белый)
+-----+                 ||+------------------------- SCLK (жёлтый)
| USB |                 |+-------------------------- MISO (синий)
+-----+                 +--------------------------- MOSI (оранжевый)
|                    +--------+  |
|  +--------------+  | ALTERA |  |
|  | DIP: 4 3 2 1 |  +--------+  |
|  +------|-|-|---+              |
|         | | +------ rendevou   |
|         | +-------- frequency  |
|         +---------- rst        |
|                                |
|                                |
|    ooooo ooooo ooooo ooooo     |
|    ooooo ooooo ooooo ooooo     |
+--------------------------------+
@

= Схема подключение интерфейса SPI к imp001

@
                    +--------------------------------+
                    | o GND                          |
                    | o VIN                          |
                    | o                              |
                    | o Pin1                         |
(синий)     MISO------* Pin2           imp001        |
(жёлтый)    SCLK------* Pin5                         |
(оранжевый) MOSI------* Pin7                         |
(белый)       CS -----* Pin8                         |
                    | o Dtc9                         |
                    | o 3V3   +-----+                |
(черный)     GND -----* GND   | USB |                |
                    +---------+-----+----------------+
@

= Требуемоемое программное обеспечение

TODO: Указать какое ПО надо установить для работы со стендом и как именно его запустить.

= Запуск демо

Каждое демо представляет из себя функцию, выполнение которой приведёт к генерации проекта, который
можно будет синтезировать, загрузить в испытательный стенд и проверить работоспособность. Для начала
работы с демо необходимо либо вставить вызов функции в функцию 'Main.main', либо осуществить запуск
из @ghci@.

Её выполнение приведет к генерации одноименного каталога с проектом в директории
@/hdl/gen/DEMO_NAME@, внутри которого необходимо найти файл @DEMO_NAME.qpf@, затем открыть его в
@Quartus@ и:

1. синтезировать проект;
2. открыть программатор и загрузить его в ПЛИС.

-}

{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Demo
    ( -- * Демо
      fibonacciDemo
    , teacupDemo
      -- * Описание алгоритмов
    , fibonacciAlg
    , teacupAlg
      -- * Описание процессоров
    , nittaArch
    ) where

import           Data.Default
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.DataFlow
import qualified NITTA.FunctionBlocks          as FB
import qualified NITTA.ProcessUnits.Accum      as A
import qualified NITTA.ProcessUnits.Divisor    as D
import qualified NITTA.ProcessUnits.Fram       as FR
import qualified NITTA.ProcessUnits.Multiplier as M
import qualified NITTA.ProcessUnits.Shift      as S
import qualified NITTA.ProcessUnits.SPI        as SPI
import           NITTA.Project
import           NITTA.Types
import           System.FilePath               (joinPath)


nittaArch = busNetwork 31 Nothing
    [ InputPort "mosi", InputPort "sclk", InputPort "cs" ]
    [ OutputPort "miso" ]
    [ ("fram1", PU def FR.PUPorts{ FR.oe=Signal 11, FR.wr=Signal 10, FR.addr=map Signal [9, 8, 7, 6] } )
    , ("fram2", PU def FR.PUPorts{ FR.oe=Signal 5, FR.wr=Signal 4, FR.addr=map Signal [3, 2, 1, 0] } )
    , ("shift", PU def S.PUPorts{ S.work=Signal 12, S.direction=Signal 13, S.mode=Signal 14, S.step=Signal 15, S.init=Signal 16, S.oe=Signal 17 })
    , ("accum", PU def A.PUPorts{ A.init=Signal 18, A.load=Signal 19, A.neg=Signal 20, A.oe=Signal 21 } )
    , ("spi", PU
        (SPI.slaveSPI 10)
        SPI.PUPorts{ SPI.wr=Signal 22, SPI.oe=Signal 23
                    , SPI.stop="stop"
                    , SPI.mosi=InputPort "mosi", SPI.miso=OutputPort "miso", SPI.sclk=InputPort "sclk", SPI.cs=InputPort "cs"
                    })
    , ("mul", PU (M.multiplier True) M.PUPorts{ M.wr=Signal 24, M.wrSel=Signal 25, M.oe=Signal 26 } )
    , ("div", PU (D.divisor 4 True) D.PUPorts{ D.wr=Signal 27, D.wrSel=Signal 28, D.oe=Signal 29, D.oeSel=Signal 30 } )
    ]
-- TODO: add frequency
-- TODO: fix rst


-- |Одним из классических примеров алгоритмов является расчёт последовательности Фибоначчи: 0, 1, 2,
-- 3, 5, 8, 13.
--
-- Описанная ниже программа реализует два независимых процесса:
--
-- - расчёт последовательности Фибоначчи;
-- - расчёт последовательности целых чисел (номера элемента).
--
-- Каждый элемент этих последовательностей отправляется на внешний интерфейс, определяемый
-- конфигурацией процессора. В данном примере это интерфейс SPI ('NITTA.ProcessUnit.SPI').
fibonacciDemo = demo "fibonacci" nittaArch fibonacciAlg

fibonacciAlg = [ FB.loop' 0 "a_new" ["a", "a_send"]
               , FB.loop' 1 "b_new" ["b", "a_new"]
               , FB.add "a" "b" ["b_new"]
               , FB.send "a_send"

               , FB.loop' 0  "index_new" ["index", "index_send"]
               , FB.constant 1 ["one"]
               , FB.add "index" "one" ["index_new"]
               , FB.send "index_send"
               ] :: [FB (Parcel String Int)]



teacupDemo = demo "teacup" nittaArch teacupAlg

teacupAlg = [ FB.loop' 0 "time_new" ["time", "time_send"]
            , FB.constant 125 ["time_step_1", "time_step_2"]
            , FB.add "time" "time_step_1" ["time_new"]
            , FB.send "time_send"

            , FB.constant 70000 ["temp_room"]
            , FB.constant 10000 ["temp_ch"]
            , FB.loop' 180000 "temp_cup_new" ["temp_cup_1", "temp_cup_2", "temp_cup_send"]
            -- (Teacup Temperature - temp_room) / temp_ch
            , FB.sub "temp_room" "temp_cup_1" ["acc"]
            , FB.division "acc" "temp_ch" ["temp_loss"] []

            -- INTEG ( -temp_loss to Room
            , FB.multiply "temp_loss" "time_step_2" ["delta"]
            , FB.add "temp_cup_2" "delta" ["temp_cup_new"]
            , FB.send "temp_cup_send"
            ] :: [FB (Parcel String Int)]



-----------------------------------------------------------

demo n arch alg = do
    let pu = nitta $ synthesis $ frame arch $ dfgraph alg
    let pwd = joinPath ["hdl", "gen", n]
    let prj = Project n "../.." pwd pu
    let cntx = def
    writeProject prj
    writeImplementation pwd $ testBenchDescription prj cntx

dfgraph = DFG . map node

frame arch g
  = let ma = bindAll (functionalBlocks g) arch
    in Frame ma g Nothing :: SystemState String String String Int (TaggedTime String Int)

synthesis f = foldl (\f' _ -> naive def f') f $ replicate 50 ()
