{-|
Module      : Demo
Description : NITTA project demos
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

В данном модуле описано несколько демо для вычислительной платформы NITTA.

= Test bench organisation
Test bench was designed with the following components:

1.  The tool computer (imp001). It provides control of a
    modelling process and data transmission. There is using a single-board computer by Electric imp.
2.  A NITTA processor. It evaluates system dynamics model in the real-time and provides modelling
    data by its interfaces as inputs for a system under test. For realising this component DE-0 Nano
    board with Cyclone IV FPGA have been used.

The controller and the NITTA processor communicate throughout SPI (for data transmission)
interfaces. Those interfaces were chosen because they are ubiquitous.

A user and the test bench communicate throughout electricimp cloud-based IDE
<https://impcentral.electricimp.com>.

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
|  |        =     |  +--------+  |
|  | DIP: 4 3 2 1 |              |
|  |      =   =   |              |
|  +------|-|-|---+              |
|         | | +----- rendevou (down)
|         | +------ frequency (up)
|         +-------------- rst (down)
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

Для запуска и наладки испытательного стенда необходимо следующее программное обеспечение:

1. Проект [САПР NITTA]<https://nitta.io/nitta-corp/nitta>, включая реализацию данного модуля.
2. [haskell stack]<https://nitta.io/penskoi/nitta/src/master/doc/stack-install.md>, необходим для
   сборки САПР и генерации демонстрационного проекта.
3. [Quartus Prime Lite Edition]<https://fpgasoftware.intel.com/?edition=lite> and Cyclone IV device
   support. It's needed for synthesis and uploading a demo project into DE0-nano evaluation board.

Also, you need an account on <https://impcentral.electricimp.com> for getting data from the NITTA
processor. You can manually register and creating the project for that purpose or request login and
password from the project maintainer.

= Запуск демо

У вас на столе лежит собранный и подключённый стенд, на компьютере установлено всё необходимо
программное обеспечение и вы открыли консоль на соответсвующем каталоге. Дальнейшее описание
приведено для Windows 10, но также должно быть справедливо для Linux.

Каждое демо представляет из себя функцию, выполнение которой приведёт к генерации проекта, который
можно будет синтезировать, загрузить в испытательный стенд и проверить работоспособность. Её
выполнение приведет к генерации одноименного каталога с проектом в директории @/hdl/gen/DEMO_NAME@.
Для начала работы с демо необходимо либо вставить вызов функции в функцию 'Main.main', либо
осуществить запуск из @stack repl@.

>>> :m Demo
>>> fibonacciDemo
Demo project in hdl/gen/fibonacciDemo

Откройте сгенерированный проект в @Quartus@ (файл @nitta.qpf@) и синтезируйте его: @Processing ->
Start Compilation@. Затем вам необходимо прошить в ПЛИС результат. Для этого @Tools -> Programmer@,
затем кнопка @Start@. Если кнопка неактивна:

- проверьте подключение платы DE0-Nano;
- нажмите @Hardware Setup@ и выберите @USB-Blaster@;
- обратитесь к руководству пользователя отладочной платы:
  <http://www.terasic.com.tw/cgi-bin/page/archive.pl?Language=English&CategoryNo=165&No=593&PartNo=4>.

Убедитесь, что положение дип переключателей соответствует указанному на схеме выше.

Теперь подготовьте к работе управляющий конроллер. Для этого вам необходимо:

1. Поднять Wi-Fi сеть и подключить к ней контроллер в соответствии с инструкцией
   <https://developer.electricimp.com/gettingstarted/explorer/blinkup>.
2. В случае если вы используете аккаунт @aleksandpenskoi@ зайдите в строке @SPI_testbench@ выберите
   @Development Zone@, затем @Code@.
3. Убедитесь, что последней вызываемой функцией в правой части экрана (Device Code) является
   функция, одноименная названию демо.
4. Внизу слева вы увидете список подключённых к проекту устройств. Напротив нужного нажмите кнопку с
   иконкой On/Off.
5. В терминале снизу справа должен будет появиться журнал передачи данных на подобие приведённого
   ниже.

@
2018-08-20 16:19:27 +03:00 	[Status] 	Agent restarted: reload.
2018-08-20 16:19:28 +03:00 	[Status] 	Device connected
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 00 00 00 00 00
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 0	Значение: 0
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 01 00 00 00 01
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 1	Значение: 1
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 01 00 00 00 02
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 2	Значение: 1
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 02 00 00 00 03
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 3	Значение: 2
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 03 00 00 00 04
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 4	Значение: 3
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 05 00 00 00 05
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 5	Значение: 5
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 08 00 00 00 06
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 6	Значение: 8
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 0d 00 00 00 07
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 7	Значение: 13
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 15 00 00 00 08
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 8	Значение: 21
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 22 00 00 00 09
2018-08-20 16:19:28 +03:00 	[Device] 	> Номер: 9	Значение: 34
@

Аналогичным образом выглядят будет выглядеть работу других демо.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Demo
    ( -- * Демо
      fibonacciDemo
    , teacupDemo
      -- * Описание алгоритмов
    , fibonacciAlg
    , teacupAlg, teacupLua, teacupAlg2
      -- * Описание процессоров
    , nittaArch
    ) where

import           Data.Default
import           Data.Text                     (Text)
import           NITTA.BusNetwork
import           NITTA.Compiler
import qualified NITTA.Functions               as F
import qualified NITTA.ProcessUnits.Accum      as A
import qualified NITTA.ProcessUnits.Divider    as D
import qualified NITTA.ProcessUnits.Fram       as FR
import qualified NITTA.ProcessUnits.Multiplier as M
import qualified NITTA.ProcessUnits.Shift      as S
import qualified NITTA.ProcessUnits.SPI        as SPI
import           NITTA.Project
import           NITTA.Types
import           Text.InterpolatedString.Perl6 (qq)


-- FIXME: В настоящее время при испытании на стенде сигнал rst не приводит к сбросу вычислителя в начальное состояние.

-- TODO: Необходимо иметь возможность указать, какая именно частота будет у целевого вычислителя. Данная задача связана
-- с задачей о целевой платформе.
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
fibonacciDemo = demo Project
    { projectName="fibonacciDemo"
    , libraryPath="../.."
    , projectPath="hdl/gen/fibonacciDemo"
    , model=mkModelWithOneNetwork nittaArch fibonacciAlg
    , testCntx=Nothing
    }

fibonacciAlg = [ F.loop 0 "a_new" ["a", "a_send"]
               , F.loop 1 "b_new" ["b", "a_new"]
               , F.add "a" "b" ["b_new"]
               , F.send "a_send"

               , F.loop 0  "index_new" ["index", "index_send"]
               , F.constant 1 ["one"]
               , F.add "index" "one" ["index_new"]
               , F.send "index_send"
               ]



-- |Классический пример из области системной динамики. Описание самой модели приведено здесь:
-- <https://pysd-cookbook.readthedocs.io/en/latest/analyses/getting_started/Hello_World_Teacup.html>.
-- Вычисления производятся в числах с фиксированной запятой. Перевод в десятичные дроби не
-- осуществляется.
--
-- Выходные данные модели:
--
-- - температура чашки;
-- - время с начала эксперимента.
--
-- Каждый элемент этих последовательностей отправляется на внешний интерфейс, определяемый
-- конфигурацией процессора. В данном примере это интерфейс SPI ('NITTA.ProcessUnit.SPI').
teacupDemo = demo Project
    { projectName="teacupDemo"
    , libraryPath="../.."
    , projectPath="hdl/gen/teacupDemo"
    , model=mkModelWithOneNetwork nittaArch teacupAlg
    , testCntx=Nothing
    }

teacupAlg = [ F.loop 0 "time_new" ["time", "time_send"]
            , F.constant 125 ["time_step_1", "time_step_2"]
            , F.add "time" "time_step_1" ["time_new"]
            , F.send "time_send"

            , F.constant 70000 ["temp_room"]
            , F.constant 10000 ["temp_ch"]
            , F.loop 180000 "temp_cup_new" ["temp_cup_1", "temp_cup_2", "temp_cup_send"]
            -- (Teacup Temperature - temp_room) / temp_ch
            , F.sub "temp_room" "temp_cup_1" ["acc"]
            , F.division "acc" "temp_ch" ["temp_loss"] []

            -- INTEG ( -temp_loss to Room
            , F.multiply "temp_loss" "time_step_2" ["delta"]
            , F.add "temp_cup_2" "delta" ["temp_cup_new"]
            , F.send "temp_cup_send"
            ]

teacupAlg2 =
    [ F.send "time#3_1"
    , F.send "temp_cup#4_2"
    , F.add "time#3_0" "time_step_constant#2_1" ["time_0"]
    , F.sub "temp_room_constant#1_0" "temp_cup#4_1" ["acc_0"]
    , F.division "acc_0" "temp_ch_constant#0_0" ["temp_loss_0"] []
    , F.multiply "temp_loss_0" "time_step_constant#2_0" ["delta_0"]
    , F.add "temp_cup#4_0" "delta_0" ["temp_cup_0"]

    , F.loop 0 "time_0" ["time#3_0", "time#3_1"]
    , F.loop 180000 "temp_cup_0" [ "temp_cup#4_0", "temp_cup#4_1","temp_cup#4_2"]
    , F.constant 125 ["time_step_constant#2_1", "time_step_constant#2_0"]
    , F.constant 70000 ["temp_room_constant#1_0"]
    , F.constant 10000 ["temp_ch_constant#0_0"]
    ] :: [F (Parcel String Int)]

teacupLua =
    [qq|function teacup(time, temp_cup)
            local temp_ch = 10000
            local temp_room = 70000
            local time_step = 125

            send(time)
            send(temp_cup)

            time = time + time_step
            local acc = temp_room - temp_cup
            local temp_loss, _ = acc / temp_ch

            local delta = temp_loss * time_step
            temp_cup = temp_cup + delta

            teacup(time, temp_cup)
        end
        teacup(0, 180000)|] :: Text



-----------------------------------------------------------

demo prj@Project{ projectPath, model } = do
    let prj' = prj{ model=schedule model }
    writeProject prj'
    putStrLn $ "Demo project in " ++ projectPath
