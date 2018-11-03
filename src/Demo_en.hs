{-|
Module      : Demo
Description : NITTA project demos
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

There is several demostrations explanation for countint platform NITTA in this module.

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

=SPI - DEO-nano сonnection diagram.

There is recommended color of wire in the brackets

@
           +--------------------------------------- GND (black)
+----------|---------------------+
|    ooooo *oooo ooooo ooooo     |
|    ooooo ooooo ooooo o****     |
|                       |||+------------------------ CS   (white)
+-----+                 ||+------------------------- SCLK (yellow)
| USB |                 |+-------------------------- MISO (blue)
+-----+                 +--------------------------- MOSI (orange)
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

= SPI - imp001 connection diagram

@
                    +--------------------------------+
                    | o GND                          |
                    | o VIN                          |
                    | o                              |
                    | o Pin1                         |
(blue)     	MISO------* Pin2           imp001        |
(yellow)    SCLK------* Pin5                         |
(orange) 	MOSI------* Pin7                         |
(white)       CS -----* Pin8                         |
                    | o Dtc9                         |
                    | o 3V3   +-----+                |
(black)     GND -----* GND   | USB |                |
                    +---------+-----+----------------+
@

Software requirments:

The software that is required to run and set up the test bench:

1. Project [NITTA CAD] <https://nitta.io/nitta-corp/nitta>, this module realisation included.
2. [Haskell stack] <https://nitta.io/penskoi/nitta/src/master/doc/stack-install.md>. 
Its required for CAD set up and demo generation.
3. [Quartus Prime Lite Edition]<https://fpgasoftware.intel.com/?edition=lite> and Cyclone IV device
   support. It's needed for synthesis and uploading a demo project into DE0-nano evaluation board.

Also, you need an account on <https://impcentral.electricimp.com> for getting data from the NITTA
processor. You can manually register and creating the project for that purpose or request login and
password from the project maintainer.

=Demo start up

You have collected and pluged in stand on your table, on your computer you have already installed 
all needed software and you have opened the terminal in suitable catalog. Furthern instructions 
is written for Wndows 10, but it can be used for Linux too. 

Each demo is a function, the execution of which will cause generation of project, that you 
can synthesize, upload to test bench and test perfomance. Its execution will cause generation 
of directory with te same name with project in directory @/hdl/gen/DEMO_NAME@.
To start working with the demo, you have to insert a function call into the function 'Main.main', 
or start from @stack repl @.

>>> :m Demo
>>> fibonacciDemo
Demo project in hdl/gen/fibonacciDemo

Open generated project in @Quartus@ (@nitta.qpf@ file) and synthesize it: @Processing ->
Start Compilation@. After this you need to flash result in FPG. To do this you need @Tools -> Programmer@,
затем кнопка @Start@. If the button is deactiveted:

- check DE0-Nano board connection.
- press @Hardware Setup@ and choose @USB-Blaster@;
- refer to the user manual of debug board:
<http://www.terasic.com.tw/cgi-bin/page/archive.pl?Language=English&CategoryNo=165&No=593&PartNo=4>.

Make sure that dip switches position match to one shown on the diagram above.

Now prepare to work the controlling contoller. For do this you need:

1. Set up the wi-fi network and connect the controller to it accordance with the instructions
	<https://developer.electricimp.com/gettingstarted/explorer/blinkup>.
2. In case you use @aleksandpenskoi@ account, in @SPI_testbench@ line choose 
	@Development Zone@, and after @Code@.
3. Make sure that the last called function in the right part of screen (Device code) is 
	function with name similar to demo.
4. You will see the connection devices list in the left side of footer. Press the On/Off button opposite to 
	the needed device.
5. The data transfer journal should appear in the bottom right side of terminal. 
It should look like this:

@
2018-08-20 16:19:27 +03:00 	[Status] 	Agent restarted: reload.
2018-08-20 16:19:28 +03:00 	[Status] 	Device connected
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 00 00 00 00 00
2018-08-20 16:19:28 +03:00 	[Device] 	> Number: 0	Value: 0
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 01 00 00 00 01
2018-08-20 16:19:28 +03:00 	[Device] 	> Number: 1	Value: 1
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 01 00 00 00 02
2018-08-20 16:19:28 +03:00 	[Device] 	> Number: 2	Value: 1
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 02 00 00 00 03
2018-08-20 16:19:28 +03:00 	[Device] 	> Number: 3	Value: 2
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 03 00 00 00 04
2018-08-20 16:19:28 +03:00 	[Device] 	> Number: 4	Value: 3
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 05 00 00 00 05
2018-08-20 16:19:28 +03:00 	[Device] 	> Number: 5	Value: 5
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 08 00 00 00 06
2018-08-20 16:19:28 +03:00 	[Device] 	> Number: 6	Value: 8
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 0d 00 00 00 07
2018-08-20 16:19:28 +03:00 	[Device] 	> Number: 7	Value: 13
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 15 00 00 00 08
2018-08-20 16:19:28 +03:00 	[Device] 	> Number: 8	Value: 21
2018-08-20 16:19:28 +03:00 	[Device] 	binary: 00 00 00 22 00 00 00 09
2018-08-20 16:19:28 +03:00 	[Device] 	> Number: 9	Value: 34
@

The work of another demos will look like simirarly.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Demo
    ( -- * Demo
      fibonacciDemo
    , teacupDemo
      -- * Algorhitms descriptions
    , fibonacciAlg
    , teacupAlg, teacupLua, teacupAlg2
      -- * Processors description
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


-- FIXME: Nowadays while testing the bench rst signal does not cause computer reset to the start condition.

-- TODO: You must be able to specify exact frequency of the target computer. This task is related with the task about target platform.
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
    , ("div", PU (D.divider 4 True) D.PUPorts{ D.wr=Signal 27, D.wrSel=Signal 28, D.oe=Signal 29, D.oeSel=Signal 30 } )
    ]

-- |The Fibonacci sequency calcutation is the one of classical examples of algoritms: 0, 1, 2, 3, 5, 8, 13...|
--
-- Program that described below realise 2 independent processes:
-- - Fibonacci sequency calcutation;
-- - Integers calculation (element's number).
--
-- Every element of the sequences sends to the external interface, definable by the processor configuration. 
-- It is SPI Interface in this example ('NITTA.ProcessUnit.SPI').

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


-- |This is the classical example of system dinamic. The model description presented here:
-- <https://pysd-cookbook.readthedocs.io/en/latest/analyses/getting_started/Hello_World_Teacup.html>.
-- Calculations are performed in fixed-point numbers. Decimals are not converted.
--
-- Model's outputs:
--
-- - cups temperature;
-- - time from experiment beginning.
--
-- Every element of the sequences sends to the external interface, definable by the processor configuration. 
-- It is SPI Interface in this example ('NITTA.ProcessUnit.SPI').
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

























