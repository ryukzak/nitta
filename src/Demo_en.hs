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


































