# Test bench organisation
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

```
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
```
#SPI - DEO-nano сonnection diagram.

There is recommended color of wire in the brackets. The proportions are broken.

```
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
```
SPI - imp001 connection diagram
```
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
```
# Software requirments:

The software that is required to run and set up the test bench:

1. Project [NITTA CAD] <https://nitta.io/nitta-corp/nitta>, this module realisation included.
2. [Haskell stack] <https://nitta.io/penskoi/nitta/src/master/doc/stack-install.md>. 
Its required for CAD set up and demo generation.
3. [Quartus Prime Lite Edition]<https://fpgasoftware.intel.com/?edition=lite> and Cyclone IV device
   support. It's needed for synthesis and uploading a demo project into DE0-nano evaluation board.

Also, you need an account on <https://impcentral.electricimp.com> for getting data from the NITTA
processor. You can manually register and creating the project for that purpose or request login and
password from the project maintainer.

# Demo start up

You have collected and pluged in stand on your table, on your computer you have already installed 
all needed software and you have opened the terminal in suitable catalog. Furthern instructions 
is written for Wndows 10, but it can be used for Linux too. 

Each demo is a function, the execution of which will cause generation of project, that you 
can synthesize, upload to test bench and test perfomance. Its execution will cause generation 
of directory with te same name with project in directory @/hdl/gen/DEMO_NAME@.
To start working with the demo, you have to insert a function call into the function 'Main.main', 
or start from @stack repl @.
```
>>> :m Demo
>>> fibonacciDemo
Demo project in hdl/gen/fibonacciDemo
```


Open generated project in @Quartus@ (@nitta.qpf@ file) and synthesize it: @Processing ->
Start Compilation@. After this you need to flash result in FPG. To do this you need @Tools -> Programmer@. In File field you need to write path to firmware nitta.sof, if you see <none> in field, you need to press the field twice and choose firmware by path output_files/nitta.sof, and after set checkbox Program/Configure to active. 
After press @Start@ button. If the button is deactiveted:

- check DE0-Nano board connection.
- press @Hardware Setup@ and choose @USB-Blaster@;
- refer to the user manual of debug board:
<http://www.terasic.com.tw/cgi-bin/page/
archive.pl?Language=English&CategoryNo=165&No=593&PartNo=4>.

Make sure that dip switches position match to one shown on the diagram above.

Now prepare to work the controller. For do this you need:

Instruction, if you use account @aleksandpenskoi@::

1. Set up the wi-fi network and connect the controller to it accordance with the instructions
	<https://developer.electricimp.com/gettingstarted/explorer/blinkup>.
2. In case you use @aleksandpenskoi@ account, in @SPI_testbench@ line choose 
	@Development Zone@, and after @Code@.
3. Make sure that the last called function in the right part of screen (Device code) is 
	function with name similar to demo.
4. You will see the connection devices list in the left side of footer. Press the On/Off button opposite to 
	the needed device.
5. The data transfer journal should appear in the bottom right side of terminal. 
It should look like you will see below. 

Instruction, if you use new account @new_account@:

1. Set up Wi-Fi network and connect controller as in instruction
 <https://developer.electricimp.com/gettingstarted/explorer/blinkup>.
2. Register account on electric imp site    <https://impcentral.electricimp.com>.
3. In the left part of menu choose @Development Device Group Devices@  Tab and press @Assign@ button. In Device Group item set Development Device Group and name of DDG, that you give while project creation, after press @Assign Devices@. Field with DEVICE ID must appear with online status. 
4. On the left in the menu go to @Code@ tab and in the Device Code field insert code below:

```
sc <- hardware.pin8;
sc.configure(DIGITAL_OUT, 1);

spi1 <- hardware.spi257;
spi1.configure( CLOCK_IDLE_LOW | CLOCK_2ND_EDGE, 1000 )

function echoTest_2bytes() {
    local tmp;
    local i = 0;   
    
    while ( i <= 0xFF) {
        local b = blob(2);
        b.writen( i, 'c' );
        b.writen( i + 1, 'c' );
        sc.write(0);
        tmp = spi1.writeread(b);
        sc.write(1);
        i += 2;
    }
}

function fibonacciTest(n) {
    local tmp;
    local i = 0;   
    
    while ( i < n ) {
        local b = blob(8);
        sc.write(0);
        tmp = spi1.writeread(b);
        sc.write(1);
        i += 1;
        server.log(tmp)
    }
}

function getTest(n, size) {
    local i = 0;   
    
    while ( i < n ) {
        local b = blob(size);
        sc.write(0);
        local tmp = spi1.writeread(b);
        sc.write(1);
        i += 1;

        server.log(tmp);
        tmp.swap4();
        local a = tmp.readn('i');
        local b = tmp.readn('i');
        server.log( format("> %d\t%d", a, b) );
    }
}

function fibonacciDemo(n) {
    local i = 0;   
    
    while ( i < n ) {
        local b = blob(8);
        sc.write(0);
        local tmp = spi1.writeread(b);
        sc.write(1);
        i += 1;

        server.log(tmp);
        tmp.swap4();
        local a = tmp.readn('i');
        local b = tmp.readn('i');
        server.log( format("> Номер: %d\tЗначение:%d", b, a) );
    }
}

function teacupDemo(n) {
    local i = 0;   
    
    while ( i < n ) {
        local b = blob(8);
        sc.write(0);
        local tmp = spi1.writeread(b);
        sc.write(1);
        i += 1;

        server.log(tmp);
        tmp.swap4();
        local a = tmp.readn('i');
        local b = tmp.readn('i');
        server.log( format("> Номер: %d\tЗначение:%d", b, a) );
    }
}

// getTest(20, 8)
// getTest(4, 12)
// getTest(4, 8)
// getTest(4, 12)
// getTest(10, 4)

// echoTest_2bytes()
// echoTest_4bytes()

// fibonacciTest(10)

fibonacciDemo(10)
```

5. The data transfer journal should appear in the bottom right side of terminal. 
It should look like this:

```
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
```
The work of another demos will look like simirarly.





