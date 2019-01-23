#SPI - DEO-nano сonnection diagram.

There is recommended color of wire in the brackets. The proportions are broken.

```

           +---------------------------------------- GND     (black)
           |            +--------------------------- MOSI_lg (orange)
           |            |+-------------------------- MISO_lg (blue)
           |            ||+------------------------- SCLK_lg (yellow)          
           |            |||+------------------------ CS_lg   (white)
+----------|------------||||-----+
|    ooooo *oooo ooooo o****     |
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

# Software requirments:

The software that is required to run and set up the test bench:

1. Project [NITTA CAD] <https://nitta.io/nitta-corp/nitta>, this module realisation included.
2. [Haskell stack] <https://nitta.io/penskoi/nitta/src/master/doc/stack-install.md>. 
Its required for CAD set up and demo generation.
3. [Quartus Prime Lite Edition]<https://fpgasoftware.intel.com/?edition=lite> and Cyclone IV device
   support. It's needed for synthesis and uploading a demo project into DE0-nano evaluation board.

# Demo start up

You have collected and pluged in stand on your table, on your computer you have already installed 
all needed software and you have opened the terminal in suitable catalog. Furthern instructions 
is written for Wndows 10, but it can be used for Linux too. 

Each demo is a function, the execution of which will cause generation of project, that you 
can synthesize, upload to test bench and test perfomance. Its execution will cause generation 
of directory with te same name with project in directory @/hdl/gen/DEMO_NAME@.
To start working with the demo, you have to insert a function call into the function 'Main.main', 
or start from @stack build@.
```
>>> stack exec nitta
Demo project in hdl/gen/hardcode
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