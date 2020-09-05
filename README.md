# Reconfigurable real-time computational platform NITTA


## Overview

Project status: early prototype.

### Application area

- developing cyber-physical systems which are based on adaptive robust control algorithms and artificial intelligence with high requirements on latency and computational volume, power, and area consumption; 
- developing hardware programmable accelerators and co-processors;
- developing problem-oriented programmable ASIC; 
- developing dynamically reconfigurable IP-core and soft-core for FPGA.

### Project goals

- rapid prototyping of control and cyber-physical systems;
- hardware in the loop simulation;
- target system synthesis and its components;
- integration of the real-time system with non-real-time environment and automation of its interaction;
- developing of IP-core for embedded systems and systems on a chip.

### Key features

- the orientation on model-driven engineering and not on software engineering; 
- automation of most of the development stages, including algorithm and model design and functional simulation, prototyping and complex verification, complex automatization of cross-layer testing and synthesis and optimization of a target system;
- deeply computational platform reconfiguration on hardware, software and tool levels, transparency of CAD system workflow.

### Contact

Maintainer: Aleksandr Penskoi <aleksandr.penskoi@gmail.com>

Project chat (telegram):  <https://t.me/joinchat/BC5sV1GY7ADnCuOsKP-bJw>

Project CI chat (telegram): <https://t.me/nitta_ci>

### Papers

Papers about the project you can find here (English and Russian): <https://nitta.io/nitta-corp/docs>.


## Preparing

### Install dependency

#### Stack (build tool for Haskell)
##### Ubuntu
```sh
sudo apt-get install haskell-stack
stack upgrade --binary-only
```

Check: `PATH` should contain `$HOME/.local/bin`.

#### Icarus Verilog (Verilog simulation)
##### Ubuntu
```sh
sudo apt-get install iverilog
```

#### GTKWave (wave viewer Verilog VCD/EVCD)
##### Ubuntu
```sh
sudo apt-get install gtkwave
```

#### npm (Node Package Manager)
##### Ubuntu
```sh
sudo apt-get install npm
```


### Build project

Inside the project path:

##### Build back-end

``` console
$ stack build
Building all executables for 'nitta' once. After a successful build of all of them, only specified executables will be rebuilt.
nitta> configure (lib + exe)

...

Registering library for nitta-0.0.0.1..

```

##### Build front-end
``` console
$ stack exec nitta-api-gen

Create output directory...
Create output directory...OK
Generate rest_api.js library...
Generate rest_api.js library...OK
Generate typescript interface...
Generate typescript interface...OK

$ cd web

$ npm ci

 ...
 
 added 2070 packages in 168.926s

$ npm run-script build
 
 ...

 Compiled successfully.

$ cd ..

```

#### Build haddock:

``` sh
stack build --haddock
```

For the fast rebuild, the project adds `--fast` flag.


## Usage

``` console
$ stack exec nitta -- --help
nitta v0.0.0.1 - CAD for reconfigurable real-time ASIP

nitta [OPTIONS] FILE

Common flags:
  -t     --type=ITEM                  Data type (default: 'fx32.32')
  -i     --io-sync=IOSYNCHRONIZATION  IO synchronization mode: sync, async,
                                      onboard
  -p     --port=INT                   Run control panel on a specific port
                                      (by default - not run)
  -n=INT                              Number of computation cycles for
                                      simulation and testbench
  -f     --fsim                       Functional simulation with trace
  -l     --lsim                       Logical (HDL) simulation with trace
  -v     --verbose                    Verbose
  -?     --help                       Display help message
  -V     --version                    Print version information
```

#### Logical simulation for a specific algorithm:
``` console
$ stack exec nitta -- examples/teacup.lua -f -t=fx12.32
temp_cup_1 time_0
180.000    0.000 
178.625    0.125 
177.375    0.250 
176.125    0.375 
174.875    0.500 
173.625    0.625 
172.375    0.750 
171.125    0.875 
169.875    1.000 
168.750    1.125 
```

#### Synthesis a target system for a specific algorithm:
``` console
$ stack exec nitta -- examples/teacup.lua -v
read source code from: "examples/teacup.lua"...
read source code from: "examples/teacup.lua"...ok
will trace: 
TraceVar {tvFmt = "%.3f", tvVar = "temp_cup_1"}
TraceVar {tvFmt = "%.3f", tvVar = "time_0"}
synthesis process...
synthesis process...ok
write target project to: "gen/main"...
write target project to: "gen/main"...ok
run logical synthesis...
run logical simulation...ok
```

#### Run control panel:
``` console
$ stack exec nitta -- examples/teacup.lua -p=8080 
Running NITTA server at http://localhost:8080 
...
```

#### Testing
``` console
$ stack test
nitta-0.0.0.1: unregistering (local file changes: test/NITTA/Model/ProcessorUnits/Tests/Utils.hs)
nitta> configure (lib + exe + test)
Configuring nitta-0.0.0.1...
nitta> build (lib + exe + test)
Preprocessing library for nitta-0.0.0.1..
Building library for nitta-0.0.0.1..
Preprocessing executable 'nitta' for nitta-0.0.0.1..
Building executable 'nitta' for nitta-0.0.0.1..
Preprocessing executable 'nitta-api-gen' for nitta-0.0.0.1..
Building executable 'nitta-api-gen' for nitta-0.0.0.1..
Preprocessing test suite 'nitta-test' for nitta-0.0.0.1..
Building test suite 'nitta-test' for nitta-0.0.0.1..
[14 of 20] Compiling NITTA.Model.ProcessorUnits.Tests.Utils
[17 of 20] Compiling NITTA.Model.ProcessorUnits.Serial.Accum.Tests [TH]

...

    concatLinesWithSpaceWithoutBeforeLine:                       OK
  NITTA.Utils.Tests
    values2dump:                                                 OK
    endpoint role equality:                                      OK

All 127 tests passed (7.28s)

nitta> Test suite nitta-test passed
Completed 2 action(s).
```
