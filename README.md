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

### Publication

Papers about the project you can find here (English and Russian): <https://nitta.io/nitta-corp/docs>.

## Install development dependency

### Mac OS X
``` console
# The Haskell Tool Stack
$ brew install stack
$ stack install hlint stylish-haskell
$ echo PATH should contain $HOME/.local/bin

# Verilog tools
$ brew install icarus-verilog
$ brew tap homebrew/cask
$ brew cask install gtkwave

# Frontend tools
$ brew install npm
$ npm install --global tern prettier
```

### Ubuntu
``` console
# The Haskell Tool Stack
$ sudo apt-get install haskell-stack
$ stack install hlint stylish-haskell
$ echo PATH should contain $HOME/.local/bin

# Verilog tools
$ sudo apt-get install iverilog
$ sudo apt-get install gtkwave

# Frontend tools
$ sudo apt-get install npm
$ npm install --global tern prettier yarn
```

## Build

Inside the project path:

### Build backend

``` console
$ stack build
Building all executables for 'nitta' once. After a successful build of all of them, only specified executables will be rebuilt.
nitta> configure (lib + exe)

...

Registering library for nitta-0.0.0.1..

```

### Build frontend
``` console
$ stack exec nitta-api-gen
Expected nitta server port: 8080
Create output directory...
Create output directory...OK
Generate rest_api.js library...
Generate rest_api.js library...OK
Generate typescript interface...
Generate typescript interface...OK
$ yarn --cwd web install
yarn install v1.22.10
[1/4] 🔍  Resolving packages...
success Already up-to-date.
✨  Done in 0.61s.
$ yarn --cwd web run build
yarn run v1.22.10
$ react-scripts --max_old_space_size=4096 build
Creating an optimized production build...
Compiled successfully.
...
✨  Done in 63.36s.
```

### Build haddock:
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
  -p     --port=INT                   Run nitta server for UI on specific
                                      port (by default - not run)
  -n=INT                              Number of computation cycles for
                                      simulation and testbench
  -f     --fsim                       Functional simulation with trace
  -l     --lsim                       Logical (HDL) simulation with trace
  -v     --verbose                    Verbose
  -?     --help                       Display help message
  -V     --version                    Print version information
         --numeric-version            Print just the version number
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

#### Run with user interface:
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

  NITTA.Utils.Tests
    values2dump:                                                 OK
    endpoint role equality:                                      OK

All 138 tests passed (8.81s)

nitta               > Test suite nitta-test passed
Completed 22 action(s).
```
