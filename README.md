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

Install [Stack](https://github.com/commercialhaskell/stack) and required dependencies.

``` console
$ brew install stack
$ stack install hlint stylish-haskell
```
> Make sure that PATH contains $HOME/.local/bin.

Install [icarus-verilog](https://github.com/steveicarus/iverilog/) and [gtkwave](https://github.com/gtkwave/gtkwave).
``` console
$ brew install icarus-verilog
$ brew tap homebrew/cask
$ brew cask install gtkwave
```

Install [npm](https://github.com/npm/cli) and required dependencies.
``` console
$ brew install npm
$ npm install --global tern prettier
```

### Ubuntu
Install [Stack](https://github.com/commercialhaskell/stack) and required dependencies.
``` console
$ sudo apt-get install haskell-stack
$ stack install hlint stylish-haskell
```
> Make sure that PATH contains $HOME/.local/bin.

Install [icarus-verilog](https://github.com/steveicarus/iverilog/) and [gtkwave](https://github.com/gtkwave/gtkwave).
``` console
$ sudo apt-get install iverilog
$ sudo apt-get install gtkwave
```

Install [npm](https://github.com/npm/cli) and required dependencies.
``` console
$ sudo apt-get install npm
$ npm install --global tern prettier yarn
```

## Build

Inside the project path:

### Build backend

``` console
$ stack build
```

### Build frontend
``` console
$ stack exec nitta-api-gen
$ yarn --cwd web install
$ yarn --cwd web run build
```

### Build documentation
``` console
$ stack build --haddock # for nitta CAD
$ stack exec nitta-api-gen # for REST API description
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
$ stack build --test
nitta-0.0.0.1: unregistering (dependencies changed)
nitta> configure (lib + exe + test)
Configuring nitta-0.0.0.1...
nitta> build (lib + exe + test)
Preprocessing library for nitta-0.0.0.1..
Building library for nitta-0.0.0.1..
[ 1 of 56] Compiling NITTA.Intermediate.Value
[ 2 of 56] Compiling NITTA.Intermediate.Variable
...
  NITTA.Utils.Tests
    values2dump:                                                 OK
    endpoint role equality:                                      OK

All 190 tests passed (27.89s)

nitta> Test suite nitta-test passed
Completed 2 action(s).
```
