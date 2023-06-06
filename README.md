# NITTA -- Tool for Hard Real-Time CGRA Processors

[![NITTA Build and Test](https://github.com/ryukzak/nitta/actions/workflows/ci.yml/badge.svg)](https://github.com/ryukzak/nitta/actions/workflows/ci.yml)
[![Test Coverage](https://img.shields.io/badge/Test%20Coverage-hpc-blue)](https://ryukzak.github.io/nitta/hpc/hpc_index.html)
[![haddock](https://img.shields.io/badge/Doc-haddock-blue)](https://ryukzak.github.io/nitta/haddock/nitta/index.html)
[![REST API Doc](https://img.shields.io/badge/Doc-API-blue)](https://github.com/ryukzak/nitta/blob/gh-pages/rest-api/rest_api.markdown)

## Overview

Kind: research pet project.

Project status: early prototype.

We develop the tool for generating and programming specialized non-von Neumann CGRA processors used for cyclic execution of control and signal/data processing algorithms. These processors are based on the original Not Instruction Transport Triggered Architecture (NITTA). That allows us to:

- Provide high speed and parallel execution of irregular algorithms (where GPU is not applicable) in hard real-time (clock accuracy).
- Make the processor reconfigurable for different application domains.
- Provide a high-level language for application developers and fast compilation.

Our future users can resolve the following tasks:

- Development of embedded and cyber-physical systems.
- Hardware and software testing and rapid prototyping (HIL and PIL).
- Development of programmable accelerators and coprocessors.

## Links

Project page & Publications: <https://ryukzak.github.io/projects/nitta/>

Maintainer: Aleksandr Penskoi <aleksandr.penskoi@gmail.com>

Project chat (telegram): <https://t.me/joinchat/BC5sV1GY7ADnCuOsKP-bJw>

Project CI chat (telegram): <https://t.me/nitta_ci>

## Contributing

See: [CONTRIBUTING.md](CONTRIBUTING.md)

## Setup development environment

### Mac OS X

Install [Stack](https://github.com/commercialhaskell/stack) and required developer tools for Haskell.

``` console
brew install ghcup
ghcup install stack
```

> Make sure that PATH contains $HOME/.local/bin.
>
> Make sure that you have up to date version of hlint and fourmolu (as on CI)!

Install [icarus-verilog](https://github.com/steveicarus/iverilog/) and [gtkwave](https://github.com/gtkwave/gtkwave).

``` console
brew install icarus-verilog
brew tap homebrew/cask
brew cask install gtkwave
```

Install [npm](https://github.com/npm/cli) and required developer tools for UI.

``` console
brew install npm yarn
npm install --global tern prettier
```

### Ubuntu

Install [Stack](https://github.com/commercialhaskell/stack) and required developer tools for Haskell.

``` console
sudo apt-get install haskell-stack
stack install hlint fourmolu
```

> Make sure that PATH contains $HOME/.local/bin.
>
> Make sure that you have up to date version of hlint and fourmolu (as on CI)!

Install [icarus-verilog](https://github.com/steveicarus/iverilog/) and [gtkwave](https://github.com/gtkwave/gtkwave).

``` console
sudo apt-get install iverilog
sudo apt-get install gtkwave
```

Install [npm](https://github.com/npm/cli) and required developer tools for UI.

``` console
sudo apt-get install npm yarn
npm install --global tern prettier yarn
```

## Build

Inside the project path

### Build backend

``` console
stack build
```

### Build frontend

``` console
stack exec nitta-api-gen
yarn --cwd web install
yarn --cwd web run build
```

### Build documentation

``` console
stack build --haddock # for nitta CAD
stack exec nitta-api-gen # for REST API description
```

For the fast rebuild, the project adds `--fast` flag.

### Testing

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

Run specified test or group:

```shell
$ stack test --test-arguments '-p "pattern for the test name"'
...
```

### Testing algorithm performance

The `evaluation.py` script supports various command-line arguments that can be used to customize and control the evaluation process.

- `example_paths` (required): Paths to the example files, separated by spaces.
- `--nitta_args` (optional): Arguments passed to Nitta. Enter the arguments in the format `--nitta_args="<arguments>"`.
- `--help`: Prints help information about the available arguments.

examples using

```bash

python3 /scripts/evaluation.py examples/fibonacci.lua examples/counter.lua --nitta_args="--format=csv"


          duration  depth  evaluator_calls      time
fibonacci        5      8                9  0.970245
counter          5      8                9  5.217567


```

### Other

``` console
# build only one target
$ stack build nitta:nitta --fast && stack exec nitta -- -p=8080 -t=fx32.32 examples/pid.lua

# rerun only failed test, if all test passed - run all test
$ stack build nitta:nitta-test --fast --test --test-arguments --rerun

# run only specific tests in one thread
stack build --test --fast --ta '-p "Divider PU" --num-threads 1'

# show profiler report
$ stack build --fast --profile && stack exec nitta --profile -- -t=fx32.32 examples/counter.lua +RTS -p && cat nitta.prof

# show stack trace if application raise an error
$ stack build --fast --profile && stack exec nitta --profile -- -t=fx32.32 examples/counter.lua +RTS -xc

# run doctest for all files
$ find src -name '*.hs' -exec grep -l '>>>' {} \; | xargs -t -L 1 -P 4 stack exec doctest
# in case:
# src/NITTA/Model/ProcessorUnits/Multiplier.hs:311:1: error:
#     Ambiguous module name ‘Data.String.Interpolate’:
#       it was found in multiple packages:
#       interpolate-0.2.1 string-interpolate-0.3.1.0
#     |
# 311 | import Data.String.Interpolate
#     | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
$ stack exec ghc-pkg unregister interpolate -- --force

# run fourmolu for all files
$ find . -name '*.hs' | xargs fourmolu -m inplace

# show modules dependency
$ graphmod -q -p src | pbcopy

# generate haddock for tests
stack exec ghc-pkg unregister interpolate -- --force
stack exec -- haddock test/**/*.hs -odocs -h
```

## Usage

``` console
$ stack exec nitta -- --help
nitta v0.0.0.1 - tool for hard real-time CGRA processors

nitta [OPTIONS] FILE

Target system configuration:
         --uarch=PATH                  Microarchitecture configuration file
  -t     --type=fxM.B                  Overrides data type specified in
                                       config file
         --io-sync=sync|async|onboard  Overrides IO synchronization mode
                                       specified in config file
         --templates=PATH[:PATH]       Target platform templates (default:
                                       'templates/Icarus:templates/DE0-Nano')
Common flags:
  -p     --port=INT                    Run nitta server for UI on specific
                                       port (by default - not run)
  -o     --output-path=PATH            Target system path
Simulation:
  -n=INT                               Number of simulation cycles
  -f     --fsim                        Functional simulation with trace
  -l     --lsim                        Logical (HDL) simulation with trace
         --format=md|json|csv          Simulation output format (default:
                                       'md')
Other:
  -v     --verbose                     Verbose
  -e     --extra-verbose               Extra verbose
  -?     --help                        Display help message
  -V     --version                     Print version information
         --numeric-version             Print just the version number
```

### Logical simulation for a specific algorithm

``` console
$ stack exec nitta -- examples/teacup.lua --fsim --format=md -t=fx24.32
| Cycle  | temp_cup_1  | time_0  |
|:-------|:------------|:--------|
| 1      | 180.000     | 0.000   |
| 2      | 178.625     | 0.125   |
| 3      | 177.375     | 0.250   |
| 4      | 176.125     | 0.375   |
| 5      | 174.875     | 0.500   |
| 6      | 173.625     | 0.625   |
| 7      | 172.375     | 0.750   |
| 8      | 171.125     | 0.875   |
| 9      | 169.875     | 1.000   |
| 10     | 168.750     | 1.125   |
```

### Synthesis of a target system for a specific algorithm

``` console
$ stack exec nitta -- examples/teacup.lua -v --lsim -t=fx24.32
[NOTICE : NITTA] synthesis process...ok
[NOTICE : NITTA] write target project to: "gen/main"...ok
[NOTICE : NITTA] run testbench (gen/main)...ok
| Cycle  | temp_cup_1  | time_0  |
|:-------|:------------|:--------|
| 1      | 180.000     | 0.000   |
| 2      | 178.625     | 0.125   |
| 3      | 177.375     | 0.250   |
| 4      | 176.125     | 0.375   |
| 5      | 174.875     | 0.500   |
| 6      | 173.625     | 0.625   |
| 7      | 172.375     | 0.750   |
| 8      | 171.125     | 0.875   |
| 9      | 169.875     | 1.000   |
| 10     | 168.750     | 1.125   |
```

### Run with user interface

``` console
$ stack exec nitta -- examples/teacup.lua -p=8080
Running NITTA server at http://localhost:8080 ...
```
