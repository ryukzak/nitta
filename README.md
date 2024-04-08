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

## Set up development environment

You have at least two options here:

1. Set it up automatically as a docker container using this [manual](/ml/synthesis/README.md) and develop in VS Code.
2. Set it up locally by hand. May be less problematic in some cases.

In this document, we will focus on the local setup.

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

### WSL2

Install [GHCUP](https://www.haskell.org/ghcup/).

``` console
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

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

### For machine learning base synthesis (optional)

1. Install python package manager: [poetry](https://python-poetry.org/docs/).
2. Install python dependencies: `cd ml/synthesis; poetry install`

## How to Build, Test, Format, Lint & Run

see [Makefile](Makefile) as a source of up-to-date command examples. Most general command are represnted as a make targets.

### Configuration

- `HS_SRC_DIR`: Defines the source code directories for Haskell.
- `HS_O`: Specifies options for Haskell compilation.
- `POETRYPATH`: Defines the path to the Poetry configuration.
- `PYTHONPATH`: Specifies the Python path for Poetry.
- `POETRY`: Command to run Poetry within the specified configuration.
- `PYTHON`: Command to run Python using Poetry.
- `ML_CRAWL_DATA_PATH`: Path to store data sets for ML training.
- `ML_MODEL_PATH`: Path to the machine learning model directory.
- `ML_MODEL`: Identifies the latest machine learning model in the `ML_MODEL_PATH`.

### nitta (the core of the tool)

- `build`: Builds the Haskell project using Stack with various options with haddock docs.
- `build-prod`: Builds the Haskell project with optimized GHC options.
- `test`: Builds and runs Haskell tests with coverage.
- `format`: Formats Haskell source code using Fourmolu.
- `format-check`: Checks Haskell source code formatting.
- `lint`: Lints Haskell code using HLint and Weeder.
- `clean`: Cleans the Haskell project with Stack.
- `benchmark`: Runs a benchmark for the Haskell project.
- `benchmark-report`: Compares and generates a benchmark report.

### UI (to see what actually happening due to synthesis process)

- `ui-build`: Builds the UI component using Yarn.
- `ui-run`: Runs the UI component using Yarn.
- `ui-format`: Formats UI source code using Prettier.
- `ui-format-check`: Checks UI source code formatting.

### Machine Learning based Synthesis

- `ml-crawl-data`: Crawls data for the machine learning model.
- `ml-train-model`: Trains the machine learning model.
- `ml-format`: Formats machine learning code using Black.
- `ml-format-check`: Checks machine learning code formatting.
- `ml-lint`: Lints machine learning code using Ruff, MyPy, and Vulture.
- `ml-nitta`: Runs the machine learning model with Nitta.

### Docker image for development

see details in: <./ml/synthesis/README.md>

- `docker-dev-build`: make dev image for your platform (~15 GB).
- `docker-dev-build-with-gpu`: make dev image for your platform with GPU support (~25 GB).
- `docker-dev-run`: run dev image without GPU support.
- `docker-dev-run-with-gpu`: run dev image with GPU support.

## CLI Hints

``` console
# Build nitta
$ stack build --fast


# Build frontend
$ stack exec nitta-api-gen
$ yarn --cwd web install
$ yarn --cwd web run build


# Run specific test
$ stack test --test-arguments '-p "pattern for the test name"'

# Run only specific tests in one thread
stack build --test --fast --ta '-p "Divider PU" --num-threads 1'

# Rerun only failed test, if all test passed - run all test
$ stack build nitta:nitta-test --fast --test --test-arguments --rerun

# Run doctest for all files
$ find src -name '*.hs' -exec grep -l '>>>' {} \; | xargs -t -L 1 -P 4 stack exec doctest

# generate haddock for tests
stack exec ghc-pkg unregister interpolate -- --force
stack exec -- haddock test/**/*.hs -odocs -h


# Build only one target and run with UI
$ stack build nitta:nitta --fast && stack exec nitta -- -p=8080 -t=fx32.32 examples/pid.lua

# Run UI without synthesis
$ stack exec nitta -- examples/teacup.lua -p=8080 --method=nosynthesis


# Show profiler report
$ stack build --fast --profile && stack exec nitta --profile -- -t=fx32.32 examples/counter.lua +RTS -p && cat nitta.prof


# Show stack trace if application raise an error
$ stack build --fast --profile && stack exec nitta --profile -- -t=fx32.32 examples/counter.lua +RTS -xc


# Show modules dependency
$ graphmod -q -p src | pbcopy
```

## Usage

``` console
$ stack exec nitta -- --help
nitta v0.0.0.1 - tool for hard real-time CGRA processors

nitta [OPTIONS] FILE

Target system configuration:
         --uarch=PATH                   Microarchitecture configuration file
  -a     --auto-uarch                   Use empty microarchitecture and
                                        allocate PUs during synthesis process.
  -t     --type=fxM.B                   Overrides data type specified in
                                        config file
         --io-sync=sync|async|onboard   Overrides IO synchronization mode
                                        specified in config file
         --templates=PATH[:PATH]        Target platform templates (default:
                                        'templates/Icarus:templates/DE0-Nano')
         --frontend-language=Lua|XMILE  Language used to source algorithm
                                        description. (default: decision by file
                                        extension)
Common flags:
  -p     --port=INT                     Run nitta server for UI on specific
                                        port (by default - not run)
  -o     --output-path=PATH             Target system path
Simulation:
  -n=INT                                Number of simulation cycles
  -f     --fsim                         Functional simulation with trace
  -l     --lsim                         Logical (HDL) simulation with trace
         --format=md|json|csv           Simulation output format (default:
                                        'md')
Other:
  -v     --verbose                      Verbose
  -e     --extra-verbose                Extra verbose
  -?     --help                         Display help message
  -V     --version                      Print version information
         --numeric-version              Print just the version number
Synthesis:
  -s     --score=NAME                   Name of the synthesis tree node score
                                        to additionally evaluate. Can be
                                        included multiple times (-s score1 -s
                                        score2). Scores like ml_<model_name>
                                        will enable ML scoring.
  -d     --depth-base=FLOAT             Only for 'TopDownByScore' synthesis:
                                        a [1; +inf) value to be an exponential
                                        base of the depth priority coefficient
                                        (default: 1.4)
  -m     --method=NAME                  Synthesis method
                                        (stateoftheart|topdownbyscore|nosynthesis,
                                        default: stateoftheart). `nosynthesis`
                                        required to run UI without synthesis on
                                        the start.                                        
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
