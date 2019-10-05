# Reconfigurable real-time computational platform NITTA

*Read this in other languages: [English](README.md), [Russian](README.ru.md).*

## Application area

- developing cyber-physical systems which are based on adaptive robust control algorithms and
  artificial intelligence with high requirements on latency and computational volume, power and area
  consumption; 
- developing hardware programmable accelerators and co-processors;
- developing problem-oriented programmable ASIC; 
- developing dynamically reconfigurable IP-core and soft-core for FPGA.

## Project goals

- rapid prototyping of control and cyber-physical systems;
- hardware in the loop simulation;
- target system synthesis and its components;
- integration of the real-time system with nonreal-time environment and automation of its
  interaction;
- developing of IP-core for embedded systems and systems on a chip.

## Key features

- the orientation on model-driven engineering and not on software engineering; 
- automation of most of the development stages, including algorithm and model design and functional
  simulation, prototyping and complex verification, complex automatisation of cross-layer testing
  and synthesis and optimisation of a target system;
- deeply computational platform reconfiguration on hardware, software and tool levels, transparency
  of CAD system workflow.

Project status: early prototype.

# Newcomers

Meeting with the project should be started from [this readme](app/README.md). The
module include several examples and demo application of NITTA platform. It
should be enough to introduce to the project briefly.

Actual tasks you can find in the project [bug tracker](https://nitta.io/nitta-corp/nitta/issues).

## Setup project toolchain
1. [Setup Git & SourceTree on Windows](doc/sourcetree-install.md)
1. [The Haskell Tool Stack (build system)](doc/stack-install.md)
1. [Icarus Verilog (simulator)](doc/hdl-install.md)
1. [Visual Studio Code (editor)](doc/vscode-install.md)

## Project rules
1. [Abbreviation](doc/abbreviation.md)
1. [Recommendations](doc/rules.md)
1. [Process unit example (haddock)](src/NITTA/ProcessUnits/Multiplier.hs)
1. [Demos](app/README.md)

## Info
1. [Why haskell?](/doc/why-haskell.md)

# Papers
Papers about the project you can find here (English and Russian): <https://nitta.io/nitta-corp/docs>.

# Contact
Maintainer: Aleksandr Penskoi <aleksandr.penskoi@gmail.com>

Project chat (telegram):  <https://t.me/joinchat/BC5sV1GY7ADnCuOsKP-bJw>
Project CI chat (telegram): <https://t.me/nitta_ci>
