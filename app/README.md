# NITTA CAD

## Before you start
- [clone](../doc/sourcetree-install.md) or download project from the site;
- install [The Haskell Tool Stack](../doc/stack-install.md);
- install [Icarus Verilog](../doc/hdl-install.md);
- install [npm](../web/README.md).

## Building

To build the project you should execute:
> stack build

You can add the following command line options:
- `--fast` for disabling optimization, which significantly reduces compile time;
- `--haddock` for generation project documentation.

To build UI static files you should execute:

> cd web

> npm run-script build

or (from the project path):

> stack exec nitta -- --npm-build

## Testing

To test project ([test spefication](../test/Spec.hs)):
> stack test

To test and build documentation (you should run this before all commits):
> stack build --test --haddock

## Running

To execute without UI:

> stack exec nitta -- FILE_NAME

where FILE_NAME is a `.lua` file with a specific algorithm. After that, you can find a target project in `hdl/gen/main` path.

Options:
- `--web` - execute UI;
- `--npm-build` - generate web UI static files;
- `--type` - specify bus data type. `fxm.b`, where `m` - the number of magnitude or integer bits, `b` the total number of bits. Default value - `fx32.32`.

To execute with UI:

> stack exec nitta -- --web FILE_NAME

where FILE_NAME is a `.lua` file with a specific algorithm. 

To open UI in a web browser you should go to <http://localhost:8080/index.html>. From the UI you can synthesis target system and generate a target project in `hdl/gen/web_ui` path by the `testbench` button.

## Running for UI development
> cd web

> npm start

# Demos

To execute the demo:
1. Build NITTA project by `stack build` from the project path.
2. Generate target system demo project by `stack exec nitta --
   $FILE_NAME` from the project path.
3. Execute the target system demo project on a hardware test bench.

Further, you can find some demo projects.

## Fibonacci

The Fibonacci sequence calculation is one of the classical examples of algorithms: 0, 1, 2, 3, 5, 8, 13...

[The program](../examples/fibonacci.lua) that realize 2 independent processes:

- fibonacci sequence calculation;
- natural counter.

Every element of the sequences sends to the external interface, definable by the processor configuration. With the default processor microarchitecture, it is SPI ('NITTA.ProcessUnit.SPI').

Generate command: 
> `stack exec nitta -- --type=fx24.32 --web examples/fibonacci.lua`

`--type=fx24.32` option is necessary because, without this, the modeling process can't progress.

## Teacup

This is a classic example of the system dynamic model. The model description presented here: <https://pysd-cookbook.readthedocs.io/en/latest/analyses/getting_started/Hello_World_Teacup.html>. Calculations are performed in fixed-point numbers. Decimals are not converted.

Model's outputs:

- cup temperature;
- time from experiment beginning.

Every element of the sequences sends to the external interface, definable by the processor configuration. With the default processor microarchitecture, it is SPI ('NITTA.ProcessUnit.SPI').

Generate command: 
> `stack exec nitta -- --type=fx24.32 --web examples/teacup.lua`

# Hardware test bench

For running prject on a real hardware you can follow :

- [Raspberry Pi 3](RaspberryPi3.md);
- [DE0nano + Electric Imp imp001](testbench_DE0nano_imp001.md).
