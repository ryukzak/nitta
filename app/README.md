# NITTA CAD


## Before you start
- get the project from repository by `git` or by [site](https://nitta.io/nitta-corp/nitta);
- install [The Haskell Tool Stack](../doc/stack-install.md);
- install [Icarus Verilog](../doc/hdl-install.md);
- install [npm](https://www.npmjs.com/get-npm).


## Building

### Building CAD

To build the project you should execute:
> stack build

You can add the following command line options:
- `--fast` for disabling optimization, which significantly reduces compile time;
- `--haddock` for generation project documentation.

That command build the following executables:
- `:nitta` -- CAD as is;
- `:nitta-api-gen` -- service files generator for interaction between CAD and UI;
- `:nitta-test` -- unit and property tests.

You can build only needed by specific: `:nitta`, which can significantly reduce build time.

### Building UI

To build UI static files you should execute:

For preparing REST API typescript interfaces and request functions you should execute in the project path:
> stack exec nitta-api-gen

Change workdir to `web`:
> cd web

Install dependency:
> npm install

To build UI static files you should execute:

> npm run-script build


## Testing

To test project ([test spefication](../test/Spec.hs)):
> stack test

To test and build documentation (you should run this before all commits):
> stack build --test --haddock


## Execution

To execute without UI:

> stack exec nitta -- FILE_NAME

where FILE_NAME is a `.lua` file with a specific algorithm. After that, you can find a target project in `gen/main` path.

Options:
- `--web` - execute UI;
- `--type` - specify bus data type. `fxM.B`, where `M` - the number of magnitude or integer bits, `B` the total number of bits. Default value - `fx32.32`.

To execute with UI:

> stack exec nitta -- FILE_NAME --web

where FILE_NAME is a `.lua` file with a specific algorithm. 

To open UI in a web browser you should go to <http://localhost:8080/index.html>. From the UI you can synthesis target system and generate a target project in `hdl/gen/web_ui` path by the `testbench` button.


## Running for UI development

> cd web
> npm run start


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

Test bench was designed with the following components:

1.  The tool computer (imp001). It provides control of a
    modelling process and data transmission. There is using a single-board computer by Electric imp.
2.  A NITTA processor. It evaluates system dynamics model in the real-time and provides modelling
    data by its interfaces as inputs for a system under test. For realising this component DE-0 Nano
    board with Cyclone IV FPGA have been used.

The controller and the NITTA processor communicate throughout SPI (for data transmission)
interfaces. Those interfaces were chosen because they are ubiquitous.

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

For running prject on a real hardware you can follow :

- [DE0nano](DE0nano.md);
- [Electric Imp imp001](imp001.md);
- [Raspberry Pi 3](RaspberryPi3.md).
