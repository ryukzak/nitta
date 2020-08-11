# Examples

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
