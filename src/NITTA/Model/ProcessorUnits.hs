{- |
Module      : NITTA.Model.ProcessorUnits
Description : Library of processor unit models
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

A Processor Unit is an element of a NITTA processor that performs data
processing, storing, and transferring to and from external devices.

= Model of a processor unit

Model of a processor unit includes the following things:

- the data type for representation process unit model state, which describes all
  aspects of its computational process;

- binding and process description (see 'ProcessorUnit' from
  "NITTA.Model.ProcessorUnits.Time");

- sending and receiving data by the processor unit (see
  "NITTA.Model.Problems.Endpoint");

- instructions and control signals (see 'Controllable', 'UnambiguouslyDecode',
  and 'Connected' from "NITTA.Model.ProcessorUnits.Time");

- refactoring (see "NITTA.Model.Problems.Refactor");

- variables casuality (see 'Lock' from "NITTA.Intermediate.Types");

- simulation (see 'Simulatable' from "NITTA.Model.ProcessorUnits.Time");

- how to generate hardware with this processor unit ('TargetSystemComponent'
  from "NITTA.Project.Implementation");

- how to test process unit (see 'Testable' from
  "NITTA.Project.TestBench");

- IO signals and how to test IO if applicable (see 'IOConnected' from
  'NITTA.Model.ProcessorUnits.Time' and 'IOTestBench' from
  "NITTA.Project.TestBench");

- and some technical stuff for it.

For a well-documented example, see "NITTA.Model.ProcessorUnits.Multiplier".

= HARDWARE INTERFACE of a processor unit

== System signals

- `clk` - clock signal;
- `rst` - reset signal;
- `signal_cycle`x - start of computational cycle (require for synchronization
  reason).

== Control signals

Used for control over a processor unit. A specific set of control signals
defined for each process unit individually. Usually, they include:

- `signal_wr` - the signal for reading data from `data_in`;
- `signal_oe` - the signal for writing data to `data_out`.

== Databus

These ports are used for transferring data between process units.

Parameters:

- `DATA_WIDTH` - machine word width;
- `ATTR_WIDTH` - machine word attribute width;
- `INVALID_ATTR` (default: 0) - index of invalid flag on machine word attribute.

Input ports:

- `input [DATA_WIDTH-1:0] data_in` - port for reciving data by a process unit:
- `input [ATTR_WIDTH-1:0] attr_in` - port for receiving data attributes by a
  process unit;
- if `signal_wr` or similar are not set - value on data_in should be ingnored.

Output ports:

- `output [DATA_WIDTH-1:0] data_out` - port for sending data to a process unit;
- `output [ATTR_WIDTH-1:0] attr_out` - port for sending data attributes to a process unit;
- a processor unit can restrict the time when a value is available for reading;
- if `signal_oe` or similar are not set - value on `data_out` should be 0, time
  of `signal_oe` and actual data transfer may variate by the process unit
  developer:

    - with offset:

        @
        signal wave:

        oe          - 0001000
        dataout     - 0000*00
        @

    - without offset:

        @
        signal wave:

        oe          - 0001000
        dataout     - 000*000
        @

== External ports or IO ports

Purpose: transferring data from and to the NITTA processor (ports simple
forwarded across processor from environment to the processor unit).

Acceptable port types: input, output, and input.

The composition of ports is determined individually by the type of I/O
interface.
-}
module NITTA.Model.ProcessorUnits (
    module NITTA.Model.ProcessorUnits.Types,
    module NITTA.Model.Time,
    module NITTA.Model.ProcessorUnits.Accum,
    module NITTA.Model.ProcessorUnits.Broken,
    module NITTA.Model.ProcessorUnits.Divider,
    module NITTA.Model.ProcessorUnits.Fram,
    module NITTA.Model.ProcessorUnits.IO.SPI,
    module NITTA.Model.ProcessorUnits.Multiplier,
    module NITTA.Model.ProcessorUnits.Shift,
    module NITTA.Model.ProcessorUnits.LUT,
    module NITTA.Model.ProcessorUnits.Compare,
    module NITTA.Model.ProcessorUnits.Multiplexer,
) where

import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Time

import NITTA.Model.ProcessorUnits.Accum
import NITTA.Model.ProcessorUnits.Broken
import NITTA.Model.ProcessorUnits.Compare
import NITTA.Model.ProcessorUnits.Divider
import NITTA.Model.ProcessorUnits.Fram
import NITTA.Model.ProcessorUnits.IO.SPI
import NITTA.Model.ProcessorUnits.LUT
import NITTA.Model.ProcessorUnits.Multiplexer
import NITTA.Model.ProcessorUnits.Multiplier
import NITTA.Model.ProcessorUnits.Shift
