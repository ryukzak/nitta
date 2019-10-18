# Content
  1. Building and testing
  2. Makefile commands reference

# Building and testing


To build project you can run:
> stack build --fast

Option `--fast` disable compiler optimisation, which significantly reduce compile time.

To execute without UI (<app/Main.hs>):
> stack exec nitta

To execute with UI (<app/Main.hs>):
> stack exec nitta -- --web examples/fibonacci.lua

For repeated execution add `--no-static-gen` option, it should significantly reduce run time.

To open UI in web browser you should go to: <http://localhost:8080/index.html>

To test (<test/Spec.hs>):
> stack test --fast

```
nitta-0.0.0.1: unregistering (dependencies changed)
nitta-0.0.0.1: configure (lib + exe + test)
Configuring nitta-0.0.0.1...
clang: warning: argument unused during compilation: '-nopie' [-Wunused-command-line-argument]
nitta-0.0.0.1: build (lib + exe + test)
Preprocessing library for nitta-0.0.0.1..
Building library for nitta-0.0.0.1..
Preprocessing executable 'nitta' for nitta-0.0.0.1..
Building executable 'nitta' for nitta-0.0.0.1..
Preprocessing test suite 'nitta-test' for nitta-0.0.0.1..
Building test suite 'nitta-test' for nitta-0.0.0.1..
[1 of 6] Compiling NITTA.Test.BusNetwork ( test/NITTA/Test/BusNetwork.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/nitta-test/nitta-test-tmp/NITTA/Test/BusNetwork.o )
[2 of 6] Compiling NITTA.Test.Functions ( test/NITTA/Test/Functions.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/nitta-test/nitta-test-tmp/NITTA/Test/Functions.o )
[3 of 6] Compiling NITTA.Test.ProcessUnits ( test/NITTA/Test/ProcessUnits.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/nitta-test/nitta-test-tmp/NITTA/Test/ProcessUnits.o )
[4 of 6] Compiling NITTA.Test.ProcessUnits.Fram ( test/NITTA/Test/ProcessUnits/Fram.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/nitta-test/nitta-test-tmp/NITTA/Test/ProcessUnits/Fram.o )
[5 of 6] Compiling NITTA.Test.Utils ( test/NITTA/Test/Utils.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/nitta-test/nitta-test-tmp/NITTA/Test/Utils.o )
[6 of 6] Compiling Main             ( test/Spec.hs, .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/nitta-test/nitta-test-tmp/Main.o )
Linking .stack-work/dist/x86_64-osx/Cabal-2.0.1.0/build/nitta-test/nitta-test ...
clang: warning: argument unused during compilation: '-nopie' [-Wunused-command-line-argument]
clang: warning: argument unused during compilation: '-nopie' [-Wunused-command-line-argument]
nitta-0.0.0.1: copy/register
Installing library in /Users/penskoi/Documents/nitta/.stack-work/install/x86_64-osx/lts-11.20/8.2.2/lib/x86_64-osx-ghc-8.2.2/nitta-0.0.0.1-9JFN627U1UPYwkLYaKUqX
Installing executable nitta in /Users/penskoi/Documents/nitta/.stack-work/install/x86_64-osx/lts-11.20/8.2.2/bin
Registering library for nitta-0.0.0.1..
nitta-0.0.0.1: test (suite: nitta-test)
                           
Progress 1/2: nitta-0.0.0.1NITTA
  Fram process unit
    framRegAndOut:                  OK (0.24s)
    framRegAndConstant:             OK (0.21s)
    completeness:                   OK (1.89s)
      +++ OK, passed 10 tests.
    Fram simulation:                OK (2.01s)
      +++ OK, passed 10 tests.
  Multiply process unit
    completeness:                   OK (0.44s)
      +++ OK, passed 10 tests.
    simulation:                     OK (2.27s)
      +++ OK, passed 10 tests.
  Divider process unit
    lua_divider_test_1:             OK (0.49s)
    lua_divider_test_2:             OK (1.48s)
  Function
    reorderAlgorithm:               OK
    fibonacci:                      OK
  BusNetwork
    testShiftAndFram:               OK (0.43s)
    testAccumAndFram:               OK (1.13s)
    testMultiplier:                 OK (1.12s)
    testDiv4:                       OK (1.75s)
    testFibonacci:                  OK (0.30s)
    testFibonacciWithSPI:           OK (0.68s)
  Utils
    values2dump:                    OK
    inputsOfFBs:                    OK
    outputsOfFBsTests:              OK
    endpointRoleEq:                 OK
  lua frontend
    lua_counter_void_function:      OK (0.75s)
    lua_counter_local_var:          OK (0.72s)
    lua_counter_function:           OK (1.43s)
    lua_fibonacci_a_b:              OK (0.65s)
    lua_fibonacci_b_a:              OK (0.65s)
    lua_fibonacci_nested_fun_call1: OK (1.30s)
    lua_fibonacci_nested_fun_call2: OK (1.05s)
    lua_teacup:                     OK (1.16s)

All 28 tests passed (6.04s)
                           
nitta-0.0.0.1: Test suite nitta-test passed
Completed 2 action(s).     
```

To build, test and documentation generation:
> stack build --fast --test --haddock

# Makefile commands reference

You will need `make` to use this functional. Make will run `stack.build stack.gen npm.build` if you run make without any option. To build Web UI make sure that you have installed all dependencies from `package.json`.

## General options:
- `dev` [`sim`]

  This option will built and run stack project with `--web` argument, generate files required for Web UI and run Web UI project via webpack-dev-server. The `sim` argument using for choosing which example you will run in stack project, by default are used `fibonacci`. To see available options you could open `examples` folder in the root of the project.

  Example usage:
    `make dev sim=fibonacci`

  WARNING: This option runs stack project in the background, so if you will run this option multiple times you would have N count of processes running in the background. Use `stack.kill` option to kill this processes.

  Commands:
  - `stack build`
  - `stack exec nitta-api-gen`
  - `stack exec nitta -- --web examples/fibonacci.lua` (By default)
  - `cd web && npm run start-dev`

- `prod`

  This option will built stack project, generate documentation, run tests and also will build Web UI.

  Commands:
  - `stack build --test --haddock --copy-bins`
  - `stack exec nitta-api-gen`
  - `cd web && npm run build`


- `build`

  This option will built stack project and generate documentation.

  Commands:
  - `stack build  --haddock`

- `clean`

  This option will clean result directories and remove all running apps that are runned in the background.

  Commands:
  - `pkill -f nitta`
  - `rm -rf .stack-work`
  - `rm -rf web/build`

## Advanced options

  More information you can see in Makefile

- `npm.build`
- `npm.dev`
- `stack.prod`
- `stack.build`
- `stack.buildNdocs`
- `stack.gen`
- `stack.web`
- `stack.kill`

