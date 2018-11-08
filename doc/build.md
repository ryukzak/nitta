# Building and testing
Before start, you should:
- [clone](doc/sourcetree-install.md) or download project from repositories;
- install [The Haskell Tool Stack](doc/stack-install.md);
- install [Icarus Verilog](doc/hdl-install.md).
- change current directory to the project path.

To build project you can run:
> stack build --fast

Option `--fast` disable compiler optimisation, which significantly reduce compile time.

To execute without UI (<app/Main.hs>):
> stack exec nitta

To execute with UI (<app/Main.hs>):
> stack exec nitta -- --web

For repeated execution add `--no-static-gen` option, it should significantly reduce run time.

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
