{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module Main where

import           Control.Applicative           ((<$>))
import           Data.Atomics.Counter          (newCounter)
import           Data.Default                  (def)
import           Demo
import           NITTA.Frontend
import           NITTA.Functions
import           NITTA.ProcessUnits.Divider
import           NITTA.ProcessUnits.Fram
import           NITTA.ProcessUnits.Multiplier
import           NITTA.Test.BusNetwork
import           NITTA.Test.Functions
import           NITTA.Test.ProcessUnits
import           NITTA.Test.ProcessUnits.Fram
import           NITTA.Test.Utils
import           NITTA.Types
import           System.Environment            (setEnv)
import           Test.Tasty                    (defaultMain, testGroup)
import           Test.Tasty.HUnit              (testCase)
import           Test.Tasty.QuickCheck         (Gen, arbitrary, testProperty)
import           Text.InterpolatedString.Perl6 (qq)


-- FIXME: Тестирование очень активно работает с диском. В связи с этим рационально положить папку
-- hdl/gen в ramfs. Это и ускорит тестирование, и сбережёт железо. Необходимо это сделать для Linux,
-- но код должен корректно запускаться на Windows / OS X.
main = do
    counter <- newCounter 0 -- Используется для того, что бы раскладывать файлы в разные папки при симуляции.
    -- FIXME: Сделать так, что бы при тестировании данная настройка могла определяться снаружи. А 10
    -- выставлялось только при быстром тестировании.
    setEnv "TASTY_QUICKCHECK_TESTS" "10"
    defaultMain $ testGroup "NITTA"
        [ testGroup "Fram process unit"
            [ testCase "framRegAndOut" framRegAndOut
            , testCase "framRegAndConstant" framRegAndConstant
            , testProperty "completeness" $ prop_completness <$> framGen
            , testProperty "Fram simulation" $ fmap (prop_simulation "prop_simulation_fram" counter) $ inputsGen =<< framGen
            ]
        ,  testGroup "Multiply process unit"
            [ testProperty "completeness" $ prop_completness <$> multiplierGen
            , testProperty "simulation" $ fmap (prop_simulation "prop_simulation_multiplier" counter) $ inputsGen =<< multiplierGen
            ]
        ,  testGroup "Divider process unit"
            [ processorTest "lua_divider_test_1" $ lua2functions
                [qq|function f(a)
                        a, _b = a / 2
                        f(a)
                    end
                    f(1024)|]
            , processorTest "lua_divider_test_2" $ lua2functions
                [qq|function f(a, b)
                        a, _ = a / 2
                        b, _ = b / 3
                        f(a, b)
                    end
                    f(1024, 1024)|]
            -- FIXME: Auto text can't work correctly, because processGen don't take into account the
            -- facts that some variables may go out.

            -- , testProperty "completeness" $ prop_completness <$> dividerGen
            -- , testProperty "simulation" $ fmap (prop_simulation "prop_simulation_divider" counter) $ inputsGen =<< dividerGen
            ]
        -- , testGroup "Shift process unit"
        --     [ testCase "shiftBiDirection" shiftBiDirection
        --     ]
        , testGroup "Function"
            [ testCase "reorderAlgorithm" reorderAlgorithmTest
            , testCase "fibonacci" simulateFibonacciTest
            ]
        , testGroup "BusNetwork"
            [ testCase "testShiftAndFram" testShiftAndFram
            , testCase "testAccumAndFram" testAccumAndFram
            , testCase "testMultiplier" testMultiplier
            , testCase "testDiv4" testDiv4
            , testCase "testFibonacci" testFibonacci
            , testCase "testFibonacciWithSPI" testFibonacciWithSPI
            ]
        , testGroup "Utils"
            [ testCase "values2dump" values2dumpTests
            , testCase "inputsOfFBs" inputsOfFBsTests
            , testCase "outputsOfFBsTests" outputsOfFBsTests
            , testCase "endpointRoleEq" endpointRoleEq
            ]
        , testGroup "lua frontend"
            [ processorTest "lua_counter_void_function" $ lua2functions
                [qq|function counter(i)
                        send(i)
                        i = i + 1
                        counter(i)
                    end
                    counter(0)
                |]
            , processorTest "lua_counter_local_var" $ lua2functions
                [qq|function counter(i)
                        local i2 = i + 1
                        counter(i2)
                    end
                    counter(0)
                |]
            , processorTest "lua_counter_function" $ lua2functions
                [qq|function counter(i)
                        i = reg(i + 1)
                        counter(i)
                    end
                    counter(0)
                |]
            , processorTest "lua_fibonacci_a_b" $ lua2functions
                [qq|function fib(a, b)
                        a, b = b, a + b
                        fib(a, b)
                    end
                    fib(0, 1)|]
            , processorTest "lua_fibonacci_b_a" $ lua2functions
                [qq|function fib(a, b)
                        b, a = a + b, b
                        fib(a, b)
                    end
                    fib(0, 1)|]
            , processorTest "lua_fibonacci_nested_fun_call1" $ lua2functions
                [qq|function fib(a, b)
                        a, b = b, reg(a + reg(b)) + 0
                        fib(a, b)
                    end
                    fib(0, 1)|]
            , processorTest "lua_fibonacci_nested_fun_call2" $ lua2functions
                [qq|function fib(a, b)
                        a, b = b, reg(a + reg(b + 0)) + 0
                        fib(a, b)
                    end
                    fib(0, 1)|]
            , processorTest "lua_teacup" $ lua2functions teacupLua
            ]
      ]

framGen = processGen (def :: (Fram String Int Int))
    [ F <$> (arbitrary :: Gen (Constant (Parcel String Int)))
    , F <$> (arbitrary :: Gen (FramInput (Parcel String Int)))
    , F <$> (arbitrary :: Gen (FramOutput (Parcel String Int)))
    , F <$> (arbitrary :: Gen (Loop (Parcel String Int)))
    , F <$> (arbitrary :: Gen (Reg (Parcel String Int)))
    ]

multiplierGen = processGen (multiplier True)
    [ F <$> (arbitrary :: Gen (Multiply (Parcel String Int)))
    ]

dividerGen = processGen (divider 4 True)
    [ F <$> (arbitrary :: Gen (Division (Parcel String Int)))
    ]
