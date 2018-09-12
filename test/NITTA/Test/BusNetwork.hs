{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Test.BusNetwork where

import           Data.Default
import           NITTA.BusNetwork
import           NITTA.Compiler
import qualified NITTA.Functions               as F
import qualified NITTA.ProcessUnits.Accum      as A
import qualified NITTA.ProcessUnits.Divider    as D
import qualified NITTA.ProcessUnits.Fram       as FR
import qualified NITTA.ProcessUnits.Multiplier as M
import qualified NITTA.ProcessUnits.Shift      as S
import qualified NITTA.ProcessUnits.SPI        as SPI
import           NITTA.Project
import           NITTA.Types
import           System.FilePath.Posix         (joinPath)
import           Test.Tasty.HUnit


netWithArithmAndSPI = busNetwork 27 (Just True)
  [ InputPort "mosi", InputPort "sclk", InputPort "cs" ]
  [ OutputPort "miso" ]
  [ ("fram1", PU def FR.PUPorts{ FR.oe=Signal 11, FR.wr=Signal 10, FR.addr=map Signal [9, 8, 7, 6] } )
  , ("fram2", PU def FR.PUPorts{ FR.oe=Signal 5, FR.wr=Signal 4, FR.addr=map Signal [3, 2, 1, 0] } )
  , ("shift", PU def S.PUPorts{ S.work=Signal 12, S.direction=Signal 13, S.mode=Signal 14, S.step=Signal 15, S.init=Signal 16, S.oe=Signal 17 })
  , ("accum", PU def A.PUPorts{ A.init=Signal 18, A.load=Signal 19, A.neg=Signal 20, A.oe=Signal 21 } )
  , ("spi", PU def SPI.PUPorts{ SPI.wr=Signal 22, SPI.oe=Signal 23
                              , SPI.stop="stop"
                              , SPI.mosi=InputPort "mosi", SPI.miso=OutputPort "miso", SPI.sclk=InputPort "sclk", SPI.cs=InputPort "cs"
                              })
  ]

netWithArithm = busNetwork 31 (Just True) [] []
  [ ("fram1", PU def FR.PUPorts{ FR.oe=Signal 0, FR.wr=Signal 1, FR.addr=map Signal [2, 3, 4, 5] } )
  , ("fram2", PU def FR.PUPorts{ FR.oe=Signal 6, FR.wr=Signal 7, FR.addr=map Signal [8, 9, 10, 11] } )
  , ("shift", PU def S.PUPorts{ S.work=Signal 12, S.direction=Signal 13, S.mode=Signal 14, S.step=Signal 15, S.init=Signal 16, S.oe=Signal 17 })
  , ("accum", PU def A.PUPorts{ A.init=Signal 18, A.load=Signal 19, A.neg=Signal 20, A.oe=Signal 21 } )
  , ("mul", PU (M.multiplier True) M.PUPorts{ M.wr=Signal 22, M.wrSel=Signal 23, M.oe=Signal 24 } )
  , ("div", PU (D.divider 8 True) D.PUPorts{ D.wr=Signal 25, D.wrSel=Signal 26, D.oe=Signal 27, D.oeSel=Signal 28 } )
  ]


testAccumAndFram = unitTest "unittestAccumAndFram" netWithArithm
  def
  [ F.framInput 3 [ "d", "p" ]
  , F.framInput 4 [ "e", "k" ]
  , F.framOutput 5 "p"
  , F.framOutput 6 "k"
  , F.loop 22 "sum" ["s"]
  , F.framOutput 7 "s"
  , F.add "d" "e" ["sum"]
  ]


testShiftAndFram = unitTest "unitShiftAndFram" netWithArithm
  def
  [ F.loop 16 "g1" ["f1"]
  , F.shiftL "f1" ["g1"]
  , F.loop 16 "g2" ["f2"]
  , F.shiftR "f2" ["g2"]
  ]

testFibonacci = unitTest "testFibonacci" netWithArithm
  def
  [ F.loop 0  "b2" ["a1"      ]
  , F.loop 1  "c"  ["b1", "b2"]
  , F.add "a1" "b1" ["c"]
  ]

testFibonacciWithSPI = unitTest "testFibonacciWithSPI" netWithArithmAndSPI
  def
  [ F.loop 0 "b2" ["a1"      ]
  , F.loop 1 "c"  ["b1", "b2"]
  , F.add "a1" "b1" ["c", "c_copy"]
  , F.send "c_copy"
  ]

testDiv4 = unitTest "testDiv4" netWithArithm
  def
  [ F.constant 100 ["a"]
  , F.loop 2 "e" ["b"]
  , F.division "a" "b" ["c"] ["d"]
  , F.add "c" "d" ["e"]

  , F.constant 200 ["a1"]
  , F.loop 2 "e1" ["b1"]
  , F.division "a1" "b1" ["c1"] ["d1"]
  , F.add "c1" "d1" ["e1"]
  ]

testMultiplier = unitTest "testMultiplier" netWithArithm
  def
  [ F.constant 2 ["a"]
  , F.loop 1 "c" ["b"]
  , F.multiply "a" "b" ["c"]

  , F.constant 3 ["x"]
  , F.loop 1 "z" ["y"]
  , F.multiply "y" "x" ["z"]
  ]


-- Почему данный тест не должен работать корректно (почему там not):
--
-- 1. BusNetwork выполняет функциональную симуляцию, без учёта состояния
--    блоков.
-- 2. Сперва к PU привязывается framInput к 3 адресу.
-- 3. Затем к томуже адресу привязывается reg, так как в противном случае он
--    может заблокировать ячейку. А с учётом того что связываение позднее, а
--    вычислительный процесс уже начал планироваться для этой ячейки, то и по
--    времени мы ничего не теряем, а ресурс бережём.
-- 4. В результате значение в ячейке переписывается значением 42, что приводит
--    к тому что на следующих циклах framInput возращает 42, а не значение по
--    умолчанию.
--
-- Более того, даже если output повесить на туже ячейку, то ничего не
-- изменится, так как регистр будет привязан тудаже.
badTestFram = badUnitTest "badTestFram" netWithArithm
  def
  [ F.framInput 3 [ "x" ]
  , F.framOutput 5 "x"
  , F.loop 42 "g" ["f"]
  , F.reg "f" ["g"]
  ]


-----------------------------------------------

processorTest name alg = testCase name $ unitTest ("processorTest_" ++ name) netWithArithmAndSPI def alg

unitTest name n cntx alg = do
  let n' = schedule $ mkModelWithOneNetwork n alg
  TestBenchReport{ tbStatus } <- writeAndRunTestBench $ Project name "../.." (joinPath ["hdl", "gen", name]) n' cntx
  tbStatus @? name

badUnitTest name n cntx alg = do
  let n' = schedule $ mkModelWithOneNetwork n alg
  TestBenchReport{ tbStatus } <- writeAndRunTestBenchDevNull $ Project name "../.." (joinPath ["hdl", "gen", name]) n' cntx
  not tbStatus @? name
