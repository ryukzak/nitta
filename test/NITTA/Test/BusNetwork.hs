{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Test.BusNetwork where

import           Data.Default
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.DataFlow
import qualified NITTA.FunctionBlocks     as FB
import qualified NITTA.ProcessUnits.Accum as A
import qualified NITTA.ProcessUnits.Divisor   as D
import qualified NITTA.ProcessUnits.Fram  as FR
import qualified NITTA.ProcessUnits.Shift as S
import qualified NITTA.ProcessUnits.SPI   as SPI
import           NITTA.Project
import           NITTA.Types
import           System.FilePath.Posix    (joinPath)
import           Test.Tasty.HUnit


netWithArithmAndSPI = busNetwork 27
  [ InputPort "mosi", InputPort "sclk", InputPort "cs" ]
  [ OutputPort "miso" ]
  [ ("fram1", PU def FR.PUPorts{ FR.oe=Signal 11, FR.wr=Signal 10, FR.addr=map Signal [9, 8, 7, 6] } )
  , ("fram2", PU def FR.PUPorts{ FR.oe=Signal 5, FR.wr=Signal 4, FR.addr=map Signal [3, 2, 1, 0] } )
  , ("shift", PU def S.PUPorts{ S.work=Signal 12, S.direction=Signal 13, S.mode=Signal 14, S.step=Signal 15, S.init=Signal 16, S.oe=Signal 17 })
  , ("accum", PU def A.PUPorts{ A.init=Signal 18, A.load=Signal 19, A.neg=Signal 20, A.oe=Signal 21 } )
  , ("spi", PU def SPI.PUPorts{ SPI.wr=Signal 22, SPI.oe=Signal 23
                              , SPI.start="start", SPI.stop="stop"
                              , SPI.mosi=InputPort "mosi", SPI.miso=OutputPort "miso", SPI.sclk=InputPort "sclk", SPI.cs=InputPort "cs"
                              })
  -- , ("mult", PU def M.PUPorts{ M.wr=Index 24, M.sel=Index 25, M.oe=Index 26 } )
  ]

netWithArithm = busNetwork 31 [] []
  [ ("fram1", PU def FR.PUPorts{ FR.oe=Signal 0, FR.wr=Signal 1, FR.addr=map Signal [2, 3, 4, 5] } )
  , ("fram2", PU def FR.PUPorts{ FR.oe=Signal 6, FR.wr=Signal 7, FR.addr=map Signal [8, 9, 10, 11] } )
  , ("shift", PU def S.PUPorts{ S.work=Signal 12, S.direction=Signal 13, S.mode=Signal 14, S.step=Signal 15, S.init=Signal 16, S.oe=Signal 17 })
  , ("accum", PU def A.PUPorts{ A.init=Signal 18, A.load=Signal 19, A.neg=Signal 20, A.oe=Signal 21 } )
  , ("div", PU D.divisor{ D.state=D.DivisorSt True, D.pipeline=8 } D.PUPorts{ D.wr=Signal 27, D.wrSel=Signal 28, D.oe=Signal 29, D.oeSel=Signal 30 } )
  -- , ("mult", PU def M.PUPorts{ M.wr=Index 26, M.sel=Index 27, M.oe=Index 28 } )
  ]

netWithArithm' = busNetwork 31 [] []
  [ ("fram1", PU def FR.PUPorts{ FR.oe=Signal 0, FR.wr=Signal 1, FR.addr=map Signal [2, 3, 4, 5] } )
  , ("fram2", PU def FR.PUPorts{ FR.oe=Signal 6, FR.wr=Signal 7, FR.addr=map Signal [8, 9, 10, 11] } )
  , ("shift", PU def S.PUPorts{ S.work=Signal 12, S.direction=Signal 13, S.mode=Signal 14, S.step=Signal 15, S.init=Signal 16, S.oe=Signal 17 })
  , ("accum", PU def A.PUPorts{ A.init=Signal 18, A.load=Signal 19, A.neg=Signal 20, A.oe=Signal 21 } )
  , ("div", PU D.divisor{ D.state=D.DivisorSt True, D.pipeline=8 } D.PUPorts{ D.wr=Signal 27, D.wrSel=Signal 28, D.oe=Signal 29, D.oeSel=Signal 30 } )
  -- , ("mult", PU def M.PUPorts{ M.wr=Index 26, M.sel=Index 27, M.oe=Index 28 } )
  ]
  

testAccumAndFram = unitTest "unittestAccumAndFram" netWithArithm
  def
  [ FB.framInput 3 [ "d", "p" ]
  , FB.framInput 4 [ "e", "k" ]
  , FB.framOutput 5 "p"
  , FB.framOutput 6 "k"
  , FB.loop 22 ["s"] "sum"
  , FB.framOutput 7 "s"
  , FB.add "d" "e" ["sum"]
  ]


testShiftAndFram = unitTest "unitShiftAndFram" netWithArithm
  def
  [ FB.loop 16 ["f1"] "g1"
  , FB.shiftL "f1" ["g1"]
  , FB.loop 16 ["f2"] "g2"
  , FB.shiftR "f2" ["g2"]
  ]

testFibonacci = unitTest "testFibonacci" netWithArithm
  def
  [ FB.loop 0 ["a1"      ] "b2"
  , FB.loop 1 ["b1", "b2"] "c"
  , FB.add "a1" "b1" ["c"]
  ]

testFibonacciWithSPI = unitTest "testFibonacciWithSPI" netWithArithmAndSPI
  def
  [ FB.loop 0 ["a1"      ] "b2"
  , FB.loop 1 ["b1", "b2"] "c"
  , FB.add "a1" "b1" ["c", "c_copy"]
  , FB.send "c_copy"
  ]

testDiv4 = unitTest "testDiv4" netWithArithm
  def
  [ FB.constant 100 ["a"]
  , FB.loop 2 ["b"] "e"
  , FB.division "a" "b" ["c"] ["d"]
  , FB.add "c" "d" ["e"]

  , FB.constant 200 ["a1"]
  , FB.loop 2 ["b1"] "e1"
  , FB.division "a1" "b1" ["c1"] ["d1"]
  , FB.add "c1" "d1" ["e1"]
  ]

testDiv8 = unitTest "testDiv8" netWithArithm'
  def
  [ FB.constant 100 ["a"]
  , FB.loop 2 ["b"] "e"
  , FB.division "a" "b" ["c"] ["d"]
  , FB.add "c" "d" ["e"]

  , FB.constant 200 ["a1"]
  , FB.loop 2 ["b1"] "e1"
  , FB.division "a1" "b1" ["c1"] ["d1"]
  , FB.add "c1" "d1" ["e1"]
  ]
  

-- Почему данный тест не должен работать корректно (почему там not):
--
-- 1) BusNetwork выполняет функциональную симуляцию, без учёта состояния
--    блоков.
-- 2) Сперва к PU привязывается framInput к 3 адресу.
-- 3) Затем к томуже адресу привязывается reg, так как в противном случае он
--    может заблокировать ячейку. А с учётом того что связываение позднее, а
--    вычислительный процесс уже начал планироваться для этой ячейки, то и по
--    времени мы ничего не теряем, а ресурс бережём.
-- 4) В результате значение в ячейке переписывается значением 42, что приводит
--    к тому что на следующих циклах framInput возращает 42, а не значение по
--    умолчанию.
--
-- Более того, даже если output повесить на туже ячейку, то ничего не
-- изменится, так как регистр будет привязан тудаже.
badTestFram = badUnitTest "badTestFram" netWithArithm
  def
  [ FB.framInput 3 [ "x" ]
  , FB.framOutput 5 "x"
  , FB.loop 42 ["f"] "g"
  , FB.reg "f" ["g"]
  ]


-----------------------------------------------

unitTest name n cntx alg = do
  let n' = nitta $ synthesis $ frame n alg
  r <- writeAndRunTestBench (Project name "../.." (joinPath ["hdl", "gen", name]) n') cntx
  r @? name

badUnitTest name n cntx alg = do
  let n' = nitta $ synthesis $ frame n alg
  r <- writeAndRunTestBench (Project name "../.." (joinPath ["hdl", "gen", name]) n') cntx
  not r @? name

synthesis f = foldl (\f' _ -> naive def f') f $ replicate 50 ()

frame n alg
  = let n' = bindAll alg n
    in Frame n' (DFG $ map node alg) Nothing :: SystemState String String String Int (TaggedTime String Int)
