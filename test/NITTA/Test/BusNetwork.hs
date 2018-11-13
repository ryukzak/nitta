{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Test.BusNetwork where

import           Data.Default
import           Data.Either                   (isRight)
import           NITTA.BusNetwork
import qualified NITTA.Functions               as F
import qualified NITTA.ProcessUnits.Accum      as A
import qualified NITTA.ProcessUnits.Divider    as D
import qualified NITTA.ProcessUnits.Fram       as FR
import qualified NITTA.ProcessUnits.Multiplier as M
import qualified NITTA.ProcessUnits.Shift      as S
import qualified NITTA.ProcessUnits.SPI        as SPI
import           NITTA.Types
import           NITTA.Utils.Test
import           Test.Tasty.HUnit


netWithArithmAndSPI = busNetwork 31 (Just True)
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
  , ("div", PU (D.divider 8 True) D.PUPorts{ D.wr=Signal 24, D.wrSel=Signal 25, D.oe=Signal 26, D.oeSel=Signal 27 } )
  , ("mul", PU (M.multiplier True) M.PUPorts{ M.wr=Signal 28, M.wrSel=Signal 29, M.oe=Signal 30 } )
  ]

netWithArithmAndSPINoDropData = netWithArithmAndSPI{ bnAllowDrop=Just False }

netWithArithm = busNetwork 31 (Just True) [] []
  [ ("fram1", PU def FR.PUPorts{ FR.oe=Signal 0, FR.wr=Signal 1, FR.addr=map Signal [2, 3, 4, 5] } )
  , ("fram2", PU def FR.PUPorts{ FR.oe=Signal 6, FR.wr=Signal 7, FR.addr=map Signal [8, 9, 10, 11] } )
  , ("shift", PU def S.PUPorts{ S.work=Signal 12, S.direction=Signal 13, S.mode=Signal 14, S.step=Signal 15, S.init=Signal 16, S.oe=Signal 17 })
  , ("accum", PU def A.PUPorts{ A.init=Signal 18, A.load=Signal 19, A.neg=Signal 20, A.oe=Signal 21 } )
  , ("mul", PU (M.multiplier True) M.PUPorts{ M.wr=Signal 22, M.wrSel=Signal 23, M.oe=Signal 24 } )
  , ("div", PU (D.divider 8 True) D.PUPorts{ D.wr=Signal 25, D.wrSel=Signal 26, D.oe=Signal 27, D.oeSel=Signal 28 } )
  ]


testAccumAndFram = algTestCase "unittestAccumAndFram" netWithArithm
  [ F.framInput 3 [ "d", "p" ]
  , F.framInput 4 [ "e", "k" ]
  , F.framOutput 5 "p"
  , F.framOutput 6 "k"
  , F.loop 22 "sum" ["s"]
  , F.framOutput 7 "s"
  , F.add "d" "e" ["sum"]
  ]


testShiftAndFram = algTestCase "unitShiftAndFram" netWithArithm
  [ F.loop 16 "g1" ["f1"]
  , F.shiftL "f1" ["g1"]
  , F.loop 16 "g2" ["f2"]
  , F.shiftR "f2" ["g2"]
  ]

testFibonacci = algTestCase "testFibonacci" netWithArithm
  [ F.loop 0  "b2" ["a1"      ]
  , F.loop 1  "c"  ["b1", "b2"]
  , F.add "a1" "b1" ["c"]
  ]

fibWithSPI = 
  [ F.loop 0 "b2" ["a1"      ]
  , F.loop 1 "c"  ["b1", "b2"]
  , F.add "a1" "b1" ["c", "c_copy"]
  , F.send "c_copy"
  ]

testFibonacciWithSPI = algTestCase "testFibonacciWithSPI" netWithArithmAndSPI fibWithSPI
testFibonacciWithSPINoDataDrop = algTestCase "testFibonacciWithSPINoDataDrop" netWithArithmAndSPINoDropData fibWithSPI

testDiv4 = algTestCase "testDiv4" netWithArithm
  [ F.constant 100 ["a"]
  , F.loop 2 "e" ["b"]
  , F.division "a" "b" ["c"] ["d"]
  , F.add "c" "d" ["e"]

  , F.constant 200 ["a1"]
  , F.loop 2 "e1" ["b1"]
  , F.division "a1" "b1" ["c1"] ["d1"]
  , F.add "c1" "d1" ["e1"]
  ]

testMultiplier = algTestCase "testMultiplier" netWithArithm
  [ F.constant 2 ["a"]
  , F.loop 1 "c" ["b"]
  , F.multiply "a" "b" ["c"]

  , F.constant 3 ["x"]
  , F.loop 1 "z" ["y"]
  , F.multiply "y" "x" ["z"]
  ]

-----------------------------------------------

luaTestCase name lua = testCase name $ do
    res <- testLua ("luaTestCase" ++ name) netWithArithmAndSPI lua
    isRight res @? show res

algTestCase name arch alg = testCase name $ do
    res <- test ("algTestCase" ++ name) arch alg
    isRight res @? show res
