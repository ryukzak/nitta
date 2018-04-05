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
import qualified NITTA.ProcessUnits.Fram  as FR
import qualified NITTA.ProcessUnits.Shift as S
import           NITTA.TestBench
import           NITTA.Types
import           System.FilePath.Posix    (joinPath)
import           Test.Tasty.HUnit


netWithFramShiftAccum = busNetwork 27
  [ ("fram1", PU def FR.Link{ FR.oe=Index 11, FR.wr=Index 10, FR.addr=map Index [9, 8, 7, 6] } )
  , ("fram2", PU def FR.Link{ FR.oe=Index 5, FR.wr=Index 4, FR.addr=map Index [3, 2, 1, 0] } )
  , ("shift", PU def S.Link{ S.work=Index 12, S.direction=Index 13, S.mode=Index 14, S.step=Index 15, S.init=Index 16, S.oe=Index 17 })
  , ("accum", PU def A.Link{ A.init=Index 18, A.load=Index 19, A.neg=Index 20, A.oe=Index 21 } )
  -- , ("spi", PU def SPI.Link{ SPI.wr=Index 22, SPI.oe=Index 23
  --                          , SPI.start=Name "start", SPI.stop=Name "stop"
  --                          , SPI.mosi=Name "mosi", SPI.miso=Name "miso", SPI.sclk=Name "sclk", SPI.cs=Name "cs"
  --                          })
  -- , ("mult", PU def M.Link{ M.wr=Index 24, M.sel=Index 25, M.oe=Index 26 } )
  ]


testAccumAndFram = unitTest "unittestAccumAndFram" netWithFramShiftAccum
  def
  [ FB.framInput 3 [ "d", "p" ]
  , FB.framInput 4 [ "e", "k" ]
  , FB.framOutput 5 "p"
  , FB.framOutput 6 "k"
  , FB.loop 22 ["s"] "sum"
  , FB.framOutput 7 "s"
  , FB.add "d" "e" ["sum"]
  ]


testShiftAndFram = unitTest "unitShiftAndFram" netWithFramShiftAccum
  def
  [ FB.loop 16 ["f1"] "g1"
  , FB.shiftL "f1" ["g1"]
  , FB.loop 16 ["f2"] "g2"
  , FB.shiftR "f2" ["g2"]
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
badTestFram = badUnitTest "badTestFram" netWithFramShiftAccum
  def
  [ FB.framInput 3 [ "x" ]
  , FB.framOutput 5 "x"
  , FB.loop 42 ["f"] "g"
  , FB.reg "f" ["g"]
  ]


-----------------------------------------------

unitTest name n cntx alg = do
  let n' = nitta $ synthesis $ frame n alg
  r <- testBench "../.." (joinPath ["hdl", "gen", name]) n' cntx
  r @? name

badUnitTest name n cntx alg = do
  let n' = nitta $ synthesis $ frame n alg
  r <- testBench "../.." (joinPath ["hdl", "gen", name]) n' cntx
  not r @? name

synthesis f = foldl (\f' _ -> naive def f') f $ replicate 50 ()

frame n alg
  = let n' = bindAll alg n
    in Frame n' (DFG $ map node alg) Nothing :: SystemState String String String Int (TaggedTime String Int)
