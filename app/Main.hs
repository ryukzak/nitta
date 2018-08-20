{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module Main where

import           Data.Default
import qualified Data.Map                      as M
import           Data.Maybe
import           Demo
import           NITTA.API                     (backendServer)
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.DataFlow
import qualified NITTA.Functions               as F
import qualified NITTA.ProcessUnits.Accum      as A
import qualified NITTA.ProcessUnits.Divisor    as D
import qualified NITTA.ProcessUnits.Fram       as FR
import qualified NITTA.ProcessUnits.Multiplier as M
import qualified NITTA.ProcessUnits.Shift      as S
import qualified NITTA.ProcessUnits.SPI        as SPI
import           NITTA.Project
import           NITTA.Types
import           System.FilePath               (joinPath)


microarch = busNetwork 31 (Just True)
  [ InputPort "mosi", InputPort "sclk", InputPort "cs" ]
  [ OutputPort "miso" ]
  [ ("fram1", PU def FR.PUPorts{ FR.oe=Signal 11, FR.wr=Signal 10, FR.addr=map Signal [9, 8, 7, 6] } )
  , ("fram2", PU def FR.PUPorts{ FR.oe=Signal 5, FR.wr=Signal 4, FR.addr=map Signal [3, 2, 1, 0] } )
  , ("shift", PU def S.PUPorts{ S.work=Signal 12, S.direction=Signal 13, S.mode=Signal 14, S.step=Signal 15, S.init=Signal 16, S.oe=Signal 17 })
  , ("accum", PU def A.PUPorts{ A.init=Signal 18, A.load=Signal 19, A.neg=Signal 20, A.oe=Signal 21 } )
  , ("spi", PU
      (SPI.slaveSPI 0)
      SPI.PUPorts{ SPI.wr=Signal 22, SPI.oe=Signal 23
                 , SPI.stop="stop"
                 , SPI.mosi=InputPort "mosi", SPI.miso=OutputPort "miso", SPI.sclk=InputPort "sclk", SPI.cs=InputPort "cs"
                 })
  , ("mul", PU (M.multiplier True) M.PUPorts{ M.wr=Signal 24, M.wrSel=Signal 25, M.oe=Signal 26 } )
  , ("div", PU (D.divisor 4 True) D.PUPorts{ D.wr=Signal 27, D.wrSel=Signal 28, D.oe=Signal 29, D.oeSel=Signal 30 } )
  ]

fibonacciMultAlg = [ F.loop 1 "b2" ["a1"      ] :: F (Parcel String Int)
                   , F.loop 2 "c"  ["b1", "b2"]
                   , F.multiply "a1" "b1" ["c"]
                   ]



divAndMulAlg
  =
    [ F.constant 100 ["a"] :: F (Parcel String Int)
    , F.loop 2 "e" ["b"]
    , F.division "a" "b" ["c"] ["d"]
    -- , F.add "c" "d" ["e"]
    , F.add "c" "d" ["e", "e'"]

    , F.constant 200 ["a1"]
    , F.loop 2 "e1" ["b1"]
    , F.division "a1" "b1" ["c1"] ["d1"]
    , F.add "c1" "d1" ["e1"]

    , F.loop 1 "y" ["x"]
    , F.multiply "x" "e'" ["y"]
    ]


spiAlg = [ F.receive ["a"] :: F (Parcel String Int)
         , F.reg "a" ["b"]
         , F.send "b"
         ]

algorithm = [ F.framInput 3 [ "a"
                             , "d"
                             ] :: F (Parcel String Int)
            , F.framInput 4 [ "b"
                             , "c"
                             , "e"
                             ]
            , F.reg "a" ["x"]
            , F.reg "b" ["y"]
            , F.reg "c" ["z"]
            , F.framOutput 5 "x"
            , F.framOutput 6 "y"
            , F.framOutput 7 "z"
            , F.framOutput 0 "sum"
            , F.constant 42 ["const"]
            , F.framOutput 9 "const"
            , F.loop 0 "g" ["f"]
            , F.shiftL "f" ["g"]
            , F.add "d" "e" ["sum"]
            ]

---------------------------------------------------------------------------------


main = do
  teacupDemo
  fibonacciDemo
  -- test "fibonacciMultAlg" (nitta $ synthesis $ frame $ dfgraph fibonacciMultAlg) def
  test "fibonacci" (nitta $ synthesis $ frame $ dfgraph fibonacciAlg) def
  -- test "graph" (nitta $ synthesis $ frame graph) def

  -- putStrLn "funSim teacup:"
  test "teacup" (nitta $ synthesis $ frame $ dfgraph teacupAlg) def
  funSim 5 def teacupAlg

  -- putStrLn "funSim fibonacci:"
  -- funSim 5 def divAndMulAlg

  -- putStrLn "funSim spi:"
  -- funSim 20 def{ cntxVars=M.fromList [("b", [0])]
  --              , cntxInputs=M.fromList [("a", [1, 2, 3])]
  --              } spiAlg

  -- putStrLn "Server start on 8080..."
  -- backendServer $ frame $ dfgraph divAndMulAlg
  putStrLn "-- the end --"


test n pu cntx = do
  let prj = Project n "../.." (joinPath ["hdl", "gen", n]) pu
  r <- writeAndRunTestBench prj cntx
  if r then putStrLn "Success"
  else putStrLn "Fail"


-----------------------------------------------------------


funSim n cntx alg = putStrLn $ (!! (n - 1)) $ map (filter (/= '"') . show) $ F.simulateAlgByCycle cntx alg

dfgraph = DFG . map node

frame g
  = let ma = bindAll (functions g) microarch
    in Frame ma g Nothing :: SystemState String String String Int (TaggedTime String Int)

synthesis f = foldl (\f' _ -> naive def f') f $ replicate 50 ()

getPU puTitle n = fromMaybe (error "Wrong PU type!") $ castPU $ bnPus n M.! puTitle
