{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module Main where

import           Data.Default
import qualified Data.Map                 as M
import           Data.Maybe
import           NITTA.API                (backendServer)
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.DataFlow
import qualified NITTA.FunctionBlocks     as FB
import qualified NITTA.ProcessUnits.Accum as A
import qualified NITTA.ProcessUnits.Div   as D
import qualified NITTA.ProcessUnits.Fram  as FR
import qualified NITTA.ProcessUnits.Mult  as M
import qualified NITTA.ProcessUnits.Shift as S
import qualified NITTA.ProcessUnits.SPI   as SPI
import           NITTA.Project
import           NITTA.Types
import           System.FilePath          (joinPath)


microarch = busNetwork 30
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
  , ("mul", PU def M.PUPorts{ M.wr=Signal 24, M.wrSel=Signal 26, M.oe=Signal 25 } )
  , ("div", PU def D.PUPorts{ D.wr=Signal 26, D.wrSel=Signal 27, D.oe=Signal 28, D.oeSel=Signal 29 } )
  ]

fibonacciAlg = [ FB.loop 0 ["a1"      ] "b2" :: FB (Parcel String Int)
               , FB.loop 1 ["b1", "b2"] "c"
               , FB.add "a1" "b1" ["c", "c_copy"]
               , FB.send "c_copy"
               , FB.send "i2"
               , FB.loop 2 ["i1", "i2"] "i+1"
               , FB.constant 1 ["one"]
               , FB.add "i1" "one" ["i+1"]
               ]

fibonacciMultAlg = [ FB.loop 1 ["a1"      ] "b2" :: FB (Parcel String Int)
                   , FB.loop 2 ["b1", "b2"] "c"
                   , FB.mul "a1" "b1" ["c"]
                   ]

teacupAlg = [ FB.constant 70000 ["T_room"] :: FB (Parcel String Int)
            , FB.constant 10000 ["t_ch"]
            , FB.constant 125 ["t_step1", "t_step2"]
            , FB.loop 180000 ["T_cup1", "T_cup2"] "t_cup'"
            -- (Teacup Temperature - T_room) / t_ch
            , FB.sub "T_room" "T_cup1" ["acc"]
            , FB.div "acc" "t_ch" ["loss"] []
            -- INTEG ( -loss to Room
            , FB.mul "loss" "t_step1" ["delta"]
            , FB.add "T_cup2" "delta" ["t_cup'"]
            , FB.loop 0 ["t"] "t'"
            , FB.add "t" "t_step2" ["t'"]
            ]

spiAlg = [ FB.receive ["a"] :: FB (Parcel String Int)
         , FB.reg "a" ["b"]
         , FB.send "b"
         ]

algorithm = [ FB.framInput 3 [ "a"
                             , "d"
                             ] :: FB (Parcel String Int)
            , FB.framInput 4 [ "b"
                             , "c"
                             , "e"
                             ]
            , FB.reg "a" ["x"]
            , FB.reg "b" ["y"]
            , FB.reg "c" ["z"]
            , FB.framOutput 5 "x"
            , FB.framOutput 6 "y"
            , FB.framOutput 7 "z"
            , FB.framOutput 0 "sum"
            , FB.constant 42 ["const"]
            , FB.framOutput 9 "const"
            , FB.loop 0 ["f"] "g"
            , FB.shiftL "f" ["g"]
            , FB.add "d" "e" ["sum"]
            ]

graph = DFG [ node (FB.framInput 3 [ "a" ] :: FB (Parcel String Int))
            , node $ FB.framInput 4 [ "b" ]
            , node $ FB.sub "a" "b" ["c"]
            , node $ FB.framOutput 0 "c"
            -- FIXME: Синтезируется, но сгенировать тест пока нельзя.
            -- , node $ FB.constant 0 ["p"]
            -- , node $ FB.constant 0 ["const0"]
            -- , node $ FB.constant 1 ["const1"]
            -- , DFGSwitch "cond"
            --   [ ( 0, DFG [ node $ FB.add "c" "const0" ["d"] ] )
            --   , ( 1, DFG [ node $ FB.add "c" "const1" ["d"] ] )
            --   ]
            -- , node $ FB.framOutput 0 "d"
            ]


---------------------------------------------------------------------------------


main = do
  -- test "fibonacciMultAlg" (nitta $ synthesis $ frame $ dfgraph fibonacciMultAlg) def
  test "fibonacci" (nitta $ synthesis $ frame $ dfgraph fibonacciAlg) def
  -- test "graph" (nitta $ synthesis $ frame graph) def

  -- putStrLn "funSim teacup:"
  -- funSim 5 def teacupAlg

  putStrLn "funSim fibonacci:"
  funSim 5 def fibonacciAlg

  -- putStrLn "funSim spi:"
  -- funSim 20 def{ cntxVars=M.fromList [("b", [0])]
  --              , cntxInputs=M.fromList [("a", [1, 2, 3])]
  --              } spiAlg

  putStrLn "Server start on 8080..."
  backendServer $ frame $ dfgraph fibonacciAlg
  putStrLn "-- the end --"


test n pu cntx = do
  let prj = Project n "../.." (joinPath ["hdl", "gen", n]) pu
  r <- writeAndRunTestBench prj cntx
  if r then putStrLn "Success"
  else putStrLn "Fail"


-----------------------------------------------------------


funSim n cntx alg = putStrLn $ (!! (n - 1)) $ map (filter (/= '"') . show) $ FB.simulateAlgByCycle cntx alg

dfgraph = DFG . map node

frame g
  = let ma = bindAll (functionalBlocks g) microarch
    in Frame ma g Nothing :: SystemState String String String Int (TaggedTime String Int)

synthesis f = foldl (\f' _ -> naive def f') f $ replicate 50 ()

getPU puTitle n = fromMaybe (error "Wrong PU type!") $ castPU $ bnPus n M.! puTitle
