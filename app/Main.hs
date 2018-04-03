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
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors (simpleCors)
import           NITTA.API
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.DataFlow
import qualified NITTA.FunctionBlocks        as FB
import qualified NITTA.ProcessUnits.Accum    as A
import qualified NITTA.ProcessUnits.Fram     as FR
import qualified NITTA.ProcessUnits.Mult     as M
import qualified NITTA.ProcessUnits.Shift    as S
import qualified NITTA.ProcessUnits.SPI      as SPI
import           NITTA.TestBench
import           NITTA.Timeline
import           NITTA.Types
import           Servant.JS
import qualified Servant.JS                  as SJS
import           System.Directory
import           System.FilePath.Posix       (joinPath)


microarch = busNetwork 27
  [ ("fram1", PU def FR.Link{ FR.oe=Index 11, FR.wr=Index 10, FR.addr=map Index [9, 8, 7, 6] } )
  , ("fram2", PU def FR.Link{ FR.oe=Index 5, FR.wr=Index 4, FR.addr=map Index [3, 2, 1, 0] } )
  , ("shift", PU def S.Link{ S.work=Index 12, S.direction=Index 13, S.mode=Index 14, S.step=Index 15, S.init=Index 16, S.oe=Index 17 })
  , ("accum", PU def A.Link{ A.init=Index 18, A.load=Index 19, A.neg=Index 20, A.oe=Index 21 } )
  , ("spi", PU def SPI.Link{ SPI.wr=Index 22, SPI.oe=Index 23
                           , SPI.start=Name "start", SPI.stop=Name "stop"
                           , SPI.mosi=Name "mosi", SPI.miso=Name "miso", SPI.sclk=Name "sclk", SPI.cs=Name "cs"
                           })
  -- , ("mult", PU def M.Link{ M.wr=Index 24, M.sel=Index 25, M.oe=Index 26 } )
  ]

fibonacciAlg = [ FB.loop 0 ["a1"      ] "b2" :: FB (Parcel String Int)
               , FB.loop 1 ["b1", "b2"] "c"
               , FB.add "a1" "b1" ["c"]
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
            , FB.div "acc" "t_ch" ["loss"]
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
  -- funSim 100 def teacupAlg

  -- putStrLn "funSim fibonacci:"
  -- funSim 20 def fibonacciAlg

  -- putStrLn "funSim spi:"
  -- funSim 20 def{ cntxVars=M.fromList [("b", [0])]
  --              , cntxInputs=M.fromList [("a", [1, 2, 3])]
  --              } spiAlg

  -- putStrLn "Server start on 8080..."
  -- webServer $ frame $ dfgraph fibonacciAlg


webServer root = do
  let prefix = "import axios from 'axios';\n\
                \var api = {}\n\
                \export default api;"
  let axios' = axiosWith defAxiosOptions defCommonGeneratorOptions{ urlPrefix="http://localhost:8080"
                                                                  , SJS.moduleName="api"
                                                                  }
  createDirectoryIfMissing True $ joinPath ["web", "src", "gen"]
  writeJSForAPI (Proxy :: Proxy SynthesisAPI) ((prefix <>) . axios') $ joinPath ["web", "src", "gen", "nitta-api.js"]
  app def{ state=root } >>= run 8080 . simpleCors


test n pu cntx = do
  timeline "resource/data.json" pu
  r <- testBench "../.." (joinPath ["hdl", "gen", n]) pu cntx
  if r then putStrLn "Success"
  else putStrLn "Fail"


-----------------------------------------------------------


funSim n cntx alg = putStrLn $ (!! n) $ map (filter (/= '"') . show) $ FB.simulateAlg cntx alg

dfgraph = DFG . map node

frame g
  = let ma = bindAll (functionalBlocks g) microarch
    in Frame ma g Nothing :: SystemState String String String Int (TaggedTime String Int)

synthesis f = foldl (\f' _ -> naive def f') f $ replicate 50 ()

getPU puTitle net = fromMaybe (error "Wrong PU type!") $ castPU $ bnPus net M.! puTitle
