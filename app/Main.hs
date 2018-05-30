{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           Control.Monad
import qualified Data.Array                  as A
import           Data.Default
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Set                    (fromList)
import qualified Data.String.Utils           as U
import           Data.Typeable
import           Debug.Trace
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors (simpleCors)
import           NITTA.API
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.DataFlow
import qualified NITTA.FunctionBlocks        as FB
import qualified NITTA.ProcessUnits.Accum    as A
import qualified NITTA.ProcessUnits.Fram     as FR
import qualified NITTA.ProcessUnits.Shift    as S
import qualified NITTA.ProcessUnits.SPI      as SPI
import qualified NITTA.ProcessUnits.Div      as D
import           NITTA.TestBench
import           NITTA.Timeline
import           NITTA.Types
import           NITTA.Utils
import           Servant.JS
import qualified Servant.JS                  as SJS
import qualified Servant.JS.Angular          as NG
import           System.Directory
import           System.FilePath.Posix       (joinPath, (</>))
import           Text.StringTemplate


microarch = busNetwork 28
  [ ("fram1", PU def FR.Link{ FR.oe=Index 11, FR.wr=Index 10, FR.addr=map Index [9, 8, 7, 6] } )
  , ("fram2", PU def FR.Link{ FR.oe=Index 5, FR.wr=Index 4, FR.addr=map Index [3, 2, 1, 0] } )
  , ("shift", PU def S.Link{ S.work=Index 12, S.direction=Index 13, S.mode=Index 14, S.step=Index 15, S.init=Index 16, S.oe=Index 17 })
  , ("accum", PU def A.Link{ A.init=Index 18, A.load=Index 19, A.neg=Index 20, A.oe=Index 21 } )
  , ("spi", PU def SPI.Link{ SPI.wr=Index 22, SPI.oe=Index 23
                           , SPI.start=Name "start", SPI.stop=Name "stop"
                           , SPI.mosi=Name "mosi", SPI.miso=Name "miso", SPI.sclk=Name "sclk", SPI.cs=Name "cs"
                           })
  , ("div", PU def D.Link{ D.wr=Index 24, D.oe=Index 25, D.wrSel=Index 26, D.oeSel=Index 27 } )
  ]

synthesisedFib
  = let g = DFG [ node $ FB.add "a" "b" ["b'"]
                , node $ FB.loop 1 ["a"] "a'"
                , node $ FB.loop 1 ["b", "a'"] "b'"
                ]
        ma = bindAll (functionalBlocks g) microarch
    in Frame ma g Nothing :: SystemState String String String Int (TaggedTime String Int)

synthesisedFib' = foldl (\f' _ -> naive def f') synthesisedFib $ replicate 50 ()

synthesisedLevel
  = let g = DFG [ node $ FB.framInput 3 [ "a" ]
                , node $ FB.framInput 4 [ "b" ]
                , node $ FB.add "a" "b" ["c"]
                , node $ FB.framOutput 0 "c"
                -- FIXME: Синтезируется, но сгенировать тест пока нельзя.
                -- , node $ FB.Constant 0 $ O ["p"]
                -- , node $ FB.Constant 0 $ O ["const0"]
                -- , node $ FB.Constant 1 $ O ["const1"]
                -- , DFGSwitch "cond"
                --   [ ( 0, DFG [ node $ FB.Add (I "c") (I "const0") $ O ["d"] ] )
                --   , ( 1, DFG [ node $ FB.Add (I "c") (I "const1") $ O ["d"] ] )
                --   ]
                -- , node $ FB.FramOutput 0 $ I "d"
                ]
        ma = bindAll (functionalBlocks g) microarch
        f = Frame ma g Nothing :: SystemState String String String Int (TaggedTime String Int)
    in nitta $ foldl (\f' _ -> naive def f') f $ replicate 50 ()

-- | Пример работы с единым временем.
synthesisedFrame
  = let alg = [ FB.framInput 3 [ "a"
                               , "d"
                               ]
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
        dataFlow = DFG $ map DFGNode alg
        microarch' = bindAll (functionalBlocks dataFlow) microarch
        f = Frame microarch' dataFlow Nothing :: SystemState String String String Int (TaggedTime String Int)
        Frame{ nitta=pu } = foldl (\f' _ -> naive def f') f $ replicate 50 ()
    in pu

-- | Пример работы с единым временем.
synthesisedFrameSPI
  = let alg = [ FB.receive ["a"]
              , FB.send "b"
              , FB.reg "a" ["b"]
              ]
        dataFlow = DFG $ map DFGNode alg
        microarch' = bindAll (functionalBlocks dataFlow) microarch
        f = Frame microarch' dataFlow Nothing :: SystemState String String String Int (TaggedTime String Int)
        Frame{ nitta=pu } = foldl (\f' _ -> naive def f') f $ replicate 50 ()
    in pu

root
  = let alg = [ FB.framInput 3 [ "a"
                               , "d"
                               ]
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
        dataFlow = DFG $ map DFGNode alg
        microarch' = bindAll (functionalBlocks dataFlow) microarch
    in Frame microarch' dataFlow Nothing :: SystemState String String String Int (TaggedTime String Int)

---------------------------------------------------------------------------------


main = do
  -- test scheduledBranch (def{ cntxVars=M.fromList [] } :: Cntx String Int)
  test (nitta synthesisedFib') def{ cntxVars=M.fromList [] }
  -- test scheduledBranchSPI
  --   (def{ cntxVars=M.fromList [("b", [0])]
  --       , cntxInputs=M.fromList [("a", [1, 2, 3])]
  --       } :: Cntx String Int)
  simulateTeaCup 100
  -- webServer synthesisedFib

webServer root = do
  let prefix = "import axios from 'axios';\n\
                \var api = {}\n\
                \export default api;"
  let axios' = axiosWith defAxiosOptions defCommonGeneratorOptions{ urlPrefix="http://localhost:8080"
                                                                  , SJS.moduleName="api"
                                                                  }
  createDirectoryIfMissing True $ joinPath ["web", "src", "gen"]
  writeJSForAPI (Proxy :: Proxy SynthesisAPI) ((prefix <>) . axios') $ joinPath ["web", "src", "gen", "nitta-api.js"]
  putStrLn "Server start on 8080..."
  app def{ state=root } >>= run 8080 . simpleCors

test pu cntx = do
  timeline "resource/data.json" pu
  r <- testBench ".." (joinPath ["hdl", "gen"]) pu cntx
  if r then putStrLn "Success"
  else putStrLn "Fail"
  print "ok"


simulateSPI n = do
  mapM_ putStrLn $ take n $ map show $ FB.simulateAlg (def{ cntxVars=M.fromList [("b", [0])]
                                                          , cntxInputs=M.fromList [("a", [1, 2, 3])]
                                                          } :: Cntx String Int)
    [ FB.receive ["a"] :: FB (Parcel String Int)
    , FB.add "a" "b" ["c1", "c2"]
    , FB.loop 0 ["b"] "c1"
    , FB.send "c2"
    ]
  print "ok"


simulateFib n = do
  putStrLn $ (!! n) $ map show $ FB.simulateAlg def
    [ FB.loop 0 ["a1"      ] "b2"
    , FB.loop 1 ["b1", "b2"] "c"
    , FB.add "a1" "b1" ["c"] :: FB (Parcel String Int)
    ]
  print "ok"


simulateTeaCup n = do
  putStrLn $ (!! n) $ map (filter (/= '"') . show) $ FB.simulateAlg def
    [ FB.constant 70000 ["T_room"] :: FB (Parcel String Int)
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
  print "ok"


getPU puTitle net = fromMaybe (error "Wrong PU type!") $ castPU $ bnPus net M.! puTitle
