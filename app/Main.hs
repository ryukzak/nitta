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


microarch = busNetwork 24
  [ ("fram1", PU def FR.Link{ FR.oe=Index 11, FR.wr=Index 10, FR.addr=map Index [9, 8, 7, 6] } )
  , ("fram2", PU def FR.Link{ FR.oe=Index 5, FR.wr=Index 4, FR.addr=map Index [3, 2, 1, 0] } )
  , ("shift", PU def S.Link{ S.work=Index 12, S.direction=Index 13, S.mode=Index 14, S.step=Index 15, S.init=Index 16, S.oe=Index 17 })
  , ("accum", PU def A.Link{ A.init=Index 18, A.load=Index 19, A.neg=Index 20, A.oe=Index 21 } )
  , ("spi", PU def SPI.Link{ SPI.wr=Index 22, SPI.oe=Index 23
                           , SPI.start=Name "start", SPI.stop=Name "stop"
                           , SPI.mosi=Name "mosi", SPI.miso=Name "miso", SPI.sclk=Name "sclk", SPI.cs=Name "cs"
                           })
  ]

synthesisedFib
  = let g = DFG [ node $ FB.Add (I "a") (I "b") (O $ fromList ["b'"])
                , node $ FB.Loop (O $ fromList ["a"]) $ I "a'"
                , node $ FB.Loop (O $ fromList ["b", "a'"]) $ I "b'"
                ]
        ma = bindAll (functionalBlocks g) microarch
    in Frame ma g Nothing :: SystemState String String String Int (TaggedTime String Int)

synthesisedFib' = foldl (\f' _ -> naive def f') synthesisedFib $ replicate 50 ()

synthesisedLevel
  = let g = DFG [ node $ FB.FramInput 3 $ O $ fromList [ "a" ]
                , node $ FB.FramInput 4 $ O $ fromList [ "b" ]
                , node $ FB.Add (I "a") (I "b") (O $ fromList ["c"])
                , node $ FB.FramOutput 0 $ I "c"
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
  = let alg = [ FB.framInput 3 $ O $ fromList [ "a"
                                              , "d"
                                              ]
              , FB.framInput 4 $ O $ fromList [ "b"
                                              , "c"
                                              , "e"
                                              ]
              , FB.reg (I "a") $ O $ fromList ["x"]
              , FB.reg (I "b") $ O $ fromList ["y"]
              , FB.reg (I "c") $ O $ fromList ["z"]
              , FB.framOutput 5 $ I "x"
              , FB.framOutput 6 $ I "y"
              , FB.framOutput 7 $ I "z"
              , FB.framOutput 0 $ I "sum"
              , FB $ FB.Constant (42 :: Int) (O $ fromList ["const"] :: O (Parcel String Int))
              , FB.framOutput 9 $ I "const"
              , FB.loop (O $ fromList ["f"]) $ I "g"
              , FB $ FB.ShiftL (I "f") $ O $ fromList ["g"]
              , FB $ FB.Add (I "d") (I "e") (O $ fromList ["sum"])
              ]
        dataFlow = DFG $ map DFGNode alg
        microarch' = bindAll (functionalBlocks dataFlow) microarch
        f = Frame microarch' dataFlow Nothing :: SystemState String String String Int (TaggedTime String Int)
        Frame{ nitta=pu } = foldl (\f' _ -> naive def f') f $ replicate 50 ()
    in pu

-- | Пример работы с единым временем.
synthesisedFrameSPI
  = let alg = [ FB $ FB.Receive $ O $ fromList ["a"]
              , FB $ FB.Send (I "b")
              , FB.reg (I "a") $ O $ fromList ["b"]
              ]
        dataFlow = DFG $ map DFGNode alg
        microarch' = bindAll (functionalBlocks dataFlow) microarch
        f = Frame microarch' dataFlow Nothing :: SystemState String String String Int (TaggedTime String Int)
        Frame{ nitta=pu } = foldl (\f' _ -> naive def f') f $ replicate 50 ()
    in pu

root
  = let alg = [ FB.framInput 3 $ O $ fromList [ "a"
                                              , "d"
                                              ]
              , FB.framInput 4 $ O $ fromList [ "b"
                                              , "c"
                                              , "e"
                                              ]
              , FB.reg (I "a") $ O $ fromList ["x"]
              , FB.reg (I "b") $ O $ fromList ["y"]
              , FB.reg (I "c") $ O $ fromList ["z"]
              , FB.framOutput 5 $ I "x"
              , FB.framOutput 6 $ I "y"
              , FB.framOutput 7 $ I "z"
              , FB.framOutput 0 $ I "sum"
              , FB $ FB.Constant (42 :: Int) (O $ fromList ["const"] :: O (Parcel String Int))
              , FB.framOutput 9 $ I "const"
              , FB.loop (O $ fromList ["f"]) $ I "g"
              , FB $ FB.ShiftL (I "f") $ O $ fromList ["g"]
              , FB $ FB.Add (I "d") (I "e") (O $ fromList ["sum"])
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
  -- simulateSPI 3
  webServer synthesisedFib

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
    [ FB $ FB.Receive $ O $ fromList ["a"] :: FB (Parcel String Int)
    , FB $ FB.Add (I "a") (I "b") (O $ fromList ["c1", "c2"])
    , FB $ FB.Loop (O $ fromList ["b"]) (I "c1")
    , FB $ FB.Send (I "c2")
    ]
  print "ok"

getPU puTitle net = fromMaybe (error "Wrong PU type!") $ castPU $ bnPus net M.! puTitle
