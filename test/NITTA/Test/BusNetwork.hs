{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Test.BusNetwork where

import           Data.Default
import qualified Data.Map                 as M
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.DataFlow
import qualified NITTA.FunctionBlocks     as FB
import qualified NITTA.ProcessUnits.Accum as A
import qualified NITTA.ProcessUnits.Fram  as FR
import           NITTA.TestBench
import           NITTA.Types
import           System.FilePath.Posix    (joinPath)
import           Test.Tasty.HUnit

type FPU = FR.Fram String Int (TaggedTime String Int)

accum_fram1_fram2_netTests
  = let net = busNetwork 24
          [ ("fram1", PU (def :: FPU) FR.Link{ FR.oe=Index 11, FR.wr=Index 10, FR.addr=map Index [9, 8, 7, 6] })
          , ("fram2", PU (def :: FPU) FR.Link{ FR.oe=Index 5, FR.wr=Index 4, FR.addr=map Index [3, 2, 1, 0] })
          , ("accum", PU def A.Link{ A.init=Index 18, A.load=Index 19, A.neg=Index 20, A.oe=Index 21 } )
          ] :: BusNetwork String String Int (TaggedTime String Int)
        alg = [ FB.framInput 3 [ "a"
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
              , ["f"] `FB.loop` "g"
              , FB.reg "f" ["g"]
              , FB.add "d" "e" ["sum"]
              ]
        net' = bindAll alg net
        g = DFG $ map DFGNode alg
        f = Frame net' g Nothing :: SystemState String String String Int (TaggedTime String Int)
        Frame{ nitta=net'' } = foldl (\f' _ -> naive def f') f $ replicate 150 ()
        lib = joinPath ["..", ".."]
        wd = joinPath ["hdl", "gen", "unittest_accum_fram1_fram2_net"]
        cntx = def{ cntxVars=M.fromList [("g", [0x1001])] } :: Cntx String Int
    in testBench lib wd net'' cntx @? "simple test bench (accum, fram1, fram2)"
