{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module Main where

import           Control.Applicative         ((<$>))
import           Data.Atomics.Counter
import           Data.Default
import qualified Data.Map                    as M
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.FlowGraph
import qualified NITTA.FunctionBlocks        as FB
import qualified NITTA.ProcessUnits.Accum    as A
import qualified NITTA.ProcessUnits.Fram     as FR
import           NITTA.ProcessUnits.FramSpec
import           NITTA.ProcessUnitsSpec
import           NITTA.TestBench
import           NITTA.Types
import           NITTA.Utils
import           System.Environment
import           System.FilePath.Posix       (joinPath)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck       as QC


main = do
  -- Используется для того, что бы раскладывать файлы в разные папки при симуляции.
  counter <- newCounter 0
  setEnv "TASTY_QUICKCHECK_TESTS" "10"
  -- TODO: Положть gen в ram fs, нечего насиловать диск.
  defaultMain $ testGroup "NITTA tests"
    [ testGroup "Fram unittests" huFramTests
    , testGroup "Fram quickcheck" (qcFramTests counter)
    , testGroup "BusNetwork unittests" [ accum_fram1_fram2_netTests ]
    , testGroup "Utils" [ values2dumpTests ]
    ]

qcFramTests counter
  = [ QC.testProperty "Fram completeness" $ prop_completness <$> processGen framProxy
    , QC.testProperty "Fram simulation" $ fmap (prop_simulation "prop_simulation_fram_" counter) $ inputsGen =<< processGen framProxy
    ]

huFramTests
  = let alg = [ FB.reg (I "aa") $ O ["ab"]
              , FB.framOutput 9 $ I "ac"
              ]
        fram = bindAllAndNaiveSchedule alg (def :: FR.Fram String Int)
        library = joinPath ["..", ".."]
        workdir = joinPath ["hdl", "gen", "unittest_fram"]
    in [ testCase "Simple unit test" $ assert (testBench library workdir fram (def{ cntxVars=M.fromList [("aa", [42]), ("ac", [0x1003])]
                                                                                  } :: Cntx String Int))
       ]



accum_fram1_fram2_netTests
  = let net :: BusNetwork String String (TaggedTime String Int)
        net = busNetwork 24
          [ ("fram1", PU def FR.Link{ FR.oe=Index 11, FR.wr=Index 10, FR.addr=map Index [9, 8, 7, 6] })
          , ("fram2", PU def FR.Link{ FR.oe=Index 5, FR.wr=Index 4, FR.addr=map Index [3, 2, 1, 0] })
          , ("accum", PU def A.Link{ A.init=Index 18, A.load=Index 19, A.neg=Index 20, A.oe=Index 21 } )
          ]
        alg = [ FB.framInput 3 $ O [ "a"
                                   , "d"
                                   ]
              , FB.framInput 4 $ O [ "b"
                                   , "c"
                                   , "e"
                                   ]
              , FB.reg (I "a") $ O ["x"]
              , FB.reg (I "b") $ O ["y"]
              , FB.reg (I "c") $ O ["z"]
              , FB.framOutput 5 $ I "x"
              , FB.framOutput 6 $ I "y"
              , FB.framOutput 7 $ I "z"
              , FB.framOutput 0 $ I "sum"
              , FB.loop (O ["f"]) $ I "g"
              , FB.reg (I "f") $ O ["g"]
              , FB $ FB.Add (I "d") (I "e") (O ["sum"])
              ]
        net' = bindAll alg (net :: BusNetwork String String (TaggedTime String Int))
        dataFlow = DFG $ map DFGNode alg
        compiler = Frame net' dataFlow (dataFlow2controlFlow dataFlow) Nothing []
        Frame{ nitta=net''
             } = foldl (\comp _ -> naive def comp) compiler $ replicate 150 ()
        library = joinPath ["..", ".."]
        workdir = joinPath ["hdl", "gen", "unittest_accum_fram1_fram2_net"]
    in testCase "Obscure integration test for net of accum, fram1, fram2"
          $ assert $ testBench library workdir net'' (def{ cntxVars=M.fromList [("g", [0x1001])]
                                                         } :: Cntx String Int)


values2dumpTests = testCase "values2dump" $ do
  assertEqual "values2dump: xxxx" "0" $ values2dump [X, X, X, X]
  assertEqual "values2dump: 0000" "0" $ values2dump [B False, B False, B False, B False]
  assertEqual "values2dump: 1111" "f" $ values2dump [B True, B True, B True, B True]
  assertEqual "values2dump: 10111" "17" $ values2dump [B True, B False, B True, B True, B True]
