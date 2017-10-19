{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module Main where

import           Data.Array                  (array)
import           Data.Default
import           NITTA.BusNetwork
import qualified NITTA.Compiler              as C
import           NITTA.Flows                 as F
import qualified NITTA.FunctionBlocks        as FB
import qualified NITTA.ProcessUnits.Accum    as A
import           NITTA.ProcessUnits.Fram
import           NITTA.ProcessUnits.FramSpec
import           NITTA.ProcessUnitsSpec
import           NITTA.TestBench
import           NITTA.Types
import           System.Environment
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck       as QC


main = do
  setEnv "TASTY_QUICKCHECK_TESTS" "10"
  -- TODO: Это не порядок. Необходимо:
  -- 1) распараллелить (убрать колизии по файлам при симуляции);
  -- 2) в ram fs положить.
  setEnv "TASTY_NUM_THREADS" "1"
  defaultMain $ testGroup "NITTA tests"
    [ testGroup "Fram unittests" huFramTests
    , testGroup "Fram quickcheck" qcFramTests
    , testGroup "BusNetwork unittests" [ accum_fram1_fram2_netTests ]
    ]


qcFramTests
  = [ QC.testProperty "Сompleteness with a maximum load"
      (prop_formalCompletness . bindAllAndNaiveSchedule :: FramIdealDataFlow -> Bool)
    , QC.testProperty "Сompleteness with a partial load"
      (prop_formalCompletness . framDataFlow :: FramDataFlow -> Bool)
    , QC.testProperty "Simulation with a maximum load"
      (prop_simulation . bindAllAndNaiveSchedule :: FramIdealDataFlow -> Property)
    , QC.testProperty "Simulation with a partial load"
      (prop_simulation . framDataFlow :: FramDataFlow -> Property)
    ]

huFramTests
  = let alg = [ FB.reg (I "aa") $ O ["ab"]
              , FB.framOutput 9 $ I "ac"
              ]
        fram = (def :: Fram String Int)
        fram' = C.bindAllAndNaiveSchedule fram alg
    in [ testCase "Simple unit test" $ assert (testBench fram' [("aa", 42), ("ac", 0x1009)])
       ]



type T = TaggedTime String Int

accum_fram1_fram2_netTests
  = let fram = PU (def :: Fram String T)
        accum = PU (def :: A.Accum String T)
        net = busNetwork
          [ ("fram1", fram)
          , ("fram2", fram)
          , ("accum", accum)
          ]
          $ array (0, 19) [ (19, [("accum", S $ (A.NEG  :: Signal (A.Accum String T)))])
                          , (18, [("accum", S $ (A.LOAD :: Signal (A.Accum String T)))])
                          , (17, [("accum", S $ (A.INIT :: Signal (A.Accum String T)))])
                          , (16, [("accum", S $ (A.OE   :: Signal (A.Accum String T)))])

                          , (15, [("fram1", S $ (OE :: Signal (Fram String T)))])
                          , (14, [("fram1", S $ (WR :: Signal (Fram String T)))])
                          , (13, [])
                          , (12, [])

                          , (11, [("fram1", S $ (ADDR 3 :: Signal (Fram String T)))])
                          , (10, [("fram1", S $ (ADDR 2 :: Signal (Fram String T)))])
                          , ( 9, [("fram1", S $ (ADDR 1 :: Signal (Fram String T)))])
                          , ( 8, [("fram1", S $ (ADDR 0 :: Signal (Fram String T)))])

                          , ( 7, [("fram2", S $ (OE :: Signal (Fram String T)))])
                          , ( 6, [("fram2", S $ (WR :: Signal (Fram String T)))])
                          , ( 5, [])
                          , ( 4, [])

                          , ( 3, [("fram2", S $ (ADDR 3 :: Signal (Fram String T)))])
                          , ( 2, [("fram2", S $ (ADDR 2 :: Signal (Fram String T)))])
                          , ( 1, [("fram2", S $ (ADDR 1 :: Signal (Fram String T)))])
                          , ( 0, [("fram2", S $ (ADDR 0 :: Signal (Fram String T)))])
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
        net' = C.bindAll (net :: BusNetwork String String T) alg
        compiler = F.Branch net' (dataFlow2controlFlow $ F.Stage $ map Actor alg) Nothing []
        F.Branch{ topPU=net''
                } = foldl (\comp _ -> C.naive def comp) compiler (take 150 $ repeat ())
    in testCase "Obscure integration test for net of accum, fram1, fram2"
          $ assert $ testBench net'' ([] :: [(String, Int)])
