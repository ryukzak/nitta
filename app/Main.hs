{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

module Main where

import           Control.Monad
import           Data.Array              (array)
import           Data.Default
import           Data.Map                (fromList)
import           NITTA.Base
import           NITTA.BusNetwork
import           NITTA.Compiler
import qualified NITTA.FunctionBlocks    as FB
import           NITTA.ProcessUnits.FRAM
import           NITTA.Timeline


fram = PU fram'
fram' = def{ frMemory=fromList [ (addr, def) | addr <- [0..10] ]
           } :: FRAM Int Passive String Int

net = busNetwork
  [ ("fram1", fram)
  , ("fram2", fram)
  ]
  $ array (0, 15) [ (15, [("fram1", S $ (OE :: Signals (FRAM Int)))])
                  , (14, [("fram1", S $ (WR :: Signals (FRAM Int)))])
                  , (13, [])
                  , (12, [])

                  , (11, [("fram1", S $ (ADDR 3 :: Signals (FRAM Int)))])
                  , (10, [("fram1", S $ (ADDR 2 :: Signals (FRAM Int)))])
                  , ( 9, [("fram1", S $ (ADDR 1 :: Signals (FRAM Int)))])
                  , ( 8, [("fram1", S $ (ADDR 0 :: Signals (FRAM Int)))])

                  , ( 7, [("fram2", S $ (OE :: Signals (FRAM Int)))])
                  , ( 6, [("fram2", S $ (WR :: Signals (FRAM Int)))])
                  , ( 5, [])
                  , ( 4, [])

                  , ( 3, [("fram2", S $ (ADDR 3 :: Signals (FRAM Int)))])
                  , ( 2, [("fram2", S $ (ADDR 2 :: Signals (FRAM Int)))])
                  , ( 1, [("fram2", S $ (ADDR 1 :: Signals (FRAM Int)))])
                  , ( 0, [("fram2", S $ (ADDR 0 :: Signals (FRAM Int)))])
                  ]

alg = [ FB.framInput 3 [ "a" ]
      , FB.framOutput 2 "z"
      , FB.reg "a" ["x"]
      , FB.framInput 8 [ "b"
                       , "c"
                       ]
      , FB.reg "b" ["y"]
      , FB.reg "c" ["z"]
      , FB.framOutput 0 "x"
      , FB.framOutput 1 "y"
      ]

net' = eval (net :: BusNetwork String (Network String) String Int) alg

bindedNet =
  let ni1 = foldl (\s (fb, dpu) -> subBind fb dpu s) net' [ (alg !! 0, "fram1")
                                                          , (alg !! 1, "fram1")
                                                          , (alg !! 2, "fram2")
                                                          , (alg !! 3, "fram2")
                                                          , (alg !! 4, "fram2")
                                                          , (alg !! 5, "fram1")
                                                          , (alg !! 6, "fram1")
                                                          , (alg !! 7, "fram1")
                                                          ]
  in ni1

---------------------------------------------------------------------------------

eval pu fbs =
  let Just pu' = foldl (\(Just s) n -> bind s n) (Just pu) fbs
  in pu'
doSteps pu acts = foldl (\s n -> step s n) pu acts

main = do
  -- let fram0 = eval fram' [ FB.framOutput 0 "a" -- save 0main
                         -- , FB.framInput 0 ["a'"] -- load 0
                         -- , FB.reg "b" ["b'"]
                         -- , FB.reg "c" ["c'", "c''"]
                         -- ]
  -- let fram1 = doSteps fram0 [ PUAct (Pull ["a'"])  $ Event 1  2
                            -- , PUAct (Push "c")     $ Event 5  2
                            -- , PUAct (Push "a")     $ Event 10 2
                            -- , PUAct (Pull ["c'"])  $ Event 15 2
                            -- , PUAct (Push "b")     $ Event 20 2
                            -- , PUAct (Pull ["c''"]) $ Event 25 2
                            -- , PUAct (Pull ["b'"])  $ Event 30 2
                            -- ]
  -- writeTestBench fram1

  test <- foldM (\s _ -> naive s) net' $ take 20 $ repeat ()
  -- test <- foldM (\s _ -> naiveStep s) bindedNet $ take 6 $ repeat ()
  timeline "resource/data.json" test
  writeTestBench test
