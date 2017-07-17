-- {-# OPTIONS -Wall -fno-warn-missing-signatures #-}
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
import           Data.Map                (fromList, (!))
import           Data.Maybe
import           Data.Typeable
import           NITTA.Base
import           NITTA.BusNetwork
import           NITTA.Compiler
import qualified NITTA.FunctionBlocks    as FB
import           NITTA.ProcessUnits.FRAM
import           NITTA.Timeline


fram = PU fram'
fram' = def :: FRAM Passive String Int

net = busNetwork
  [ ("fram1", fram)
  , ("fram2", fram)
  ]
  $ array (0, 15) [ (15, [("fram1", S $ (OE :: Signals FRAM))])
                  , (14, [("fram1", S $ (WR :: Signals FRAM))])
                  , (13, [])
                  , (12, [])

                  , (11, [("fram1", S $ (ADDR 3 :: Signals FRAM))])
                  , (10, [("fram1", S $ (ADDR 2 :: Signals FRAM))])
                  , ( 9, [("fram1", S $ (ADDR 1 :: Signals FRAM))])
                  , ( 8, [("fram1", S $ (ADDR 0 :: Signals FRAM))])

                  , ( 7, [("fram2", S $ (OE :: Signals FRAM))])
                  , ( 6, [("fram2", S $ (WR :: Signals FRAM))])
                  , ( 5, [])
                  , ( 4, [])

                  , ( 3, [("fram2", S $ (ADDR 3 :: Signals FRAM))])
                  , ( 2, [("fram2", S $ (ADDR 2 :: Signals FRAM))])
                  , ( 1, [("fram2", S $ (ADDR 1 :: Signals FRAM))])
                  , ( 0, [("fram2", S $ (ADDR 0 :: Signals FRAM))])
                  ]

alg = [ FB.framInput 3 [ "a" ]
      , FB.framOutput 2 "z"
      , FB.loop "g" ["f"]
      , FB.reg "f" ["g"]
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
  --                        , FB.framInput 0 ["a'"] -- load 0
  --                        , FB.reg "b" ["b'"]
  --                        , FB.reg "c" ["c'", "c''"]
  --                        ]
  -- let fram1 = doSteps fram0 [ PUAct (Push "b")     $ Event 0  1
  -- --                           , PUAct (Pull ["c''"]) $ Event 2 1
  -- --                           -- , PUAct (Pull ["a'"])  $ Event 0  1
  -- --                           -- , PUAct (Pull ["c'"])  $ Event 8 1
  -- --                           -- , PUAct (Push "a")     $ Event 10 2
  -- --                           -- , PUAct (Push "b")     $ Event 20 2
  -- --                           -- , PUAct (Pull ["b'"])  $ Event 30 2
  --                           ]
  -- mapM_ (putStrLn . show) $ variants fram1
  -- -- writeTestBench fram1

  let test = foldl (\s _ -> naive s) net' $ take 30 $ repeat ()
  timeline "resource/data.json" test
  writeTestBench (getPU "fram1" test :: FRAM Passive String Int)

getPU puTitle net = case niPus net ! puTitle of
  PU pu -> fromMaybe undefined $ cast pu
