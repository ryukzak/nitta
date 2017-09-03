{-# LANGUAGE Arrows                 #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- {-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module Main where

import           Control.Monad
import           Data.Array              (array, elems)
import           Data.Default
import           Data.Map                (assocs, fromList, (!))
import           Data.Maybe
import           Data.Typeable
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.Flows
import           NITTA.FunctionBlocks
import qualified NITTA.FunctionBlocks    as FB
import           NITTA.ProcessUnits.Fram
import           NITTA.TestBench
import           NITTA.Timeline
import           NITTA.Types
import           NITTA.Utils

type T = TaggetTime String Int

fram = PU (def :: Fram String T)


net0 = busNetwork
  [ ("fram1", fram)
  , ("fram2", fram)
  ]
  $ array (0, 15) [ (15, [("fram1", S $ (OE :: Signals (Fram String T)))])
                  , (14, [("fram1", S $ (WR :: Signals (Fram String T)))])
                  , (13, [])
                  , (12, [])

                  , (11, [("fram1", S $ (ADDR 3 :: Signals (Fram String T)))])
                  , (10, [("fram1", S $ (ADDR 2 :: Signals (Fram String T)))])
                  , ( 9, [("fram1", S $ (ADDR 1 :: Signals (Fram String T)))])
                  , ( 8, [("fram1", S $ (ADDR 0 :: Signals (Fram String T)))])

                  , ( 7, [("fram2", S $ (OE :: Signals (Fram String T)))])
                  , ( 6, [("fram2", S $ (WR :: Signals (Fram String T)))])
                  , ( 5, [])
                  , ( 4, [])

                  , ( 3, [("fram2", S $ (ADDR 3 :: Signals (Fram String T)))])
                  , ( 2, [("fram2", S $ (ADDR 2 :: Signals (Fram String T)))])
                  , ( 1, [("fram2", S $ (ADDR 1 :: Signals (Fram String T)))])
                  , ( 0, [("fram2", S $ (ADDR 0 :: Signals (Fram String T)))])
                  ]





-- algA = proc _ -> do
  -- x <- (arr (\() -> 11)) -< ()
  -- returnA -< x

alg = [ FB.framInput 3 $ O ["a"]
      , FB.framInput 4 $ O [ "b"
                           , "c"
                           ]
      , FB.reg (I "a") $ O ["x"]
      , FB.reg (I "b") $ O ["y"]
      , FB.reg (I "c") $ O ["z"]
      , FB.framOutput 5 $ I "x"
      , FB.framOutput 6 $ I "y"
      , FB.framOutput 7 $ I "z"
      , FB.loop (O ["f"]) $ I "g"
      , FB.reg (I "f") $ O ["g"]
      ]



program = DataFlow
  [ Statement $ FB.framInput 0 $ O [ "cond", "cond'" ]
  , Statement $ FB.framInput 1 $ O [ "x1", "x2" ]
  , Statement $ FB.framOutput 2 $ I "cond'"

  -- , Statement $ FB.reg (I "x1") $ O ["y1"]
  -- , Statement $ FB.framOutput 10 $ I "y1"
  -- , Statement $ FB.reg (I "x2") $ O ["y2"]
  -- , Statement $ FB.framOutput 11 $ I "y2"

  , Switch "cond"
    [ (0, DataFlow [ Statement $ FB.reg (I "x1") $ O ["y1"], Statement $ FB.framOutput 10 $ I "y1" ])
    , (1, DataFlow [ Statement $ FB.reg (I "x2") $ O ["y2"], Statement $ FB.framOutput 11 $ I "y2" ])
    ]
  ]

net' = bindAll (net0 :: BusNetwork String (PU Passive String T) String T) alg
net'' = bindAll (net0 :: BusNetwork String (PU Passive String T) String T) $ functionalBlocks program



---------------------------------------------------------------------------------

main = do
  let compiler = Fork net' (def{ controlFlow=mkControlFlow $ DataFlow $ map Statement alg }) Nothing []
  -- let compiler = Fork net'' (def{ controlFlow=mkControlFlow program }) Nothing []
  let Fork{ net=pu
          , controlModel=cm'
          } = foldl (\comp _ -> naive comp) compiler (take 15 $ repeat ())
  -- let Forks{ current=Fork{ net=pu
                         -- , controlModel=cm'
                         -- }
           -- } = foldl (\comp _ -> naive comp) compiler (take 15 $ repeat ())
  timeline "resource/data.json" pu
  -- print $ (getPU "fram2" pu :: Fram String T)
  -- mapM_ (putStrLn . show)
    -- $ steps $ process (getPU "fram2" pu :: Fram String T)

  -- don't work as expected...
  testBench pu ([] :: [(String, Int)])


getPU puTitle net0
  = case bnPus net0 ! puTitle of
      PU pu | Just pu' <- cast pu -> pu'

