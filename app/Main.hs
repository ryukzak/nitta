-- {-# OPTIONS -Wall -fno-warn-missing-signatures #-}

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

module Main where

import           Control.Monad
import           Data.Array               (array, elems)
import qualified Data.Array               as A
import           Data.Default
import           Data.Map                 (Map, assocs, fromList, (!))
import qualified Data.Map                 as M
import           Data.Maybe
import           Data.Proxy
import           Data.String.Utils
import qualified Data.String.Utils        as S
import           Data.Typeable
import           Debug.Trace
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.Flows
import           NITTA.FunctionBlocks
import qualified NITTA.FunctionBlocks     as FB
import qualified NITTA.ProcessUnits.Accum as A
import           NITTA.ProcessUnits.Fram
import           NITTA.TestBench
import           NITTA.Timeline
import           NITTA.Types
import           NITTA.Utils
import           Text.StringTemplate

type T = TaggedTime String Int

fram = PU (def :: Fram String T)
accum = PU (def :: A.Accum String T)


net0 = busNetwork
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


-- alg = [ FB.framInput 3 $ O ["a"]
--       , FB.framInput 4 $ O [ "b"
--                            , "c"
--                            ]
--       , FB.reg (I "a") $ O ["x"]
--       , FB.reg (I "b") $ O ["y"]
--       , FB.reg (I "c") $ O ["z"]
--       , FB.framOutput 5 $ I "x"
--       , FB.framOutput 6 $ I "y"
--       , FB.framOutput 7 $ I "z"
--       , FB.loop (O ["f"]) $ I "g"
--       , FB.reg (I "f") $ O ["g"]
--       ]

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
      , FB $ Add (I "d") (I "e") (O ["sum"])
      ]


-- alg = [ FB.framInput 1 $ O [ "a" ]
--       , FB.framInput 2 $ O [ "b" ]
--       , FB $ Add (I "a") (I "b") (O ["sum"])
--       , FB.framOutput 0 $ I "sum"
--       ]


-- program = DataFlow
--   [ Statement $ FB.framInput 0 $ O [ "cond", "cond'" ]
--   , Statement $ FB.framInput 1 $ O [ "x1", "x2" ]
--   , Statement $ FB.framOutput 2 $ I "cond'"

--   , Switch "cond"
--     [ (0, DataFlow [ Statement $ FB.reg (I "x1") $ O ["y1"], Statement $ FB.framOutput 10 $ I "y1" ])
--     , (1, DataFlow [ Statement $ FB.reg (I "x2") $ O ["y2"], Statement $ FB.framOutput 11 $ I "y2" ])
--     ]
--   ]

program = DataFlow $ map Statement alg


net' = bindAll (net0 :: BusNetwork String (PU Passive String T) String T) alg
-- net'' = bindAll (net0 :: BusNetwork String (PU Passive String T) String T) $ functionalBlocks program

---------------------------------------------------------------------------------

main = do
  let compiler = Fork net' (def{ controlFlow=mkControlFlow $ DataFlow $ map Statement alg }) Nothing []
  let Fork{ net=pu
          , controlModel=cm'
          } = foldl (\comp _ -> naive comp) compiler (take 150 $ repeat ())

  timeline "resource/data.json" pu
  r <- testBench pu ([] :: [(String, Int)])
  if r then putStrLn "Success"
  else putStrLn "Fail"

getPU puTitle net0
  = case bnPus net0 ! puTitle of
      PU pu | Just pu' <- cast pu -> pu'
