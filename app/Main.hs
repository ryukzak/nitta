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

net = busNetwork
  [ ("fram1", fram)
  , ("fram2", fram)
  , ("accum", accum)
  ]
  $ array (0, 19) [ (19, [("accum", S (A.NEG  :: Signal (A.Accum String T)))])
                  , (18, [("accum", S (A.LOAD :: Signal (A.Accum String T)))])
                  , (17, [("accum", S (A.INIT :: Signal (A.Accum String T)))])
                  , (16, [("accum", S (A.OE   :: Signal (A.Accum String T)))])

                  , (15, [("fram1", S (OE :: Signal (Fram String T)))])
                  , (14, [("fram1", S (WR :: Signal (Fram String T)))])
                  , (13, [])
                  , (12, [])

                  , (11, [("fram1", S (ADDR 3 :: Signal (Fram String T)))])
                  , (10, [("fram1", S (ADDR 2 :: Signal (Fram String T)))])
                  , ( 9, [("fram1", S (ADDR 1 :: Signal (Fram String T)))])
                  , ( 8, [("fram1", S (ADDR 0 :: Signal (Fram String T)))])

                  , ( 7, [("fram2", S (OE :: Signal (Fram String T)))])
                  , ( 6, [("fram2", S (WR :: Signal (Fram String T)))])
                  , ( 5, [])
                  , ( 4, [])

                  , ( 3, [("fram2", S (ADDR 3 :: Signal (Fram String T)))])
                  , ( 2, [("fram2", S (ADDR 2 :: Signal (Fram String T)))])
                  , ( 1, [("fram2", S (ADDR 1 :: Signal (Fram String T)))])
                  , ( 0, [("fram2", S (ADDR 0 :: Signal (Fram String T)))])
                  ]


dataFlow = Stage
  [ Actor $ FB.framInput 0 $ O [ "cond", "cond'" ]
  , Actor $ FB.framInput 1 $ O [ "x1", "x2" ]
  , Actor $ FB.framOutput 2 $ I "cond'"
  , Paths "cond"
    [ (0, Stage [ Actor $ FB.reg (I "x1") $ O ["y1"], Actor $ FB.framOutput 10 $ I "y1" ])
    , (1, Stage [ Actor $ FB.reg (I "x2") $ O ["y2"], Actor $ FB.framOutput 11 $ I "y2" ])
    ]
  ]

net' = bindAll (net :: BusNetwork String String T) $ functionalBlocks dataFlow


---------------------------------------------------------------------------------


main = do
  let compiler = Branch net' (dataFlow2controlFlow dataFlow) Nothing []
  let p = foldl (\comp _ -> naive def comp) compiler (take 50 $ repeat ())
  let Branch{ topPU=pu } = p
  -- let BranchingInProgress{ currentBranch=Fork{ topPU=pu } } = p

  -- putStrLn $ show $ graph dataFlow
  timeline "resource/data.json" pu
  -- r <- testBench pu ([] :: [(String, Int)])
  -- if r then putStrLn "Success"
  -- else putStrLn "Fail"
  print "ok"

getPU puTitle net
  = case bnPus net ! puTitle of
      PU pu | Just pu' <- cast pu -> pu'
