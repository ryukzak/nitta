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
import           Data.Array              (array, elems)
import           Data.Default
import           Data.Map                (assocs, fromList, (!))
import           Data.Maybe
import           Data.Typeable
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.FunctionBlocks
import qualified NITTA.FunctionBlocks    as FB
import           NITTA.ProcessUnits.Fram
import           NITTA.TestBench
import           NITTA.Timeline
import           NITTA.Types
import           NITTA.Utils


fram = PU (def :: Fram Passive String Int)

net = busNetwork
  [ ("fram1", fram)
  , ("fram2", fram)
  ]
  $ array (0, 15) [ (15, [("fram1", S $ (OE :: Signals Fram))])
                  , (14, [("fram1", S $ (WR :: Signals Fram))])
                  , (13, [])
                  , (12, [])

                  , (11, [("fram1", S $ (ADDR 3 :: Signals Fram))])
                  , (10, [("fram1", S $ (ADDR 2 :: Signals Fram))])
                  , ( 9, [("fram1", S $ (ADDR 1 :: Signals Fram))])
                  , ( 8, [("fram1", S $ (ADDR 0 :: Signals Fram))])

                  , ( 7, [("fram2", S $ (OE :: Signals Fram))])
                  , ( 6, [("fram2", S $ (WR :: Signals Fram))])
                  , ( 5, [])
                  , ( 4, [])

                  , ( 3, [("fram2", S $ (ADDR 3 :: Signals Fram))])
                  , ( 2, [("fram2", S $ (ADDR 2 :: Signals Fram))])
                  , ( 1, [("fram2", S $ (ADDR 1 :: Signals Fram))])
                  , ( 0, [("fram2", S $ (ADDR 0 :: Signals Fram))])
                  ]

alg = [ FB.framInput 3 [ "a" ]
      , FB.framInput 4 [ "b"
                       , "c"
                       ]
      , FB.reg "a" ["x"]
      , FB.reg "b" ["y"]
      , FB.reg "c" ["z"]
      , FB.framOutput 5 "x"
      , FB.framOutput 6 "y"
      , FB.framOutput 7 "z"
      , FB.loop ["f"] "g"
      , FB.reg "f" ["g"]
      ]


data Program v
  = Statement (FB v)
  | DataFlow [Program v]
  | Switch
    { conduction :: v
    , inputs     :: [v]
    , outputs    :: [v]
    , branchs    :: [(Int, Program v)]
    }
  deriving ( Show )

instance ( Var v ) => Vars (Program v) v where
  variables (DataFlow ps)  = concatMap variables ps
  variables (Statement fb) = variables fb
  variables Switch{..}     = conduction : inputs ++ outputs

program = DataFlow
  [ Statement $ FB.framInput 3 [ "a" ]
  , Statement $ FB.framInput 4 [ "b" , "c" ]
  , Statement $ FB.reg "a" ["x"]
  , Statement $ FB.reg "b" ["y"]
  , Statement $ FB.reg "c" ["z"]
  , Statement $ FB.framOutput 5 "x"
  , Statement $ FB.framOutput 6 "y"
  , Switch "x" ["z"] []
    [ (0, Statement $ FB.framOutput 10 "z")
    , (1, Statement $ FB.framOutput 11 "z")
    ]
  , Statement $ FB.loop ["f"] "g"
  , Statement $ FB.reg "f" ["g"]
  ]


net' = bindAll (net :: BusNetwork String (Network String) String Int) alg

---------------------------------------------------------------------------------

main = do
  test <- foldM (\s _ -> naive s) net' $ take 40 $ repeat ()
  timeline "resource/data.json" test
  mapM_ (putStrLn . show) $ steps $ process (getPU "fram2" test :: Fram Passive String Int)

  -- let cntx = simulate test ([] :: [(String, Int)])
  -- mapM_ (putStrLn . ("> " ++) . show) $ assocs cntx
  testBench test ([] :: [(String, Int)])


getPU puTitle net = case bnPus net ! puTitle of
  PU pu -> fromMaybe undefined $ cast pu
