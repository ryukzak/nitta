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
import           Data.Map                (fromList, (!))
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


fram = PU fram'
fram' = def :: Fram Passive String Int

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
      , FB.loop "g" ["f"]
      , FB.reg "f" ["g"]
      ]

net' = bindAll (net :: BusNetwork String (Network String) String Int) alg


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


main = do
  -- let fram0 = eval fram' [ FB.framOutput 0 "a" -- save 0main
  --                        , FB.framInput 0 ["a'"] -- load 0
  --                        , FB.reg "b" ["b'"]
  --                        , FB.loop "c" ["c'"]
  --                        ]
  -- let fram1 = doSteps fram0 [ PUAct (Pull ["a'"]) $ Event 0 1
  --                           , PUAct (Push "a")    $ Event 2 1
  --                           , PUAct (Pull ["c'"]) $ Event 4 1
  --                           , PUAct (Push "b")    $ Event 6 1
  --                           , PUAct (Push "c")    $ Event 8 1
  --                           , PUAct (Pull ["b'"]) $ Event 10 1
  --                           ]
  -- mapM_ (putStrLn . show) $ take 3 $ elems $ frMemory fram1
  -- mapM_ (putStrLn . show) $ variants fram1
  -- writeTestBench fram1

  -- let fram = naive0 (def:: Fram Passive String Int) alg0
  -- timeline "resource/data.json" fram


  test <- foldM (\s _ -> naive s) net' $ take 40 $ repeat ()
  timeline "resource/data.json" test
  -- testBench (getPU "fram1" test :: Fram Passive String Int)
    -- [ ("a", 0xFF0A), ("x", 0xFF0A)
    -- , ("b", 0xFF0B), ("y", 0xFF0B)
    -- , ("c", 0xFF0C), ("z", 0xFF0C)
    -- , ("f", 0xFF0F), ("g", 0xFF0F)
    -- ]

  testBench (getPU "fram2" test :: Fram Passive String Int)
    [ ("a", 0x0A03), ("x", 0xFF0A)
    , ("b", 0x0A04), ("y", 0xFF0B)
    , ("c", 0x0A04), ("z", 0xFF0C)
    , ("f", 0x0A00) -- потому что Loop должен попасть по адресу 0, так как он свободен.
    , ("g", 0xFF0F)
    ]


  -- writeTestBench test
  --   [ ("a", 0x00B3)
  --   , ("b", 0x00B4)
  --   , ("c", 0x00B4)

  --   , ("x", 0x00B3)
  --   , ("y", 0x00B4)
  --   , ("z", 0x00B4)

  --   , ("f", 0x00B0)
  --   , ("g", 0x00B0)
  --   ]


getPU puTitle net = case niPus net ! puTitle of
  PU pu -> fromMaybe undefined $ cast pu
