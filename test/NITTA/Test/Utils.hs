{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Test.Utils where

import           NITTA.FunctionBlocks
import           NITTA.Types
import           NITTA.Utils
import           Test.Tasty.HUnit

values2dumpTests = do
  "0" @=? values2dump [X, X, X, X]
  "0" @=? values2dump [B False, B False, B False, B False]
  "f" @=? values2dump [B True, B True, B True, B True]
  "17" @=? values2dump [B True, B False, B True, B True, B True]

inputsOfFBsTests = do
  let f = algInputs :: [FB (Parcel String)] -> [String]
  []         @=? f [ FB $ FramInput 3 $ O [ "a" ] ]
  ["a", "b"] @=? f [ FB $ Add (I "a") (I "b") (O ["c"]) ]
  ["c"]      @=? f [ FB $ FramOutput 0 $ I "c" ]
  []         @=? f [ FB $ FramInput 3 $ O [ "a" ]
                   , FB $ FramInput 4 $ O [ "b" ]
                   , FB $ Add (I "a") (I "b") (O ["c"])
                   , FB $ FramOutput 0 $ I "c"
                   ]

outputsOfFBsTests = do
  let f = algOutputs :: [FB (Parcel String)] -> [String]
  ["a"] @=? f [ FB $ FramInput 3 $ O [ "a" ] ]
  ["c"] @=? f [ FB $ Add (I "a") (I "b") (O ["c"]) ]
  []    @=? f [ FB $ FramOutput 0 $ I "c" ]
  []    @=? f [ FB $ FramInput 3 $ O [ "a" ]
              , FB $ FramInput 4 $ O [ "b" ]
              , FB $ Add (I "a") (I "b") (O ["c"])
              , FB $ FramOutput 0 $ I "c"
              ]
