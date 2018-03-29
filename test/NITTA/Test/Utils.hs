{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Test.Utils where

import           Data.Set             (Set, fromList)
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
  let f = algInputs :: [FB (Parcel String Int)] -> Set String
  fromList []         @=? f [ FB $ FramInput 3 $ O $ fromList [ "a" ] ]
  fromList ["a", "b"] @=? f [ FB $ Add (I "a") (I "b") (O $ fromList ["c"]) ]
  fromList ["c"]      @=? f [ FB $ FramOutput 0 $ I "c" ]
  fromList []         @=? f [ FB $ FramInput 3 $ O $ fromList [ "a" ]
                            , FB $ FramInput 4 $ O $ fromList [ "b" ]
                            , FB $ Add (I "a") (I "b") (O $ fromList ["c"])
                            , FB $ FramOutput 0 $ I "c"
                            ]

outputsOfFBsTests = do
  let f = algOutputs :: [FB (Parcel String Int)] -> Set String
  fromList ["a"] @=? f [ FB $ FramInput 3 $ O $ fromList [ "a" ] ]
  fromList ["c"] @=? f [ FB $ Add (I "a") (I "b") (O $ fromList ["c"]) ]
  fromList []    @=? f [ FB $ FramOutput 0 $ I "c" ]
  fromList []    @=? f [ FB $ FramInput 3 $ O $ fromList [ "a" ]
                       , FB $ FramInput 4 $ O $ fromList [ "b" ]
                       , FB $ Add (I "a") (I "b") (O $ fromList ["c"])
                       , FB $ FramOutput 0 $ I "c"
                       ]

endpointRoleEq = do
  let source = Source . fromList
  Target "a" == Target "a" @? "Target eq"
  Target "a" /= Target "b" @? "Target not eq"
  source ["a"] /= Target "b" @? "Target not eq Source"
  source ["a"] == source ["a"] @? "Source eq"
  source ["a", "b"] == source ["a", "b"] @? "Source eq"
  source ["b", "a"] == source ["a", "b"] @? "Source eq"
  source ["b", "a"] /= source ["a", "c"] @? "Source not eq"
