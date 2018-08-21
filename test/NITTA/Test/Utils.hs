{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Test.Utils where

import           Data.Set         (Set, fromList)
import           NITTA.Functions
import           NITTA.Types
import           NITTA.Utils
import           Test.Tasty.HUnit


values2dumpTests = do
  "0" @=? values2dump [Undef, Undef, Undef, Undef]
  "0" @=? values2dump [Bool False, Bool False, Bool False, Bool False]
  "f" @=? values2dump [Bool True, Bool True, Bool True, Bool True]
  "17" @=? values2dump [Bool True, Bool False, Bool True, Bool True, Bool True]
  "000000" @=? values2dump (replicate 24 $ Bool False)

inputsOfFBsTests = do
  let f = algInputs :: [F (Parcel String Int)] -> Set String
  fromList []         @=? f [ framInput 3 [ "a" ] ] -- F $ FramInput 3 $ O $ fromList [ "a" ] ]
  fromList ["a", "b"] @=? f [ add "a" "b" ["c"] ]
  fromList ["c"]      @=? f [ framOutput 0 "c" ]
  fromList []         @=? f [ framInput 3 [ "a" ]
                            , framInput 4 [ "b" ]
                            , add "a" "b" ["c"]
                            , framOutput 0 "c"
                            ]

outputsOfFBsTests = do
  let f = algOutputs :: [F (Parcel String Int)] -> Set String
  fromList ["a"] @=? f [ framInput 3 [ "a" ] ]
  fromList ["c"] @=? f [ add "a" "b" ["c"] ]
  fromList []    @=? f [ framOutput 0 "c" ]
  fromList []    @=? f [ framInput 3 [ "a" ]
                       , framInput 4 [ "b" ]
                       , add "a" "b" ["c"]
                       , framOutput 0 "c"
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
