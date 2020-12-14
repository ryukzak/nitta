{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-partial-type-signatures #-}

-- |
-- Module      : NITTA.Model.ProcessorUnits.Fram.Tests
-- Description :
-- Copyright   : (c) Aleksandr Penskoi, 2020
-- License     : BSD3
-- Maintainer  : aleksandr.penskoi@gmail.com
-- Stability   : experimental
module NITTA.Model.ProcessorUnits.Fram.Tests
  ( tests,
  )
where

import Data.Default
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Tests.Functions ()
import NITTA.Intermediate.Types
import NITTA.Model.ProcessorUnits.Fram
import NITTA.Model.ProcessorUnits.Tests.Utils
import Test.QuickCheck
import Test.Tasty (testGroup)

tests =
  testGroup
    "Fram PU"
    [ puCoSimTestCase
        "register function"
        u
        [("a", 42)]
        [reg "a" ["b"]],
      puCoSimTestCase
        "constant function"
        u
        []
        [constant 11 ["ovj"]],
      puCoSimTestCase
        "loop function"
        u
        [("b", 42)]
        [loop 10 "b" ["a"]],
      -- TODO: not available, because needed self transaction
      -- , unitCoSimulationTestCase "loop_reg" u []
      --     [ reg "a" ["b"]
      --     , loop 10 "b" ["a"]
      --     ]
      finitePUSynthesisProp "finite synthesis properties" u fsGen,
      puCoSimProp "co simulation properties" u fsGen,
      puCoSimTestCase
        "buffer function with Attr"
        u2
        [("a", Attr 42 True)]
        [reg "a" ["b"]],
      puCoSimTestCase
        "constant function with Attr"
        u2
        []
        [constant (Attr 42 True) ["a"]],
      puCoSimProp "co simulation properties with attr" u2 fsGen
    ]
  where
    u = def :: Fram String Int Int
    u2 = def :: Fram String (Attr (IntX 32)) Int
    fsGen =
      algGen
        [ fmap packF (arbitrary :: Gen (Constant _ _)),
          fmap packF (arbitrary :: Gen (Loop _ _)),
          fmap packF (arbitrary :: Gen (Reg _ _))
        ]
