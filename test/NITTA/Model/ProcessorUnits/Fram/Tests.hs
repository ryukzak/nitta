{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-partial-type-signatures #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Fram.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Fram.Tests (
    tests,
) where

import Data.Default
import Data.Text qualified as T
import NITTA.Intermediate.Functions qualified as F
import NITTA.Model.ProcessorUnits.Tests.Providers
import Test.QuickCheck
import Test.Tasty (testGroup)

tests =
    testGroup
        "Fram PU"
        [ puCoSimTestCase
            "buffer function"
            u
            [("a", 42)]
            [F.buffer "a" ["b"]]
        , puCoSimTestCase
            "constant function"
            u
            []
            [F.constant 11 ["ovj"]]
        , unitTestCase "test BreakLoop" u2 $ do
            assign $ F.loop 10 "b" ["a"]
            setValue "b" 64
            mkBreakLoop 10 "b" ["a"] >>= \r -> refactor r
            decideAt 1 1 $ provide ["a"]
            decideAt 2 2 $ consume "b"
            traceProcess
            assertPUCoSimulation
        , puCoSimTestCase
            "loop function"
            u
            [("b", 42)]
            [F.loop 10 "b" ["a"]]
        , -- TODO: not available, because needed self transaction
          -- , unitCoSimulationTestCase "loop_reg" u []
          --     [ buffer "a" ["b"]
          --     , loop 10 "b" ["a"]
          --     ]
          finitePUSynthesisProp "finite synthesis properties" u fsGen
        , puCoSimProp "co simulation properties" u fsGen
        , puCoSimTestCase
            "buffer function with Attr"
            u2
            [("a", Attr 42 True)]
            [F.buffer "a" ["b"]]
        , puCoSimTestCase
            "constant function with Attr"
            u2
            []
            [F.constant (Attr 42 True) ["a"]]
        , puCoSimProp "co simulation properties with attr" u2 fsGen
        ]
    where
        u = def :: Fram T.Text Int Int
        u2 = def :: Fram T.Text (Attr (IntX 32)) Int
        fsGen =
            algGen
                [ fmap packF (arbitrary :: Gen (F.Constant _ _))
                , fmap packF (arbitrary :: Gen (F.Loop _ _))
                , fmap packF (arbitrary :: Gen (F.Buffer _ _))
                ]
