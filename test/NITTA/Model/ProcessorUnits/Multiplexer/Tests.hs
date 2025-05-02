{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module NITTA.Model.ProcessorUnits.Multiplexer.Tests (
    tests,
) where

import Data.Default
import qualified Data.Text as T
import qualified NITTA.Intermediate.Functions as F
import NITTA.Model.ProcessorUnits.Multiplexer
import NITTA.Model.ProcessorUnits.Tests.Providers
import NITTA.Model.TargetSystem ()
import Test.QuickCheck
import Test.Tasty (testGroup)

tests =
    testGroup
        "Multiplexer PU"
        [ puCoSimTestCase
            "sel_first_input"
            u
            [("a", 10), ("b", 20), ("sel", 0)]
            [F.mux (T.pack "a") (T.pack "b") (T.pack "sel") [T.pack "out"]]
        , puCoSimTestCase
            "sel_second_input"
            u
            [("a", 10), ("b", 20), ("sel", 1)]
            [F.mux (T.pack "a") (T.pack "b") (T.pack "sel") [T.pack "out"]]
        , -- Property-based tests
          finitePUSynthesisProp
            "finite synthesis properties"
            u
            muxGen
        , puCoSimPropWithContext
            "co simulation properties"
            u
            muxWithContextGen
        ]
    where
        u = def :: Multiplexer T.Text Int Int
        muxGen = algGen [fmap packF (arbitrary :: Gen (F.Mux T.Text Int))]

        muxWithContextGen = do
            (m, cntx) <- arbitrary :: Gen ([F.Mux T.Text Int], Cntx T.Text Int)
            return (fmap packF m, cntx)
