{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Spec
Description : Test specification
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module Spec where

import Data.FileEmbed (embedStringFile)
import Data.Maybe
import NITTA.Frontends.Lua.Tests qualified
import NITTA.Frontends.Lua.Tests.Providers
import NITTA.Frontends.XMILE.DocumentParserTests qualified
import NITTA.Frontends.XMILE.MathParserTests qualified
import NITTA.Intermediate.Functions.Accum.Tests qualified
import NITTA.Intermediate.Simulation.Tests qualified
import NITTA.Intermediate.Value.Tests qualified
import NITTA.Model.Problems.Refactor.Accum.Tests qualified
import NITTA.Model.Problems.Refactor.ConstantFolding.Tests qualified
import NITTA.Model.Problems.Refactor.Tests qualified
import NITTA.Model.ProcessorUnits.Accum.Tests qualified
import NITTA.Model.ProcessorUnits.Broken.Tests qualified
import NITTA.Model.ProcessorUnits.Compare.Tests qualified
import NITTA.Model.ProcessorUnits.Divider.Tests qualified
import NITTA.Model.ProcessorUnits.Fram.Tests qualified
import NITTA.Model.ProcessorUnits.IO.SPI.Tests qualified
import NITTA.Model.ProcessorUnits.LUT.Tests qualified
import NITTA.Model.ProcessorUnits.Multiplexer.Tests qualified
import NITTA.Model.ProcessorUnits.Multiplier.Tests qualified
import NITTA.Model.ProcessorUnits.Shift.Tests qualified
import NITTA.Model.ProcessorUnits.Tests.DSL.Tests qualified
import NITTA.Tests qualified
import NITTA.Utils.Tests qualified
import System.Environment (lookupEnv, setEnv)
import Test.Tasty (testGroup)
import Test.Tasty.Ingredients.Rerun

main = do
    qtests <- fromMaybe "10" <$> lookupEnv "TASTY_QUICKCHECK_TESTS"
    setEnv "TASTY_QUICKCHECK_TESTS" qtests
    ci <- fromMaybe "" <$> lookupEnv "CI"
    defaultMainWithRerun
        $ testGroup
            "NITTA"
        $ [ NITTA.Intermediate.Functions.Accum.Tests.tests
          , NITTA.Intermediate.Simulation.Tests.tests
          , NITTA.Intermediate.Value.Tests.tests
          , NITTA.Frontends.Lua.Tests.tests
          , NITTA.Frontends.XMILE.MathParserTests.tests
          , NITTA.Frontends.XMILE.DocumentParserTests.tests
          , NITTA.Model.Problems.Refactor.Tests.tests
          , NITTA.Model.Problems.Refactor.Accum.Tests.tests
          , NITTA.Model.Problems.Refactor.ConstantFolding.Tests.tests
          , NITTA.Model.ProcessorUnits.Broken.Tests.tests
          , NITTA.Model.ProcessorUnits.Divider.Tests.tests
          , NITTA.Model.ProcessorUnits.Fram.Tests.tests
          , NITTA.Model.ProcessorUnits.Compare.Tests.tests
          , NITTA.Model.ProcessorUnits.LUT.Tests.tests
          , NITTA.Model.ProcessorUnits.Multiplexer.Tests.tests
          , NITTA.Model.ProcessorUnits.IO.SPI.Tests.tests
          , NITTA.Model.ProcessorUnits.Multiplier.Tests.tests
          , NITTA.Model.ProcessorUnits.Accum.Tests.tests
          , NITTA.Model.ProcessorUnits.Shift.Tests.tests
          , NITTA.Model.ProcessorUnits.Tests.DSL.Tests.tests
          , NITTA.Tests.tests
          , NITTA.Utils.Tests.tests
          ]
            <> if ci == "true" then [ciOnlyTestGroup] else []

ciOnlyTestGroup =
    testGroup
        "CI only"
        [ typedLuaTestCase
            (microarch Sync SlaveSPI)
            pFX48_64
            "sin_ident"
            $(embedStringFile "examples/sin_ident/sin_ident.lua")
        ]
