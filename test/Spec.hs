{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : Spec
Description : Test specification
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module Spec where

import Data.Maybe
import qualified NITTA.Intermediate.Functions.Accum.Tests
import qualified NITTA.Intermediate.Simulation.Tests
import qualified NITTA.Intermediate.Value.Tests
import qualified NITTA.LuaFrontend.Tests
import qualified NITTA.Model.Problems.Refactor.Accum.Tests
import qualified NITTA.Model.Problems.Refactor.ConstantFolding.Tests
import qualified NITTA.Model.Problems.Refactor.Tests
import qualified NITTA.Model.ProcessorUnits.Accum.Tests
import qualified NITTA.Model.ProcessorUnits.Broken.Tests
import qualified NITTA.Model.ProcessorUnits.Divider.Tests
import qualified NITTA.Model.ProcessorUnits.Fram.Tests
import qualified NITTA.Model.ProcessorUnits.IO.SPI.Tests
import qualified NITTA.Model.ProcessorUnits.Multiplier.Tests
import qualified NITTA.Model.ProcessorUnits.Shift.Tests
import qualified NITTA.Model.ProcessorUnits.Tests.DSL.Tests
import qualified NITTA.Tests
import qualified NITTA.Utils.Tests
import System.Environment (lookupEnv, setEnv)
import Test.Tasty (testGroup)
import Test.Tasty.Ingredients.Rerun

main = do
    qtests <- fromMaybe "10" <$> lookupEnv "TASTY_QUICKCHECK_TESTS"
    setEnv "TASTY_QUICKCHECK_TESTS" qtests
    defaultMainWithRerun $
        testGroup
            "NITTA"
            [ NITTA.Intermediate.Functions.Accum.Tests.tests
            , NITTA.Intermediate.Simulation.Tests.tests
            , NITTA.Intermediate.Value.Tests.tests
            , NITTA.LuaFrontend.Tests.tests
            , NITTA.Model.Problems.Refactor.Tests.tests
            , NITTA.Model.Problems.Refactor.Accum.Tests.tests
            , NITTA.Model.Problems.Refactor.ConstantFolding.Tests.tests
            , NITTA.Model.ProcessorUnits.Broken.Tests.tests
            , NITTA.Model.ProcessorUnits.Divider.Tests.tests
            , NITTA.Model.ProcessorUnits.Fram.Tests.tests
            , NITTA.Model.ProcessorUnits.IO.SPI.Tests.tests
            , NITTA.Model.ProcessorUnits.Multiplier.Tests.tests
            , NITTA.Model.ProcessorUnits.Accum.Tests.tests
            , NITTA.Model.ProcessorUnits.Shift.Tests.tests
            , NITTA.Model.ProcessorUnits.Tests.DSL.Tests.tests
            , NITTA.Tests.tests
            , NITTA.Utils.Tests.tests
            ]
