{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : Main
Description : Test specification
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module Main where

import           Data.Maybe
import qualified NITTA.Intermediate.Functions.Accum.Tests
import qualified NITTA.Intermediate.Simulation.Tests
import qualified NITTA.Intermediate.Types.Tests
import qualified NITTA.LuaFrontend.Tests
import qualified NITTA.Model.Problems.Refactor.Tests
import qualified NITTA.Model.ProcessorUnits.Divider.Tests
import qualified NITTA.Model.ProcessorUnits.Fram.Tests
import qualified NITTA.Model.ProcessorUnits.IO.SPI.Tests
import qualified NITTA.Model.ProcessorUnits.Multiplier.Tests
import qualified NITTA.Model.ProcessorUnits.Serial.Accum.Tests
import qualified NITTA.Model.ProcessorUnits.Serial.Shift.Tests
import qualified NITTA.Tests
import qualified NITTA.Utils.CodeFormat.Tests
import qualified NITTA.Utils.Tests
import           System.Environment                            (lookupEnv,
                                                                setEnv)
import           Test.Tasty                                    (defaultMain,
                                                                testGroup)


-- FIXME: Тестирование очень активно работает с диском. В связи с этим рационально положить папку
-- hdl/gen в ramfs. Это и ускорит тестирование, и сбережёт железо. Необходимо это сделать для Linux,
-- но код должен корректно запускаться на Windows / OS X.
main = do
    qtests <- fromMaybe "10" <$> lookupEnv "TASTY_QUICKCHECK_TESTS"
    setEnv "TASTY_QUICKCHECK_TESTS" qtests
    defaultMain $ testGroup "NITTA"
        [ NITTA.Intermediate.Functions.Accum.Tests.tests
        , NITTA.Intermediate.Simulation.Tests.tests
        , NITTA.Intermediate.Types.Tests.tests
        , NITTA.LuaFrontend.Tests.tests
        , NITTA.Model.Problems.Refactor.Tests.tests
        , NITTA.Model.ProcessorUnits.Divider.Tests.tests
        , NITTA.Model.ProcessorUnits.Fram.Tests.tests
        , NITTA.Model.ProcessorUnits.IO.SPI.Tests.tests
        , NITTA.Model.ProcessorUnits.Multiplier.Tests.tests
        , NITTA.Model.ProcessorUnits.Serial.Accum.Tests.tests
        , NITTA.Model.ProcessorUnits.Serial.Shift.Tests.tests
        , NITTA.Tests.tests
        , NITTA.Utils.CodeFormat.Tests.tests
        , NITTA.Utils.Tests.tests
        ]
