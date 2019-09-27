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
import           NITTA.Test.BusNetwork
import           NITTA.Test.FunctionSimulation
import           NITTA.Test.LuaFrontend
import           NITTA.Test.ProcessorUnits
import           NITTA.Test.Refactor
import           NITTA.Test.Types
import           NITTA.Test.Utils
import           System.Environment            (lookupEnv, setEnv)
import           Test.Tasty                    (defaultMain, testGroup)


-- FIXME: Тестирование очень активно работает с диском. В связи с этим рационально положить папку
-- hdl/gen в ramfs. Это и ускорит тестирование, и сбережёт железо. Необходимо это сделать для Linux,
-- но код должен корректно запускаться на Windows / OS X.
main = do
    qtests <- fromMaybe "10" <$> lookupEnv "TASTY_QUICKCHECK_TESTS"
    setEnv "TASTY_QUICKCHECK_TESTS" qtests
    defaultMain $ testGroup "NITTA"
        [ utilTests
        , typesTests
        , functionSimulationTests
        , processUnitTests
        , busNetworkTests
        , refactorTests
        , luaTests
        ]
