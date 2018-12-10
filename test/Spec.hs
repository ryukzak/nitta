{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module Main where

import           NITTA.Test.BusNetwork
import           NITTA.Test.Functions
import           NITTA.Test.LuaFrontend
import           NITTA.Test.ProcessUnits
import           NITTA.Test.Utils
import           System.Environment      (setEnv)
import           Test.Tasty              (defaultMain, testGroup)


-- FIXME: Тестирование очень активно работает с диском. В связи с этим рационально положить папку
-- hdl/gen в ramfs. Это и ускорит тестирование, и сбережёт железо. Необходимо это сделать для Linux,
-- но код должен корректно запускаться на Windows / OS X.
main = do
    -- FIXME: Сделать так, что бы при тестировании данная настройка могла определяться снаружи. А 10
    -- выставлялось только при быстром тестировании.
    setEnv "TASTY_QUICKCHECK_TESTS" "10"
    defaultMain $ testGroup "NITTA"
        [ utilTests
        , functionTests
        , processUnitTests
        , busNetworkTests
        , luaTests
        ]
