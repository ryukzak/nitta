{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module Main where

import           Control.Applicative           ((<$>))
import           Data.Atomics.Counter          (newCounter)
import           Data.Default                  (def)
import           NITTA.Functions
import           NITTA.ProcessUnits.Divider
import           NITTA.ProcessUnits.Fram
import           NITTA.ProcessUnits.Multiplier
import           NITTA.Test.BusNetwork
import           NITTA.Test.Functions
import           NITTA.Test.LuaFrontend
import           NITTA.Test.ProcessUnits
import           NITTA.Test.Utils
import           NITTA.Types
import           System.Environment            (setEnv)
import           Test.Tasty                    (defaultMain, testGroup)
import           Test.Tasty.QuickCheck         (Gen, arbitrary, testProperty)


-- FIXME: Тестирование очень активно работает с диском. В связи с этим рационально положить папку
-- hdl/gen в ramfs. Это и ускорит тестирование, и сбережёт железо. Необходимо это сделать для Linux,
-- но код должен корректно запускаться на Windows / OS X.
main = do
    counter <- newCounter 0 -- Используется для того, что бы раскладывать файлы в разные папки при симуляции.
    -- FIXME: Сделать так, что бы при тестировании данная настройка могла определяться снаружи. А 10
    -- выставлялось только при быстром тестировании.
    setEnv "TASTY_QUICKCHECK_TESTS" "10"
    defaultMain $ testGroup "NITTA"
        [ utilTests
        , functionTests
        , processUnitTests
        , busNetworkTests
        , luaTests
        , testGroup "Fram quickcheck"
            [ testProperty "isFinished" $ isFinished <$> framGen
            , testProperty "coSimulation" $ fmap (coSimulation "prop_simulation_fram" counter) $ inputsGen =<< framGen
            ]
        ,  testGroup "Multiply quickcheck"
            [ testProperty "isFinished" $ isFinished <$> multiplierGen
            , testProperty "coSimulation" $ fmap (coSimulation "prop_simulation_multiplier" counter) $ inputsGen =<< multiplierGen
            ]
        -- FIXME: Auto text can't work correctly, because processGen don't take into account the
        -- facts that some variables may go out.
        -- ,  testGroup "Divider process unit"
        --     , testProperty "isFinished" $ isFinished <$> dividerGen
        --     , testProperty "coSimulation" $ fmap (coSimulation "prop_simulation_divider" counter) $ inputsGen =<< dividerGen
        --     ]
      ]

framGen = processGen (def :: (Fram String Int Int))
    [ F <$> (arbitrary :: Gen (Constant String Int))
    , F <$> (arbitrary :: Gen (FramInput String Int))
    , F <$> (arbitrary :: Gen (FramOutput String Int))
    , F <$> (arbitrary :: Gen (Loop String Int))
    , F <$> (arbitrary :: Gen (Reg String Int))
    ]

multiplierGen = processGen (multiplier True)
    [ F <$> (arbitrary :: Gen (Multiply String Int))
    ]

dividerGen = processGen (divider 4 True)
    [ F <$> (arbitrary :: Gen (Division String Int))
    ]
