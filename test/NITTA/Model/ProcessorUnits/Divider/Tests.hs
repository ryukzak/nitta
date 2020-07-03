{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}

{-|
Module      : NITTA.Model.ProcessorUnits.Divider.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Divider.Tests
    ( tests
    ) where

import           NITTA.Intermediate.Functions
import           NITTA.Model.ProcessorUnits.Tests.Utils
import           NITTA.Model.Tests.Microarchitecture
import           Test.Tasty                             (testGroup)


tests = testGroup "Divider PU"
    [ nittaCoSimTestCase "smoke test" march
        [ constant 100 ["a"]
        , loop 2 "e" ["b"]
        , division "a" "b" ["c"] ["d"]
        , add "c" "d" ["e"]

        , constant 200 ["a1"]
        , loop 2 "e1" ["b1"]
        , division "a1" "b1" ["c1"] ["d1"]
        , add "c1" "d1" ["e1"]
        ]
    -- FIXME: Auto text can't work correctly, because processGen don't take into account the
    -- facts that some variables may go out.
    -- , testProperty "isUnitSynthesisFinish" $ isUnitSynthesisFinish <$> dividerGen
    -- , testProperty "coSimulation" $ fmap (coSimulation "prop_simulation_divider") $ initialCycleCntxGen =<< dividerGen
    ]
    -- where
        -- _gen = processAlgOnEndpointGen (divider 4 True :: Divider String Int Int)
        --     [ fmap F (arbitrary :: Gen (Division _ _))
        --     ]
