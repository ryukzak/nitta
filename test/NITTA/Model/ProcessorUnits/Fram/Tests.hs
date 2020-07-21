{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}

{-|
Module      : NITTA.Model.ProcessorUnits.Fram.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Fram.Tests
    ( tests
    ) where

import           Data.Default
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Tests.Functions ()
import           NITTA.Intermediate.Types
import           NITTA.Model.ProcessorUnits.Fram
import           NITTA.Model.ProcessorUnits.Tests.Utils
import           Test.QuickCheck
import           Test.Tasty ( testGroup )


tests = testGroup "Fram PU"
    [ puCoSimTestCase "register function" u [("a", 42)]
        [ reg "a" ["b"]
        ]
    , puCoSimTestCase "constant function" u []
        [ constant 11 ["ovj"]
        ]
    , puCoSimTestCase "loop function" u [("b", 42)]
        [ loop 10 "b" ["a"]
        ]
    -- TODO: not available, because needed self transaction
    -- , unitCoSimulationTestCase "loop_reg" u []
    --     [ reg "a" ["b"]
    --     , loop 10 "b" ["a"]
    --     ]
    , finitePUSynthesisProp "finite synthesis properties" u fsGen
    , puCoSimProp "co simulation properties" u fsGen
    ]
    where
        u = def :: Fram String Int Int
        fsGen = algGen
            [ fmap packF (arbitrary :: Gen (Constant _ _))
            , fmap packF (arbitrary :: Gen (Loop _ _))
            , fmap packF (arbitrary :: Gen (Reg _ _))
            ]
