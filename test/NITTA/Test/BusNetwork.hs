{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Test.BusNetwork
    ( busNetworkTests
    ) where

import qualified NITTA.Functions               as F
import           NITTA.Test.Microarchitectures
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.TH


test_someAlgorithm =
    [ algTestCase "accum_fram" march
        [ F.framInput 3 [ "d", "p" ]
        , F.framInput 4 [ "e", "k" ]
        , F.framOutput 5 "p"
        , F.framOutput 6 "k"
        , F.loop 22 "sum" ["s"]
        , F.framOutput 7 "s"
        , F.add "d" "e" ["sum"]
        ]
    ]


test_fibonacci =
    [ algTestCase "simple" march
        [ F.loop 0  "b2" ["a1"      ]
        , F.loop 1  "c"  ["b1", "b2"]
        , F.add "a1" "b1" ["c"]
        ]
    , algTestCase "drop_data" (marchSPIDropData proxyInt) alg
    -- , algTestCase "no_drop_data" (marchSPI proxyInt) alg
    ]
    where
        alg =
            [ F.loop 0 "b2" ["a1"      ]
            , F.loop 1 "c"  ["b1", "b2"]
            , F.add "a1" "b1" ["c", "c_copy"]
            , F.send "c_copy"
            ]


busNetworkTests :: TestTree
busNetworkTests = $(testGroupGenerator)
