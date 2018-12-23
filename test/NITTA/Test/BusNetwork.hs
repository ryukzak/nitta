{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Test.BusNetwork
Description :
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
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
    , algTestCase "io_drop_data" (marchSPIDropData proxyInt) alg
    , algTestCase "io_no_drop_data" (marchSPI proxyInt) alg
    ]
    where
        alg =
            [ F.loop 0 "b2" ["a1"      ]
            , F.loop 1 "c"  ["b1", "b2"]
            , F.add "a1" "b1" ["c", "c_copy"]
            , F.send "c_copy"
            ]


test_io =
    [ algTestCaseWithInput "double_receive" [("a", [10..15]),("b", [20..25])] (marchSPI proxyInt)
        [ F.receive ["a"]
        , F.receive ["b"]
        , F.add "a" "b" ["c"]
        , F.send "c"
        ]
    ]


busNetworkTests :: TestTree
busNetworkTests = $(testGroupGenerator)
