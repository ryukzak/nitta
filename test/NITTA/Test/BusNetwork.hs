{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-|
Module      : NITTA.Test.BusNetwork
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Test.BusNetwork
    ( busNetworkTests
    ) where

import           Control.Monad                    (void)
import           Data.Default
import           Data.Map                         (fromList)
import qualified Data.Set                         as S
import qualified NITTA.Intermediate.Functions     as F
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Types
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.Problems.Types
import           NITTA.Model.ProcessorUnits
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.TargetSystem
import           NITTA.Project
import           NITTA.Test.Microarchitectures
import           Test.Tasty                       (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.TH


test_fibonacci =
    [ algTestCase "simple" march
        [ F.loop 0  "b2" ["a1"      ]
        , F.loop 1  "c"  ["b1", "b2"]
        , F.add "a1" "b1" ["c"]
        ]
    , algTestCase "io_drop_data" (marchSPIDropData True proxyInt) algWithSend
    , algTestCase "io_no_drop_data" (marchSPI True proxyInt) algWithSend
    ]
    where
        algWithSend =
            [ F.loop 0 "b2" ["a1"      ]
            , F.loop 1 "c"  ["b1", "b2"]
            , F.add "a1" "b1" ["c", "c_copy"]
            , F.send "c_copy"
            ]


test_io =
    [ testCase "receive two variables" $ void $ runTargetSynthesis' (def :: TargetSynthesis _ _ _ Int)
        { tName="double_receive"
        , tMicroArch=marchSPI True proxyInt
        , tReceivedValues=[ ("a", [10..15]), ("b", [20..25]) ]
        , tDFG=fsToDataFlowGraph
            [ F.receive ["a"]
            , F.receive ["b"]
            , F.add "a" "b" ["c"]
            , F.send "c"
            ]
        }
    ]



f1 = F.add "a" "b" ["c", "d"] :: F String Int

test_patchFunction =
    [ testCase "non-patched function" $
        show f1 @?= "c = d = a + b"

    , testCase "direct patched function input" $
        show (patch ("a", "a'") f1) @?= "c = d = a' + b"
    , testCase "direct patched function output" $
        show (patch ("c", "c'") f1) @?= "c' = d = a + b"

    , testCase "diff patched function input by input" $
        show (patch def{ diffI=fromList [("a", "a'")] } f1) @?= "c = d = a' + b"
    , testCase "diff non patched function input by output" $
        show (patch def{ diffO=fromList [("a", "a'")] } f1) @?= "c = d = a + b"

    , testCase "diff patched function output by output" $
        show (patch def{ diffO=fromList [("c", "c'")] } f1) @?= "c' = d = a + b"
    , testCase "diff non patched function output by input" $
        show (patch def{ diffI=fromList [("c", "c'")] } f1) @?= "c = d = a + b"

    , testCase "diff non patched function output by input" $
        show (patch def
                { diffI=fromList [("b", "b'"), ("d", "d!")]
                , diffO=fromList [("d", "d'"), ("b", "b!")]
                } f1) @?= "c = d' = a + b'"
    ]


pu = let
    Right pu' = tryBind f1 PU
        { diff=def
        , unit=def :: Accum String Int Int
        , ports=undefined
        , ioPorts=undefined
        , systemEnv=undefined
        }
    in pu'

test_patchEndpointOptions =
    [ testCase "non-patched function options" $
        show' opts @?= "[Target a,Target b]"

    , testCase "patched function options input by input" $
        show' (patch def{ diffI=fromList [("a","a'")]} opts) @?= "[Target a',Target b]"
    , testCase "non-patched function options input by output" $
        show' (patch def{ diffO=fromList [("a","a'")]} opts) @?= "[Target a,Target b]"

    , testCase "patched function options output by output" $
        show' (patch def{ diffO=fromList [("d","d'")]} opts') @?= "[Source c,d']"
    , testCase "non-patched function options output by input" $
        show' (patch def{ diffI=fromList [("d","d'")]} opts') @?= "[Source c,d]"
    ]
    where
        opts = options endpointDT pu
        opts' = let
                o1 = head opts
                pu' = decision endpointDT pu $ endpointOptionToDecision o1
                o2 = head $ options endpointDT pu'
                pu'' = decision endpointDT pu' $ endpointOptionToDecision o2
            in options endpointDT pu''
        show' = show . map epoRole


test_patchPU =
    [ testCase "patched PU input options" $
        show' o1 @?= "[Target a',Target b]"
    , testCase "non-patched PU input options" $
        show' o3 @?= "[Target b]"
    , testCase "patched PU output options" $
        show' o4 @?= "[Source c,d']"
    , testCase "non-patched PU all done" $
        show' o5 @?= "[]"
    ]
    where
        pu1 = patch (I "a", I "a'") pu
        o1 = options endpointDT pu1
        pu2 = patch (O $ S.fromList ["d"], O $ S.fromList ["d'"]) pu1
        o2 = options endpointDT pu2
        pu3 = decision endpointDT pu2 $ endpointOptionToDecision $ head o2
        o3 = options endpointDT pu3
        pu4 = decision endpointDT pu3 $ endpointOptionToDecision $ head o3
        o4 = options endpointDT pu4
        pu5 = decision endpointDT pu4 $ endpointOptionToDecision $ head o4
        o5 = options endpointDT pu5

        show' = show . map epoRole


busNetworkTests :: TestTree
busNetworkTests = $(testGroupGenerator)
