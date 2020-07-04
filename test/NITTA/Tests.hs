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
Module      : NITTA.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Tests
    ( tests
    ) where

import           Control.Monad                       (void)
import           Data.Default
import           Data.Map                            (fromList)
import qualified Data.Set                            as S
import qualified NITTA.Intermediate.Functions        as F
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Types
import           NITTA.Model.Problems
import           NITTA.Model.ProcessorUnits
import           NITTA.Model.ProcessorUnits.Time
import           NITTA.Model.TargetSystem
import           NITTA.Model.Tests.Microarchitecture
import           NITTA.TargetSynthesis
import           Test.Tasty                          (TestTree, testGroup)
import           Test.Tasty.HUnit
import           Test.Tasty.TH


test_fibonacci =
    [ algTestCase "simple" march
        [ F.loop 0  "b2" ["a1"      ]
        , F.loop 1  "c"  ["b1", "b2"]
        , F.add "a1" "b1" ["c"]
        ]
    , algTestCase "io_drop_data" (marchSPIDropData True pInt) algWithSend
    , algTestCase "io_no_drop_data" (marchSPI True pInt) algWithSend
    ]
    where
        algWithSend =
            [ F.loop 0 "b2" ["a1"      ]
            , F.loop 1 "c1"  ["b1", "b2"]
            , F.add "a1" "b1" ["c1", "c2"]
            , F.send "c2"
            ]

test_add_and_io =
    [ testCase "receive 4 variables" $ void $ runTargetSynthesisWithUniqName (def :: TargetSynthesis _ _ _ Int)
        { tName="Two functions 4 variables"
        , tMicroArch=marchSPI True pInt
        , tReceivedValues=[ ("a", [10..15]), ("b", [20..25]), ("e", [0..25]), ("f", [20..30])]
        , tDFG=fsToDataFlowGraph
            [ F.receive ["a"]
            , F.receive ["b"]
            , F.receive ["e"]
            , F.receive ["f"]
            , F.accFromStr "+a +b = c = d; +e - f = g = h"
            , F.send "d"
            , F.send "c"
            , F.send "g"
            , F.send "h"
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
        show (patch def{ changeI=fromList [("a", "a'")] } f1) @?= "c = d = a' + b"
    , testCase "diff non patched function input by output" $
        show (patch def{ changeO=fromList [("a", S.singleton "a'")] } f1) @?= "c = d = a + b"

    , testCase "diff patched function output by output" $
        show (patch def{ changeO=fromList [("c", S.singleton "c'")] } f1) @?= "c' = d = a + b"
    , testCase "diff non patched function output by input" $
        show (patch def{ changeI=fromList [("c", "c'")] } f1) @?= "c = d = a + b"

    , testCase "diff non patched function output by input" $
        show (patch def
                { changeI=fromList [("b", "b'"), ("d", "d!")]
                , changeO=fromList [("d", S.singleton "d'"), ("b", S.singleton "b!")]
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
        show' (patch def{ changeI=fromList [("a", "a'")]} opts) @?= "[Target a',Target b]"
    , testCase "non-patched function options input by output" $
        show' (patch def{ changeO=fromList [("a", S.singleton "a'")]} opts) @?= "[Target a,Target b]"

    , testCase "patched function options output by output" $
        show' (patch def{ changeO=fromList [("d", S.singleton "d'")]} opts') @?= "[Source c,d']"
    , testCase "non-patched function options output by input" $
        show' (patch def{ changeI=fromList [("d","d'")]} opts') @?= "[Source c,d]"
    ]
    where
        opts = endpointOptions pu
        opts' = let
                o1 = head opts
                pu' = endpointDecision pu $ endpointOptionToDecision o1
                o2 = head $ endpointOptions pu'
                pu'' = endpointDecision pu' $ endpointOptionToDecision o2
            in endpointOptions pu''
        show' = show . map epRole


test_patchPUone2one =
    [ testCase "options, PU patched" $
        show' o1 @?= "[Target A,Target b]"
    , testCase "options, first arg loaded" $
        show' o3 @?= "[Target b]"
    , testCase "options, all all args loaded" $
        show' o4 @?= "[Source D,c]"
    , testCase "all work done" $
        show' o5 @?= "[]"
    ]
    where
        -- F.add "a" "b" ["c", "d"] -> F.add "A" "b" ["c", "d"]
        pu1 = patch (I "a", I "A") pu
        o1 = endpointOptions pu1
        -- F.add "A" "b" ["c", "d"] -> F.add "A" "b" ["c", "D"]
        pu2 = patch (O $ S.fromList ["d"], O $ S.fromList ["D"]) pu1
        o2 = endpointOptions pu2
        -- F.add "A" "b" ["c", "D"] -> F.add ___ "b" ["c", "D"]
        pu3 = endpointDecision pu2 $ endpointOptionToDecision $ head o2
        o3 = endpointOptions pu3
        -- F.add ___ "b" ["c", "D"] -> F.add ___ ___ ["c", "D"]
        pu4 = endpointDecision pu3 $ endpointOptionToDecision $ head o3
        o4 = endpointOptions pu4
        -- F.add ___ "b" ["c", "D"] -> F.add ___ ___ __________
        pu5 = endpointDecision pu4 $ endpointOptionToDecision $ head o4
        o5 = endpointOptions pu5

        show' = show . map epRole


test_patchPUmany2one =
    [ testCase "options, PU patched" $
        show' o1 @?= "[Target A,Target b]"
    , testCase "options, first arg loaded" $
        show' o3 @?= "[Target b]"
    , testCase "options, all all args loaded" $
        show' o4 @?= "[Source CD]"
    , testCase "all work done" $
        show' o5 @?= "[]"
    ]
    where
        -- F.add "a" "b" ["c", "d"] -> F.add "A" "b" ["c", "d"]
        pu1 = patch (I "a", I "A") pu
        o1 = endpointOptions pu1
        -- F.add "A" "b" ["c", "d"] -> F.add "A" "b" ["CD"]
        pu2 = patch (O $ S.fromList ["c", "d"], O $ S.fromList ["CD"]) pu1
        o2 = endpointOptions pu2
        -- F.add "A" "b" ["CD"] -> F.add ___ "b" ["CD"]
        pu3 = endpointDecision pu2 $ endpointOptionToDecision $ head o2
        o3 = endpointOptions pu3
        -- F.add ___ "b" ["CD"] -> F.add ___ ___ ["CD"]
        pu4 = endpointDecision pu3 $ endpointOptionToDecision $ head o3
        o4 = endpointOptions pu4
        -- F.add ___ "b" ["CD"] -> F.add ___ ___ ______
        pu5 = endpointDecision pu4 $ endpointOptionToDecision $ head o4
        o5 = endpointOptions pu5

        show' = show . map epRole


tests :: TestTree
tests = $(testGroupGenerator)
