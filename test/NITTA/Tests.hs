{-# LANGUAGE DataKinds #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-partial-type-signatures #-}

{- |
Module      : NITTA.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Tests (
    tests,
) where

import Data.Default
import Data.Map.Strict (fromList)
import Data.Set qualified as S
import Data.String.Interpolate
import Data.Text qualified as T
import NITTA.Intermediate.Functions qualified as F
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Model.ProcessorUnits.Tests.Providers
import NITTA.Model.Tests.Providers
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.TH

test_fibonacci =
    [ unitTestCase "simple" def $ do
        setNetwork march
        bind2network $ F.loop (0 :: Int) "b2" ["a1"]
        bind2network $ F.loop (1 :: Int) "c" ["b1", "b2"]
        bind2network $ F.add "a1" "b1" ["c"]
        synthesizeAndCoSim
    , unitTestCase "io_drop_data" def $ do
        setNetwork $ marchSPIDropData True pInt
        mapM_ (\f -> bind2network f) algWithSend
        synthesizeAndCoSim
    , unitTestCase "io_no_drop_data" def $ do
        setNetwork $ marchSPI True pInt
        mapM_ (\f -> bind2network f) algWithSend
        synthesizeAndCoSim
    ]
    where
        algWithSend =
            [ F.loop 0 "b2" ["a1"]
            , F.loop 1 "c1" ["b1", "b2"]
            , F.add "a1" "b1" ["c1", "c2"]
            , F.send "c2"
            ]

test_add_and_io =
    [ unitTestCase "receive several values and complex accum" def $ do
        setNetwork $ microarch Sync SlaveSPI
        setBusType pIntX32
        mapM_
            (\f -> bind2network f)
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
        setReceivedValues
            [ ("a", [10 .. 15])
            , ("b", [20 .. 25])
            , ("e", [0 .. 25])
            , ("f", [20 .. 30])
            ]
        synthesizeAndCoSim
    ]

test_manual =
    [ unitTestCase "target system: full manual synthesis" def $ do
        setNetwork $ marchSPI True pAttrIntX32
        setBusType pAttrIntX32
        assignLua
            [__i|
                function sum(a, b, c)
                    local d = a + b + c -- should AccumOptimization
                    local e = d + 1 -- e and d should be buffered
                    local f = d+ 2
                    sum(d, f, e)
                end
                sum(0,0,0)
            |]
        mapM_
            (\(tag, unit) -> doBind tag unit)
            [ ("fram1", F.loop 0 "d^0#2" ["a^0#0"])
            , ("fram1", F.loop 0 "f^0#0" ["b^0#0"])
            , ("fram1", F.loop 0 "e^0#0" ["c^0#0"])
            , ("fram1", F.constant 1 ["!1#0"])
            , ("fram1", F.constant 2 ["!2#0"])
            , ("accum", F.add "a^0#0" "b^0#0" ["_0#d"])
            , ("accum", F.add "_0#d" "c^0#0" ["d^0#0", "d^0#1", "d^0#2"])
            , ("accum", F.add "d^0#1" "!2#0" ["f^0#0"])
            , ("accum", F.add "d^0#0" "!1#0" ["e^0#0"])
            ]
        mkBreakLoop 0 "d^0#2" ["a^0#0"] >>= \r -> refactor r
        mkBreakLoop 0 "f^0#0" ["b^0#0"] >>= \r -> refactor r
        mkBreakLoop 0 "e^0#0" ["c^0#0"] >>= \r -> refactor r
        doTransfer ["a^0#0"]
        doTransfer ["b^0#0"]
        mkResolveDeadlock ["_0#d"] >>= \r -> refactor r
        doBind "fram1" $ F.buffer "_0#d@buf" ["_0#d"]
        doTransfer ["_0#d@buf"]
        doTransfer ["_0#d"]
        doTransfer ["c^0#0"]
        doTransfer ["d^0#2"]
        mkResolveDeadlock ["d^0#0", "d^0#1"] >>= \r -> refactor r
        doBind "fram1" $ F.buffer "d^0#0@buf" ["d^0#0", "d^0#1"]
        doTransfer ["d^0#0@buf"]
        doTransfer ["d^0#1"]
        doTransfer ["!2#0"]
        doTransfer ["f^0#0"]
        doTransfer ["d^0#0"]
        doTransfer ["!1#0"]
        doTransfer ["e^0#0"]
        assertSynthesisComplete
        assertTargetSystemCoSimulation
    ]

f1 = F.add "a" "b" ["c", "d"] :: F T.Text Int

patchP :: Patch a (T.Text, T.Text) => (T.Text, T.Text) -> a -> a
patchP = patch

patchI :: Patch a (I T.Text, I T.Text) => (I T.Text, I T.Text) -> a -> a
patchI = patch

patchO :: Patch a (O T.Text, O T.Text) => (O T.Text, O T.Text) -> a -> a
patchO = patch

patchC :: Patch a (Changeset T.Text) => Changeset T.Text -> a -> a
patchC = patch

test_patchFunction =
    [ testCase "non-patched function" $
        show f1 @?= "a + b = c = d"
    , testCase "direct patched function input" $
        show (patchP ("a", "a'") f1) @?= "a' + b = c = d"
    , testCase "direct patched function output" $
        show (patchP ("c", "c'") f1) @?= "a + b = c' = d"
    , testCase "diff patched function input by input" $
        show (patchC def{changeI = fromList [("a", "a'")]} f1) @?= "a' + b = c = d"
    , testCase "diff non patched function input by output" $
        show (patchC def{changeO = fromList [("a", S.singleton "a'")]} f1) @?= "a + b = c = d"
    , testCase "diff patched function output by output" $
        show (patchC def{changeO = fromList [("c", S.singleton "c'")]} f1) @?= "a + b = c' = d"
    , testCase "diff non patched function output by input" $
        show (patchC def{changeI = fromList [("c", "c'")]} f1) @?= "a + b = c = d"
    , testCase "diff non patched function output by input" $
        show
            ( patchC
                def
                    { changeI = fromList [("b", "b'"), ("d", "d!")]
                    , changeO = fromList [("d", S.singleton "d'"), ("b", S.singleton "b!")]
                    }
                f1
            )
            @?= "a + b' = c = d'"
    ]

pu = case tryBind
    f1
    PU
        { diff = def
        , unit = def :: Accum T.Text Int Int
        , uEnv = undefined
        } of
    Right pu_ -> pu_
    Left err -> error $ show err

test_patchEndpointOptions =
    [ testCase "non-patched function options" $
        show' opts @?= "[Target a,Target b]"
    , testCase "patched function options input by input" $
        show' (patchC def{changeI = fromList [("a", "a'")]} opts) @?= "[Target a',Target b]"
    , testCase "non-patched function options input by output" $
        show' (patchC def{changeO = fromList [("a", S.singleton "a'")]} opts) @?= "[Target a,Target b]"
    , testCase "patched function options output by output" $
        show' (patchC def{changeO = fromList [("d", S.singleton "d'")]} opts') @?= "[Source c,d']"
    , testCase "non-patched function options output by input" $
        show' (patchC def{changeI = fromList [("d", "d'")]} opts') @?= "[Source c,d]"
    ]
    where
        opts = endpointOptions pu
        opts' =
            let o1 = head opts
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
        pu1 = patchI (I "a", I "A") pu
        o1 = endpointOptions pu1
        -- F.add "A" "b" ["c", "d"] -> F.add "A" "b" ["c", "D"]
        pu2 = patchO (O $ S.fromList ["d"], O $ S.fromList ["D"]) pu1
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
        pu1 = patchI (I "a", I "A") pu
        o1 = endpointOptions pu1
        -- F.add "A" "b" ["c", "d"] -> F.add "A" "b" ["CD"]
        pu2 = patchO (O $ S.fromList ["c", "d"], O $ S.fromList ["CD"]) pu1
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
