{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS -fno-warn-partial-type-signatures #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Multiplier.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Multiplier.Tests (
    tests,
) where

import Data.Default
import Data.String.Interpolate
import NITTA.Intermediate.Functions
import qualified NITTA.Intermediate.Functions as F
import NITTA.Intermediate.Tests.Functions ()
import NITTA.Intermediate.Types
import NITTA.LuaFrontend.Tests.Utils
import NITTA.Model.MultiplierDsl
import NITTA.Model.Networks.Types
import NITTA.Model.ProcessorUnits
import NITTA.Model.ProcessorUnits.Tests.Utils
import NITTA.Model.Tests.Microarchitecture
import Test.QuickCheck
import Test.Tasty (testGroup)

tests =
    testGroup
        "Multiplier PU"
        [ nittaCoSimTestCase
            "smoke test"
            march
            [ constant 2 ["a"]
            , loop 1 "c" ["b"]
            , multiply "a" "b" ["c"]
            , constant 3 ["x"]
            , loop 1 "z" ["y"]
            , multiply "y" "x" ["z"]
            ]
        , puCoSimTestCase
            "multiplier with attr"
            u2
            [ ("a", Attr (IntX 1) True)
            , ("b", Attr (IntX 2) False)
            , ("d", Attr (IntX 258) False)
            , ("e", Attr (IntX 258) False)
            ]
            [multiply "a" "b" ["c"], multiply "d" "e" ["f"]]
        , luaTestCase
            "geometric progression"
            [__i|
                function f(x)
                    local tmp = buffer(2 * x)
                    f(tmp)
                end
                f(1)
            |]
        , typedLuaTestCase
            (microarch ASync SlaveSPI)
            pFX22_32
            "fixpoint 22 32"
            [__i|
                function f()
                    send(0.5 * -0.5)
                    send(-20.5 * -2)
                end
                f()
            |]
        , typedLuaTestCase
            (microarch ASync SlaveSPI)
            pFX42_64
            "fixpoint 42 64"
            [__i|
                function f()
                    send(0.5 * -0.5)
                    send(-20.5 * -2)
                end
                f()
            |]
        , finitePUSynthesisProp "isFinish" u fsGen
        , puCoSimProp "multiplier_coSimulation" u fsGen
        , multiplierTest "multiplier smoke test" $
            evalMultiplier u $ do
                bindFunc fDef
                _ <- assertBindFullness
                doDecisionSafe $ beTargetAt 1 2 "a"
                doNDecision 0
                doDecisionSafe $ beSourceAt 5 5 ["c"]
                doFstDecision
                _ <- assertProcessDone
                assertExecute
        , multiplierTest "accum smoke test" $
            evalMultiplier u3 $ do
                bindFunc fSub
                doDecisionSafe $ beTargetAt 4 4 "a"
                doFstDecision
                doDecisionSafe $ beSourceAt 5 5 ["c"]
                _ <- assertProcessDone
                assertExecute
        , multiplierNegTest "should error, when proccess is not done" $
            evalMultiplier u $ do
                bindFunc fDef
                doDecisionSafe $ beTargetAt 1 2 "a"
                doNDecision 0
                _ <- assertProcessDone
                assertExecute
        , multiplierNegTest "shouldn't bind, when PU incompatible with F" $
            evalMultiplier u $ do
                bindFunc fSub
                assertExecute
        , multiplierNegTest "shouldn't bind, when different signatures" $
            evalMultiplier u3 $ do
                bindFunc fSub
                _ <- assertBindFullness -- TODO: Why Accum return "Acc" as a label instead "-"?
                assertExecute
        , multiplierNegTest "doDecisionSafe should error, when Target in Decision is not present" $
            evalMultiplier u $ do
                bindFunc fDef
                doDecisionSafe $ beTargetAt 1 1 "aa"
                assertExecute
        , multiplierNegTest "Multiplier should error, when Source in Decision is Targets" $
            -- TODO: there "Multiplier decision error" not edsl?
            evalMultiplier u $ do
                bindFunc fDef
                doDecisionSafe $ beSourceAt 1 1 ["a"]
                assertExecute
        , multiplierNegTest "doDecisionSafe should error, when Target in Decision is Source" $
            evalMultiplier u $ do
                bindFunc fDef
                doFstDecision
                doFstDecision
                doDecisionSafe $ beTargetAt 4 4 "c"
                assertExecute
        , multiplierNegTest "doDecisionSafe should error, when Interval is not correct" $
            evalMultiplier u $ do
                bindFunc fDef
                doDecisionSafe $ beTargetAt 2 2 "a"
                doDecisionSafe $ beTargetAt 1 1 "b"
                assertExecute
        , multiplierNegTest "doNDecision should error, when index is out of bound" $
            evalMultiplier u $ do
                bindFunc fDef
                doNDecision 3
                assertExecute
        , multiplierNegTest "doFstDecision should error, when decisions are spent" $
            evalMultiplier u3 $ do
                bindFunc fSub
                doFstDecision
                doFstDecision
                doFstDecision
                doFstDecision
                assertExecute
        , multiplierNegTest "doFstDecision should error, when PU is not bound" $
            evalMultiplier u3 $ do
                doFstDecision
                assertExecute
        ]
    where
        u = multiplier True :: Multiplier String Int Int
        u2 = multiplier True :: Multiplier String (Attr (IntX 16)) Int
        u3 = def :: Accum String Int Int
        fDef = F.multiply "a" "b" ["c", "d"] :: F String Int
        fSub = F.sub "a" "b" ["c"] :: F String Int
        fsGen =
            algGen
                [ fmap packF (arbitrary :: Gen (Multiply _ _))
                ]
