{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS -fno-warn-partial-type-signatures #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Tests.DSL.Tests
Description :
Copyright   : (c) Artyom Kostyuchik, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Tests.DSL.Tests (
    tests,
) where

import Data.Default
import Data.Proxy
import Data.Set qualified as S
import Data.String.Interpolate
import Data.Text qualified as T
import NITTA.Model.Networks.Bus qualified as Bus
import NITTA.Model.ProcessorUnits.Tests.Providers
import NITTA.Model.Tests.Providers
import Test.Tasty (testGroup)
import Test.Tasty.ExpectedFailure

tests =
    testGroup
        "DSL Tests"
        [ expectFail $
            unitTestCase "should error, when proccess is not done" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                decideAt 1 2 $ consume "a"
                assertSynthesisDone
        , expectFail $
            unitTestCase "should fail coSim, when variables not set" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                decide $ consume "a"
                decide $ consume "b"
                decide $ provide ["c", "d"]
                assertPUCoSimulation
        , unitTestCase "check assertEndpoint and assertAllEndpointRoles with Target success" u $ do
            assign $ multiply "a" "b" ["c", "d"]
            assertAllEndpointRoles [consume "a", consume "b"]
            assertEndpoint 0 maxBound $ consume "a"
            decide $ consume "a"
        , expectFail $
            unitTestCase "check assertAllEndpointRoles and fail (too many)" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                assertAllEndpointRoles [consume "a", consume "b", consume "c"]
        , expectFail $
            unitTestCase "check assertAllEndpointRoles and fail (too less)" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                assertAllEndpointRoles [consume "a"]
        , unitTestCase "check assertEndpoint with Source success" u $ do
            assign $ multiply "a" "b" ["c", "d"]
            decideAt 1 1 $ consume "a"
            decideAt 2 2 $ consume "b"
            assertEndpoint 5 maxBound $ provide ["c", "d"]
        , unitTestCase "check assertEndpoint with Source success" u $ do
            assign $ multiply "a" "b" ["c", "d"]
            decideAt 1 1 $ consume "a"
            decideAt 2 2 $ consume "b"
            decideAt 5 5 $ provide ["c"]
            assertEndpoint 6 maxBound $ provide ["d"]
        , expectFail $
            unitTestCase "check assertEndpoint fail when wrong interval" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                assertEndpoint 2 2 $ consume "a"
                decide $ consume "a"
        , expectFail $
            unitTestCase "check assertEndpoint success" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                assertEndpoint 2 2 $ consume "a"
                decide $ consume "a"
        , expectFail $
            unitTestCase "should fail coSim, when variables incorrect" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                setValue "a" 1
                setValue "b" 1
                decide $ consume "a"
                decide $ consume "b"
                decide $ provide ["c", "d"]
                assertPUCoSimulation
        , expectFail $
            unitTestCase "should not bind, when PU incompatible with F" u $
                assign $
                    sub "a" "b" ["c"]
        , expectFail $
            unitTestCase "decide should error, when Target in Decision is not present" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                decideAt 1 1 $ consume "aa"
        , expectFail $
            unitTestCase "Multiplier should error, when Source in Decision is Targets" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                decideAt 1 1 $ provide ["a"]
        , expectFail $
            unitTestCase "decide should error, when Target in Decision is Source" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                decide $ consume "a"
                decide $ consume "b"
                decideAt 4 4 $ consume "c"
        , expectFail $
            unitTestCase "decide should error, when Interval is not correct" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                decideAt 2 2 $ consume "a"
                decideAt 1 1 $ consume "b"
        , expectFail $
            unitTestCase "should error: breakLoop is not supportd" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                refactor =<< mkBreakLoop 10 "a" ["c"]
        , expectFail $
            unitTestCase "should error: setValue variable is unavailable" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                setValue "e" 10
        , expectFail $
            unitTestCase "should error: setValue variable is alreay set" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                setValue "a" 10
                setValue "b" 11
                setValue "a" 15
        , testGroup
            "decideAtUnsafe"
            [ unitTestCase "check constrain existance" broken $ do
                assign $ brokenBuffer "a" ["b"]
                assertEndpoint 0 maxBound $ consume "a"
                decideAt 0 0 $ consume "a"
                assertEndpoint 3 maxBound $ provide ["b"]
            , expectFail $
                unitTestCase "check that safe decide fail on wrong decision" broken $ do
                    assign $ brokenBuffer "a" ["b"]
                    setValue "a" 1
                    assertEndpoint 0 maxBound $ consume "a"
                    decideAt 0 0 $ consume "a"
                    assertEndpoint 3 maxBound $ provide ["b"]
                    decideAt 2 2 $ provide ["b"]
            , unitTestCase "check that unsafe decide success and test success pass" broken $ do
                assign $ brokenBuffer "a" ["b"]
                setValue "a" 1
                assertEndpoint 0 maxBound $ consume "a"
                decideAt 0 0 $ consume "a"
                assertEndpoint 3 maxBound $ provide ["b"]
                decideAtUnsafe 2 2 $ provide ["b"] -- incorrect decision
                assertPUCoSimulation
            ]
        , testGroup
            "assertLocks"
            [ unitTestCase "assertLocks - success" u $ do
                assign $ multiply "a" "b" ["c", "d"]
                decide $ consume "a"
                assertLocks [Lock{locked = "c", lockBy = "b"}, Lock{locked = "d", lockBy = "b"}]
            , expectFail $
                unitTestCase "assertLocks - fail" u $ do
                    assign $ multiply "a" "b" ["c", "d"]
                    decide $ consume "a"
                    assertLocks [Lock{locked = "c", lockBy = "b"}]
            ]
        , testGroup
            "BusNetwork positive tests"
            [ unitTestCase "target system: autosynthesis, assert breakLoop" def $ do
                setNetwork march
                setBusType pInt
                assignLua
                    [__i|
                        function sum(a)
                            local d = a + 1
                            sum(d)
                        end
                        sum(0)
                        |]
                synthesizeAndCoSim
                assertRefactor =<< mkBreakLoop 0 "d^0#0" ["a^0#0"]
            , unitTestCase "target system: autosynthesis, constant folding" def $ do
                setNetwork march
                setBusType pInt
                assignLua
                    [__i|
                        function sum(a)
                            local d = a + 1
                            sum(d)
                        end
                        sum(0)
                        |]
                synthesizeAndCoSim
                assertRefactor =<< mkBreakLoop 0 "d^0#0" ["a^0#0"]
            , unitTestCase "target system: autosynthesis, constant folding 1" def $ do
                setNetwork march
                setBusType pInt
                assignLua
                    [__i|
                        function sum(a)
                            local d = 1 + 2 + a
                            sum(d)
                        end
                        sum(0)
                        |]
                synthesizeAndCoSim
                assertRefactor
                    =<< mkConstantFolding
                        [ add "!1#0" "!2#0" ["_0#d"]
                        , constant 2 ["!2#0"]
                        , constant 1 ["!1#0"]
                        ]
                        [ constant 3 ["_0#d"]
                        , constant 2 ["!2#0"] -- FIXME: Do we actually need this?
                        , constant 1 ["!1#0"]
                        ]
            , unitTestCase "target system: autosynthesis, constant folding 2" def $ do
                setNetwork march
                setBusType pInt
                assignLua
                    [__i|
                        function sum(a)
                            local d = a + 1 + 2
                            sum(d)
                        end
                        sum(0)
                        |]
                optAccum <-
                    mkOptimizeAccum
                        [ add "_0#d" "!2#0" ["d^0#0"]
                        , add "a^0#0" "!1#0" ["_0#d"]
                        ]
                        [ acc
                            [ Push Plus $ I "a^0#0"
                            , Push Plus $ I "!1#0"
                            , Push Plus $ I "!2#0"
                            , Pull $ O $ S.fromList ["d^0#0"]
                            ]
                        ]
                refactorAvail optAccum
                refactor optAccum
                assertRefactor optAccum
            , -- FIXME: should be presented (with small changes)
              -- refactorAvail
              --     =<< mkConstantFolding
              --         [ acc
              --             [ Push Plus $ I "a^0#0"
              --             , Push Plus $ I "!1#0"
              --             , Push Plus $ I "!2#0"
              --             , Pull $ O $ S.fromList ["d^0#0"]
              --             ]
              --         , constant 1 ["!1#0"]
              --         , constant 2 ["!2#0"]
              --         ]
              --         [ acc
              --             [ Push Plus $ I "a^0#0"
              --             , Push Plus $ I "_0#d"
              --             , Pull $ O $ S.fromList ["d^0#0"]
              --             ]
              --         , constant 2 ["_0#d"]
              --         ]
              unitTestCase "target system: manual synthesis, assert breakLoop" def $ do
                setNetwork march
                setBusType pInt
                let l = loop 0 "d^0#0" ["a^0#0"]
                bind2network l
                doBind "fram1" l
                refactor =<< mkBreakLoop 0 "d^0#0" ["a^0#0"]
                assertRefactor =<< mkBreakLoop 0 "d^0#0" ["a^0#0"]
            , unitTestCase "target system: autosynthesis, buffer & accum refactor" def $ do
                setNetwork march
                setBusType pInt
                assignLua
                    [__i|
                        function sum(a, b, c)
                            local d = a + b + c -- should AccumOptimization
                            local e = d + 1 -- e and d should be buffered
                            local f = d + 2
                            sum(d, f, e)
                        end
                        sum(0,0,0)
                    |]
                synthesizeAndCoSim
                assertRefactor =<< mkBreakLoop 0 "d^0#2" ["a^0#0"]
                assertRefactor =<< mkBreakLoop 0 "f^0#0" ["b^0#0"]
                assertRefactor =<< mkBreakLoop 0 "e^0#0" ["c^0#0"]
                assertRefactor =<< mkResolveDeadlock ["d^0#0", "d^0#1"]
                assertRefactor
                    =<< mkOptimizeAccum
                        [ add "_0#d" "c^0#0" ["d^0#0", "d^0#1", "d^0#2"]
                        , add "a^0#0" "b^0#0" ["_0#d"]
                        ]
                        [ acc
                            [ Push Plus $ I "a^0#0"
                            , Push Plus $ I "b^0#0"
                            , Push Plus $ I "c^0#0"
                            , Pull $ O $ S.fromList ["d^0#0"]
                            , Pull $ O $ S.fromList ["d^0#1"]
                            , Pull $ O $ S.fromList ["d^0#2"]
                            ]
                        ]
            , testGroup
                "Allocation synthesis step"
                [ unitTestCase "target system: manual synthesis, allocation works correctly" def $ do
                    setNetwork $
                        Bus.defineNetwork "net1" ASync $ do
                            Bus.addPrototype "fram{x}" FramIO
                            Bus.addPrototype "accum" AccumIO
                    setBusType pInt
                    assignLua
                        [__i|
                            function sum(a)
                                local d = a + 1
                                sum(d)
                            end
                            sum(0)
                            |]
                    doAllocation "net1" "accum"
                    doAllocation "net1" "fram{x}"
                    assertAllocation 1 =<< mkAllocation "net1" "fram{x}"
                    assertAllocation 1 =<< mkAllocation "net1" "accum"
                    assertAllocationOptions =<< mkAllocationOptions "net1" ["fram{x}"]
                    assertPU "net1_accum" (Proxy :: Proxy (Accum T.Text Int Int))
                    assertPU "net1_fram1" (Proxy :: Proxy (Fram T.Text Int Int))
                    synthesizeAndCoSim
                , unitTestCase "target system: autosynthesis, allocate required PUs" def $ do
                    setNetwork $
                        Bus.defineNetwork "net1" ASync $ do
                            Bus.addCustomPrototype "fram{x}" (framWithSize 32) FramIO
                            Bus.addPrototype "accum{x}" AccumIO
                            Bus.addPrototype "mul{x}" MultiplierIO
                            Bus.add "spi" $ -- FIXME: use addPrototype when https://github.com/ryukzak/nitta/issues/194 will be fixed
                                SPISlave
                                    { slave_mosi = InputPortTag "mosi"
                                    , slave_miso = OutputPortTag "miso"
                                    , slave_sclk = InputPortTag "sclk"
                                    , slave_cs = InputPortTag "cs"
                                    }
                    setBusType pInt
                    assignLua
                        [__i|
                            function counter(x1)
                                send(x1)
                                x2 = x1 + 1
                                counter(x2)
                            end
                            counter(0)
                            |]
                    synthesizeAndCoSim
                    assertAllocation 1 =<< mkAllocation "net1" "fram{x}"
                    assertAllocation 1 =<< mkAllocation "net1" "accum{x}"
                    assertAllocation 0 =<< mkAllocation "net1" "mul{x}"
                , unitTestCase "target system: autosynthesis, allocation comes after constant folding" def $ do
                    setNetwork $
                        Bus.defineNetwork "net1" ASync $ do
                            Bus.addCustomPrototype "fram{x}" (framWithSize 32) FramIO
                            Bus.addPrototype "accum{x}" AccumIO
                            Bus.addPrototype "mul{x}" MultiplierIO
                    setBusType pInt
                    assignLua
                        [__i|
                            function mul3(x1)
                                x1 = (1 + 1 + 1) * x1
                                mul3(x1)
                            end
                            mul3(1)
                            |]
                    synthesizeAndCoSim
                    assertAllocation 1 =<< mkAllocation "net1" "fram{x}"
                    assertAllocation 1 =<< mkAllocation "net1" "mul{x}"
                    assertAllocation 0 =<< mkAllocation "net1" "accum{x}"
                ]
            ]
        , testGroup
            "BusNetwork negative tests"
            [ expectFail $
                unitTestCase "target system: manual synthesis, refactor loop break not applied" def $ do
                    setNetwork march
                    setBusType pInt
                    let l = loop 0 "d^0#0" ["a^0#0"]
                    bind2network l
                    doBind "fram1" l
                    assertRefactor =<< mkBreakLoop 0 "d^0#0" ["a^0#0"]
            ]
        ]
    where
        u = multiplier True :: Multiplier String Int Int
        broken = def :: Broken String Int Int
