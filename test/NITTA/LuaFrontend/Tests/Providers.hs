{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-redundant-constraints #-}

{- |
Module      : NITTA.LuaFrontend.Tests.Providers
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.LuaFrontend.Tests.Providers (
    luaTestCase,
    typedLuaTestCase,
    typedIOLuaTestCase,
    traceLuaSimulationTestCase,
    module NITTA.Model.Tests.Microarchitecture,
) where

import Data.CallStack
import Data.Default
import Data.Proxy
import qualified Data.Text as T
import NITTA.Intermediate.Simulation
import NITTA.Intermediate.Types
import NITTA.LuaFrontend
import NITTA.Model.Networks.Bus
import NITTA.Model.Networks.Types
import NITTA.Model.ProcessorUnits.Tests.Providers
import NITTA.Model.Tests.Microarchitecture
import Test.Tasty (TestTree)
import Test.Tasty.HUnit

traceLuaSimulationTestCase ::
    forall x.
    (HasCallStack, Val x, Integral x) =>
    Proxy x ->
    String ->
    T.Text ->
    String ->
    TestTree
traceLuaSimulationTestCase _ name src expect =
    testCase name $
        let FrontendResult{frDataFlow, frPrettyLog} :: FrontendResult T.Text x = lua2functions src
            cntx = simulateDataFlowGraph 5 def def frDataFlow
            actual = log2md $ frPrettyLog $ map cycleCntx $ cntxProcess cntx
         in expect @=? actual

luaTestCase :: HasCallStack => String -> T.Text -> TestTree
luaTestCase name = typedIOLuaTestCase (microarch ASync SlaveSPI) pAttrIntX32 name def

typedLuaTestCase ::
    (HasCallStack, Val x, Integral x) =>
    BusNetwork T.Text T.Text x Int ->
    Proxy x ->
    String ->
    T.Text ->
    TestTree
typedLuaTestCase arch proxy name = typedIOLuaTestCase arch proxy name def

typedIOLuaTestCase ::
    (HasCallStack, Val x, Integral x) =>
    BusNetwork T.Text T.Text x Int ->
    Proxy x ->
    String ->
    [(T.Text, [x])] ->
    T.Text ->
    TestTree
typedIOLuaTestCase arch proxy name received src = unitTestCase name def $ do
    setNetwork arch
    setBusType proxy
    setReceivedValues received
    assignLua src
    synthesizeAndCoSim
