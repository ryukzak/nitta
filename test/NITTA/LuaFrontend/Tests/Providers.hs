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
import NITTA.Model.Tests.Internals
import NITTA.Model.Tests.Microarchitecture
import NITTA.Project
import NITTA.Synthesis
import NITTA.Utils
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
        let FrontendResult{frDataFlow, frPrettyCntx} :: FrontendResult x = lua2functions src
            cntx = simulateDataFlowGraph 5 def def frDataFlow
            actual = cntx2table $ frPrettyCntx cntx
         in expect @=? actual

luaTestCase :: HasCallStack => String -> T.Text -> TestTree
luaTestCase name = typedIOLuaTestCase (microarch ASync SlaveSPI) pAttrIntX32 name def

typedLuaTestCase ::
    (HasCallStack, Val x, Integral x) =>
    BusNetwork String String x Int ->
    Proxy x ->
    String ->
    T.Text ->
    TestTree
typedLuaTestCase arch proxy name = typedIOLuaTestCase arch proxy name def

typedIOLuaTestCase ::
    (HasCallStack, Val x, Integral x) =>
    BusNetwork String String x Int ->
    Proxy x ->
    String ->
    [(String, [x])] ->
    T.Text ->
    TestTree
typedIOLuaTestCase arch proxy name received src = testCase name $ do
    let wd = "lua_" ++ toModuleName name
    status <- runLua arch proxy wd received src
    case status of
        Left err -> assertFailure err
        Right _ -> return ()

-- Internals

runLua ::
    forall x.
    (Val x, Integral x) =>
    BusNetwork String String x Int ->
    Proxy x ->
    String ->
    [(String, [x])] ->
    T.Text ->
    IO (Either String ())
runLua arch _proxy wd received src = do
    reportE <-
        runTargetSynthesisWithUniqName
            (def :: TargetSynthesis String String x Int)
                { tName = wd
                , tMicroArch = arch
                , tSourceCode = Just src
                , tReceivedValues = received
                }
    return $ case reportE of
        Left err -> Left $ "synthesis process fail" <> err
        Right TestbenchReport{tbStatus = True} -> Right ()
        Right report@TestbenchReport{tbCompilerDump}
            | T.length tbCompilerDump > 2 ->
                Left $ "icarus synthesis error:\n" <> show report
        Right report@TestbenchReport{} ->
            Left $ "icarus simulation error:\n" <> show report
