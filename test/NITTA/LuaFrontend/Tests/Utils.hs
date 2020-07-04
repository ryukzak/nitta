{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures  -fno-warn-partial-type-signatures #-}

{-|
Module      : NITTA.LuaFrontend.Tests.Utils
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.LuaFrontend.Tests.Utils
    ( luaTestCase, typedLuaTestCase, typedIOLuaTestCase
    ) where

import           Data.CallStack
import           Data.Default
import           Data.Either
import           Data.Proxy
import qualified Data.String.Utils as S
import qualified Data.Text as T
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Networks.Types
import           NITTA.Model.Tests.Microarchitecture
import           NITTA.Project
import           NITTA.TargetSynthesis
import           NITTA.Utils
import           Test.Tasty ( TestTree )
import           Test.Tasty.HUnit


luaTestCase :: HasCallStack => String -> T.Text -> TestTree
luaTestCase name = typedIOLuaTestCase (microarch ASync SlaveSPI) pInt name def


typedLuaTestCase ::
    ( HasCallStack, Val x, Integral x
    ) => BusNetwork String String x Int -> Proxy x -> String -> T.Text -> TestTree
typedLuaTestCase arch proxy name = typedIOLuaTestCase arch proxy name def


typedIOLuaTestCase ::
    ( HasCallStack, Val x, Integral x
    ) => BusNetwork String String x Int -> Proxy x -> String
    -> [ ( String, [x] ) ] -> T.Text -> TestTree
typedIOLuaTestCase arch proxy name received src = testCase name $ do
    let wd = "lua_" ++ toModuleName name
    status <- runLua arch proxy wd received src
    let errMsg = codeBlock (T.unpack src) ++ "-- runTargetSynthesis fail with: " ++ fromLeft undefined status
    assertBool errMsg $ isRight status


runLua :: forall x.
    ( Val x, Integral x
    ) => BusNetwork String String x Int -> Proxy x -> String
    -> [ ( String, [x] ) ] -> T.Text -> IO ( Either String () )
runLua arch _proxy wd received src = do
    report <- runTargetSynthesisWithUniqName (def :: TargetSynthesis String String x Int)
        { tName=wd
        , tMicroArch=arch
        , tSourceCode=Just src
        , tReceivedValues=received
        }
    return $ case report of
        Right TestbenchReport{ tbStatus=True } -> Right ()
        Left err -> Left $ "synthesis process fail: " ++ err
        Right TestbenchReport{ tbCompilerDump } | length tbCompilerDump > 2 -- [ "stdout:", "stderr:" ]
            -> Left $ "icarus synthesis error:\n" ++ S.join "\n" tbCompilerDump
        Right TestbenchReport{ tbSimulationDump }
            -> Left $ "icarus simulation error:\n" ++ S.join "\n" tbSimulationDump
