{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : APIGen
Description : Generate REST API files for NITTA UI Backend
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module APIGen (
    main,
) where

import Data.Aeson
import Data.Aeson.TypeScript.TH
import Data.Proxy
import qualified Data.String.Utils as S
import Data.Version
import NITTA.Model.Microarchitecture
import NITTA.Model.Networks.Types
import NITTA.Model.Problems
import NITTA.Model.Problems.ViewHelper
import NITTA.Model.Types
import NITTA.Project.TestBench
import NITTA.Synthesis
import NITTA.UIBackend
import NITTA.UIBackend.REST
import NITTA.UIBackend.Timeline
import NITTA.UIBackend.ViewHelper
import NITTA.UIBackend.VisJS
import Numeric.Interval.NonEmpty
import Paths_nitta (version)
import System.Console.CmdArgs
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (joinPath)
import System.IO (stdout)
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger

data APIGen = APIGen
    { port :: Int
    , output_path :: FilePath
    , verbose :: Bool
    }
    deriving (Show, Data, Typeable)

apiGenArgs =
    APIGen
        { port = 8080 &= help "NITTA UI Backend will start on this port"
        , output_path = apiPath &= help ("Place the output into specified directory (default: " <> apiPath <> ")")
        , verbose = False &= help "Verbose"
        }
        &= program "nitta-api-gen"
        &= summary ("nitta-api-gen v" ++ showVersion version ++ " - Generate REST API files for NITTA UI Backend")

$(deriveTypeScript defaultOptions ''ViewPointID)
$(deriveTypeScript defaultOptions ''TimelinePoint)
$(deriveTypeScript defaultOptions ''Interval)
$(deriveTypeScript defaultOptions ''TimeConstraint)
$(deriveTypeScript defaultOptions ''TimelineWithViewPoint)
$(deriveTypeScript defaultOptions ''ProcessTimelines)

$(deriveTypeScript defaultOptions ''SID) -- in according to custom ToJSON instance, the real type description is hardcoded.
$(deriveTypeScript defaultOptions ''FView)
$(deriveTypeScript defaultOptions ''TreeView)
$(deriveTypeScript defaultOptions ''ShortNodeView)

$(deriveTypeScript defaultOptions ''NodeView)
$(deriveTypeScript defaultOptions ''DecisionView)
$(deriveTypeScript defaultOptions ''BindMetrics)
$(deriveTypeScript defaultOptions ''DataflowMetrics)
$(deriveTypeScript defaultOptions ''BreakLoopMetrics)
$(deriveTypeScript defaultOptions ''OptimizeAccumMetrics)
$(deriveTypeScript defaultOptions ''ResolveDeadlockMetrics)

$(deriveTypeScript defaultOptions ''GraphEdge)
$(deriveTypeScript defaultOptions ''GraphNode)
$(deriveTypeScript defaultOptions ''GraphStructure)

$(deriveTypeScript defaultOptions ''EndpointRole)
$(deriveTypeScript defaultOptions ''EndpointSt)
$(deriveTypeScript defaultOptions ''UnitEndpoints)

$(deriveTypeScript defaultOptions ''TestbenchReport)

-- Microarchitecture
$(deriveTypeScript defaultOptions ''MicroarchitectureDesc)
$(deriveTypeScript defaultOptions ''NetworkDesc)
$(deriveTypeScript defaultOptions ''UnitDesc)
$(deriveTypeScript defaultOptions ''IOSynchronization)

main = do
    APIGen{port, output_path, verbose} <- cmdArgs apiGenArgs
    let level = if verbose then DEBUG else NOTICE
    h <-
        streamHandler stdout level >>= \lh ->
            return $
                setFormatter lh (simpleLogFormatter "[$prio : $loggername] $msg")

    removeAllHandlers
    updateGlobalLogger "NITTA" (setLevel level . addHandler h)

    infoM "NITTA.APIGen" "Create output directory..."
    createDirectoryIfMissing True output_path
    infoM "NITTA.APIGen" "Create output directory...OK"

    infoM "NITTA.APIGen" $ "Expected nitta server port: " <> show port
    writeFile (joinPath [output_path, "PORT"]) $ show port

    infoM "NITTA.APIGen" $ "Generate call library " <> output_path <> "/rest_api.js..."
    prepareJSAPI port output_path
    infoM "NITTA.APIGen" $ "Generate call library " <> output_path <> "/rest_api.js...OK"

    infoM "NITTA.APIGen" $ "Generate typescript interface " <> output_path <> "/types.ts..."
    let ts =
            formatTSDeclarations $
                foldl1
                    (<>)
                    [ getTypeScriptDeclarations (Proxy :: Proxy ViewPointID)
                    , getTypeScriptDeclarations (Proxy :: Proxy TimelinePoint)
                    , getTypeScriptDeclarations (Proxy :: Proxy Interval)
                    , getTypeScriptDeclarations (Proxy :: Proxy TimeConstraint)
                    , getTypeScriptDeclarations (Proxy :: Proxy TimelineWithViewPoint)
                    , getTypeScriptDeclarations (Proxy :: Proxy ProcessTimelines)
                    , -- synthesis tree
                      getTypeScriptDeclarations (Proxy :: Proxy DecisionView)
                    , -- metrics
                      getTypeScriptDeclarations (Proxy :: Proxy BindMetrics)
                    , getTypeScriptDeclarations (Proxy :: Proxy DataflowMetrics)
                    , getTypeScriptDeclarations (Proxy :: Proxy BreakLoopMetrics)
                    , getTypeScriptDeclarations (Proxy :: Proxy OptimizeAccumMetrics)
                    , getTypeScriptDeclarations (Proxy :: Proxy ResolveDeadlockMetrics)
                    , -- other
                      getTypeScriptDeclarations (Proxy :: Proxy FView)
                    , getTypeScriptDeclarations (Proxy :: Proxy TreeView)
                    , getTypeScriptDeclarations (Proxy :: Proxy ShortNodeView)
                    , getTypeScriptDeclarations (Proxy :: Proxy NodeView)
                    , getTypeScriptDeclarations (Proxy :: Proxy GraphEdge)
                    , getTypeScriptDeclarations (Proxy :: Proxy GraphNode)
                    , getTypeScriptDeclarations (Proxy :: Proxy GraphStructure)
                    , getTypeScriptDeclarations (Proxy :: Proxy EndpointRole)
                    , getTypeScriptDeclarations (Proxy :: Proxy EndpointSt)
                    , getTypeScriptDeclarations (Proxy :: Proxy UnitEndpoints)
                    , getTypeScriptDeclarations (Proxy :: Proxy TestbenchReport)
                    , -- Microarchitecture
                      getTypeScriptDeclarations (Proxy :: Proxy MicroarchitectureDesc)
                    , getTypeScriptDeclarations (Proxy :: Proxy NetworkDesc)
                    , getTypeScriptDeclarations (Proxy :: Proxy UnitDesc)
                    , getTypeScriptDeclarations (Proxy :: Proxy IOSynchronization)
                    ]
    writeFile (joinPath [output_path, "types.ts"]) $
        foldl
            (\st (old, new) -> S.replace old new st)
            (ts ++ "\n" ++ "type NId = string\n")
            [ ("type ", "export type ") -- export all types
            , ("interface ", "export interface ") -- export all interfaces
            , ("[k: T1]", "[k: string]") -- dirty hack for fixing map types for TestbenchReport
            , ("[k: T2]", "[k: string]") -- dirty hack for fixing map types for TestbenchReport
            ]
    infoM "NITTA.APIGen" $ "Generate typescript interface " <> output_path <> "/types.ts...OK"

    infoM "NITTA.APIGen" $ "Generate REST API description " <> output_path <> "/rest_api.markdown..."
    writeFile (joinPath [output_path, "rest_api.markdown"]) $ restDocs port
    infoM "NITTA.APIGen" $ "Generate REST API description " <> output_path <> "/rest_api.markdown...ok"
