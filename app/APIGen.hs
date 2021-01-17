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
Description :
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
import NITTA.Model.Problems
import NITTA.Model.Problems.ViewHelper
import NITTA.Model.Types
import NITTA.Synthesis.Binding
import NITTA.Synthesis.Dataflow
import NITTA.Synthesis.Refactor
import NITTA.Synthesis.Types
import NITTA.UIBackend
import NITTA.UIBackend.Timeline
import NITTA.UIBackend.ViewHelper
import NITTA.UIBackend.VisJS
import Numeric.Interval
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
    , opath :: FilePath
    , verbose :: Bool
    }
    deriving (Show, Data, Typeable)

apiGenArgs =
    APIGen
        { port = 8080 &= help "nitta server port"
        , opath = "./web/src/gen" &= typ "output path"
        , verbose = False &= help "Verbose"
        }

$(deriveTypeScript defaultOptions ''ViewPointID)
$(deriveTypeScript defaultOptions ''TimelinePoint)
$(deriveTypeScript defaultOptions ''Interval)
$(deriveTypeScript defaultOptions ''TimeConstrain)
$(deriveTypeScript defaultOptions ''TimelineWithViewPoint)
$(deriveTypeScript defaultOptions ''ProcessTimelines)
$(deriveTypeScript defaultOptions ''TestbenchReportView)

$(deriveTypeScript defaultOptions ''SID) -- in according to custom ToJSON instance, the real type description is hardcoded.
$(deriveTypeScript defaultOptions ''FView)
$(deriveTypeScript defaultOptions ''TreeView)
$(deriveTypeScript defaultOptions ''SynthesisNodeView)

$(deriveTypeScript defaultOptions ''DataflowEndpointView)

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

$(deriveTypeScript defaultOptions ''IntervalView)
$(deriveTypeScript defaultOptions ''TimeConstrainView)
$(deriveTypeScript defaultOptions ''EndpointRole)
$(deriveTypeScript defaultOptions ''EndpointSt)
$(deriveTypeScript defaultOptions ''EndpointStView)

main = do
    APIGen{port, opath, verbose} <- cmdArgs apiGenArgs

    let level = if verbose then DEBUG else NOTICE
    h <-
        streamHandler stdout level >>= \lh ->
            return $
                setFormatter lh (simpleLogFormatter "[$prio : $loggername] $msg")

    removeAllHandlers
    updateGlobalLogger "NITTA" (setLevel level . addHandler h)

    infoM "NITTA.APIGen" "Create output directory..."
    createDirectoryIfMissing True opath
    infoM "NITTA.APIGen" "Create output directory...OK"

    infoM "NITTA.APIGen" $ "Expected nitta server port: " <> show port
    writeFile (joinPath [opath, "PORT"]) $ show port

    infoM "NITTA.APIGen" "Generate rest_api.js library..."
    prepareJSAPI port opath
    infoM "NITTA.APIGen" "Generate rest_api.js library...OK"

    infoM "NITTA.APIGen" "Generate typescript interface..."
    let ts =
            formatTSDeclarations $
                foldl1
                    (<>)
                    [ getTypeScriptDeclarations (Proxy :: Proxy ViewPointID)
                    , getTypeScriptDeclarations (Proxy :: Proxy TimelinePoint)
                    , getTypeScriptDeclarations (Proxy :: Proxy Interval)
                    , getTypeScriptDeclarations (Proxy :: Proxy TimeConstrain)
                    , getTypeScriptDeclarations (Proxy :: Proxy TimelineWithViewPoint)
                    , getTypeScriptDeclarations (Proxy :: Proxy ProcessTimelines)
                    , getTypeScriptDeclarations (Proxy :: Proxy TestbenchReportView)
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
                    , getTypeScriptDeclarations (Proxy :: Proxy SynthesisNodeView)
                    , getTypeScriptDeclarations (Proxy :: Proxy DataflowEndpointView)
                    , getTypeScriptDeclarations (Proxy :: Proxy NodeView)
                    , getTypeScriptDeclarations (Proxy :: Proxy GraphEdge)
                    , getTypeScriptDeclarations (Proxy :: Proxy GraphNode)
                    , getTypeScriptDeclarations (Proxy :: Proxy GraphStructure)
                    , getTypeScriptDeclarations (Proxy :: Proxy IntervalView)
                    , getTypeScriptDeclarations (Proxy :: Proxy TimeConstrainView)
                    , getTypeScriptDeclarations (Proxy :: Proxy EndpointRole)
                    , getTypeScriptDeclarations (Proxy :: Proxy EndpointSt)
                    , getTypeScriptDeclarations (Proxy :: Proxy EndpointStView)
                    ]
    writeFile (joinPath [opath, "types.ts"]) $
        foldl
            (\st (old, new) -> S.replace old new st)
            (ts ++ "\n" ++ "type NId = string\n")
            [ ("type ", "export type ") -- export all types
            , ("interface ", "export interface ") -- export all interfaces
            , ("[k: T1]", "[k: string]") -- dirty hack for fixing map types for TestbenchReport
            , ("[k: T2]", "[k: string]") -- dirty hack for fixing map types for TestbenchReport
            ]
    infoM "NITTA.APIGen" "Generate typescript interface...OK"

    infoM "NITTA.APIGen" "Generate REST API description..."
    writeFile (joinPath [opath, "rest_api.markdown"]) $ restDocs port
    infoM "NITTA.APIGen" "Generate REST API description...ok"
