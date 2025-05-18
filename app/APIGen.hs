{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -fno-warn-orphans #-}

-- FIXME: should be removed on aeson-typescript update
{-# OPTIONS -fno-warn-redundant-constraints #-}

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
import Data.String.Utils qualified as S
import Data.Version
import NITTA.Model.Microarchitecture.Types
import NITTA.Model.Networks.Types
import NITTA.Model.Problems
import NITTA.Model.Problems.ViewHelper
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Time
import NITTA.Project.TestBench
import NITTA.Synthesis
import NITTA.Synthesis.Analysis
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

$(deriveTypeScript defaultOptions ''ParallelismType)

$(deriveTypeScript defaultOptions ''BindMetrics)
$(deriveTypeScript defaultOptions ''AllocationMetrics)
$(deriveTypeScript defaultOptions ''DataflowMetrics)
$(deriveTypeScript defaultOptions ''BreakLoopMetrics)
$(deriveTypeScript defaultOptions ''OptimizeAccumMetrics)
$(deriveTypeScript defaultOptions ''OptimizeLutMetrics)
$(deriveTypeScript defaultOptions ''ResolveDeadlockMetrics)

$(deriveTypeScript defaultOptions ''ViewPointID)
$(deriveTypeScript defaultOptions ''Interval)
$(deriveTypeScript defaultOptions ''TimelinePoint)
$(deriveTypeScript defaultOptions ''TimeConstraint)
$(deriveTypeScript defaultOptions ''TimelineWithViewPoint)
$(deriveTypeScript defaultOptions ''ProcessTimelines)

$(deriveTypeScript defaultOptions ''Sid) -- in according to custom ToJSON instance, the real type description is hardcoded.
$(deriveTypeScript defaultOptions ''FView)
$(deriveTypeScript defaultOptions ''TreeView)
$(deriveTypeScript defaultOptions ''TreeInfo)
$(deriveTypeScript defaultOptions ''ShortNodeView)

$(deriveTypeScript defaultOptions ''EndpointRole)
$(deriveTypeScript defaultOptions ''EndpointSt)

$(deriveTypeScript defaultOptions ''DecisionView)
$(deriveTypeScript defaultOptions ''NodeView)

$(deriveTypeScript defaultOptions ''GraphEdge)
$(deriveTypeScript defaultOptions ''GraphNode)
$(deriveTypeScript defaultOptions ''GraphStructure)

$(deriveTypeScript defaultOptions ''UnitEndpoints)

$(deriveTypeScript defaultOptions ''StepInfoView)
$(deriveTypeScript defaultOptions ''Step)
$(deriveTypeScript defaultOptions ''Relation)
$(deriveTypeScript defaultOptions ''Process)

$(deriveTypeScript defaultOptions ''TestbenchReport)

-- Microarchitecture
$(deriveTypeScript defaultOptions ''IOSynchronization)
$(deriveTypeScript defaultOptions ''UnitDesc)
$(deriveTypeScript defaultOptions ''NetworkDesc)
$(deriveTypeScript defaultOptions ''MicroarchitectureDesc)

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
                    , getTypeScriptDeclarations (Proxy :: Proxy (Interval T))
                    , getTypeScriptDeclarations (Proxy :: Proxy (TimelinePoint T))
                    , getTypeScriptDeclarations (Proxy :: Proxy (TimeConstraint T))
                    , getTypeScriptDeclarations (Proxy :: Proxy (TimelineWithViewPoint T))
                    , getTypeScriptDeclarations (Proxy :: Proxy (ProcessTimelines T))
                    , -- synthesis tree
                      getTypeScriptDeclarations (Proxy :: Proxy DecisionView)
                    , -- metrics
                      getTypeScriptDeclarations (Proxy :: Proxy BindMetrics)
                    , getTypeScriptDeclarations (Proxy :: Proxy AllocationMetrics)
                    , getTypeScriptDeclarations (Proxy :: Proxy DataflowMetrics)
                    , getTypeScriptDeclarations (Proxy :: Proxy BreakLoopMetrics)
                    , getTypeScriptDeclarations (Proxy :: Proxy OptimizeAccumMetrics)
                    , getTypeScriptDeclarations (Proxy :: Proxy OptimizeLutMetrics)
                    , getTypeScriptDeclarations (Proxy :: Proxy ResolveDeadlockMetrics)
                    , -- other
                      getTypeScriptDeclarations (Proxy :: Proxy FView)
                    , getTypeScriptDeclarations (Proxy :: Proxy (TreeView T))
                    , getTypeScriptDeclarations (Proxy :: Proxy TreeInfo)
                    , getTypeScriptDeclarations (Proxy :: Proxy ShortNodeView)
                    , getTypeScriptDeclarations (Proxy :: Proxy (NodeView T1 T2 T3 T))
                    , getTypeScriptDeclarations (Proxy :: Proxy GraphEdge)
                    , getTypeScriptDeclarations (Proxy :: Proxy GraphNode)
                    , getTypeScriptDeclarations (Proxy :: Proxy (GraphStructure T))
                    , getTypeScriptDeclarations (Proxy :: Proxy (EndpointRole T))
                    , getTypeScriptDeclarations (Proxy :: Proxy (EndpointSt T1 T2))
                    , getTypeScriptDeclarations (Proxy :: Proxy (UnitEndpoints T1 T2 T3))
                    , -- Process
                      getTypeScriptDeclarations (Proxy :: Proxy (Process T1 T2))
                    , getTypeScriptDeclarations (Proxy :: Proxy (Step T1 T2))
                    , getTypeScriptDeclarations (Proxy :: Proxy Relation)
                    , getTypeScriptDeclarations (Proxy :: Proxy StepInfoView)
                    , getTypeScriptDeclarations (Proxy :: Proxy (TestbenchReport T1 T2))
                    , -- Microarchitecture
                      getTypeScriptDeclarations (Proxy :: Proxy (MicroarchitectureDesc T))
                    , getTypeScriptDeclarations (Proxy :: Proxy (NetworkDesc T))
                    , getTypeScriptDeclarations (Proxy :: Proxy (UnitDesc T))
                    , getTypeScriptDeclarations (Proxy :: Proxy IOSynchronization)
                    , getTypeScriptDeclarations (Proxy :: Proxy ParallelismType)
                    ]
    writeFile (joinPath [output_path, "types.ts"]) $
        foldl
            (\st (old, new) -> S.replace old new st)
            (ts ++ "\n" ++ "type NId = string\n")
            [ ("type ", "export type ") -- export all types
            , ("interface ", "export interface ") -- export all interfaces
            , ("[k in T1]?", "[k: string]") -- dirty hack for fixing map types for TestbenchReport
            , ("[k in T2]?", "[k: string]") -- dirty hack for fixing map types for TestbenchReport
            , ("[k in number]?: number", "[k: number]: number") -- dirty hack for fixing map types for TreeInfo
            ]
    infoM "NITTA.APIGen" $ "Generate typescript interface " <> output_path <> "/types.ts...OK"

    infoM "NITTA.APIGen" $ "Generate REST API description " <> output_path <> "/rest_api.markdown..."
    writeFile (joinPath [output_path, "rest_api.markdown"]) $ restDocs port
    infoM "NITTA.APIGen" $ "Generate REST API description " <> output_path <> "/rest_api.markdown...ok"
