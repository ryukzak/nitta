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
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module APIGen (
    main,
    HistoryStep (..), -- only for suppress warning
) where

import Data.Aeson
import Data.Aeson.TypeScript.TH
import Data.Proxy
import qualified Data.String.Utils as S
import NITTA.Model.Problems
import NITTA.Model.Types
import NITTA.Synthesis.Tree
import NITTA.UIBackend
import NITTA.UIBackend.Orphans ()
import NITTA.UIBackend.Timeline
import NITTA.UIBackend.ViewHelper
import NITTA.UIBackend.VisJS
import Numeric.Interval
import System.Console.CmdArgs
import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix (joinPath)

data APIGen = APIGen
    { port :: Int
    , opath :: FilePath
    }
    deriving (Show, Data, Typeable)

apiGenArgs =
    APIGen
        { port = 8080 &= help "nitta server port"
        , opath = "./web/src/gen" &= typ "output path"
        }

$(deriveTypeScript defaultOptions ''ViewPointID)
$(deriveTypeScript defaultOptions ''TimelinePoint)
$(deriveTypeScript defaultOptions ''Interval)
$(deriveTypeScript defaultOptions ''TimeConstrain)
$(deriveTypeScript defaultOptions ''TimelineWithViewPoint)
$(deriveTypeScript defaultOptions ''ProcessTimelines)
$(deriveTypeScript defaultOptions ''TestbenchReportView)

$(deriveTypeScript defaultOptions ''RefactorView)
$(deriveTypeScript defaultOptions ''ParametersView)

$(deriveTypeScript defaultOptions ''NId) -- in according to custom ToJSON instance, the real type description is hardcoded.
$(deriveTypeScript defaultOptions ''FView)
$(deriveTypeScript defaultOptions ''TreeView)
$(deriveTypeScript defaultOptions ''SynthesisNodeView)

$(deriveTypeScript defaultOptions ''DataflowEndpointView)
$(deriveTypeScript defaultOptions ''SynthesisStatementView)
$(deriveTypeScript defaultOptions ''NodeView)
$(deriveTypeScript defaultOptions ''EdgeView)

$(deriveTypeScript defaultOptions ''GraphEdge)
$(deriveTypeScript defaultOptions ''GraphNode)
$(deriveTypeScript defaultOptions ''GraphStructure)

$(deriveTypeScript defaultOptions ''IntervalView)
$(deriveTypeScript defaultOptions ''TimeConstrainView)
$(deriveTypeScript defaultOptions ''EndpointRole)
$(deriveTypeScript defaultOptions ''EndpointSt)
$(deriveTypeScript defaultOptions ''EndpointStView)

data HistoryStep tag v x tp = HistoryStep NId (SynthesisStatementView tag v x tp)
$(deriveTypeScript defaultOptions ''HistoryStep)

main = do
    APIGen{port, opath} <- cmdArgs apiGenArgs

    putStrLn "Create output directory..."
    createDirectoryIfMissing True opath
    putStrLn "Create output directory...OK"

    putStrLn $ "Expected nitta server port: " <> show port
    writeFile (joinPath [opath, "PORT"]) $ show port

    putStrLn "Generate rest_api.js library..."
    prepareJSAPI port opath
    putStrLn "Generate rest_api.js library...OK"

    putStrLn "Generate typescript interface..."
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
                    , getTypeScriptDeclarations (Proxy :: Proxy RefactorView)
                    , getTypeScriptDeclarations (Proxy :: Proxy ParametersView)
                    , getTypeScriptDeclarations (Proxy :: Proxy FView)
                    , getTypeScriptDeclarations (Proxy :: Proxy TreeView)
                    , getTypeScriptDeclarations (Proxy :: Proxy SynthesisNodeView)
                    , getTypeScriptDeclarations (Proxy :: Proxy DataflowEndpointView)
                    , getTypeScriptDeclarations (Proxy :: Proxy SynthesisStatementView)
                    , getTypeScriptDeclarations (Proxy :: Proxy HistoryStep)
                    , getTypeScriptDeclarations (Proxy :: Proxy NodeView)
                    , getTypeScriptDeclarations (Proxy :: Proxy EdgeView)
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
    putStrLn "Generate typescript interface...OK"

    putStrLn "Generate REST API description..."
    writeFile (joinPath [opath, "rest_api.markdown"]) $ restDocs port
    putStrLn "Generate REST API description...ok"
