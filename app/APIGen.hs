{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-cse #-}

{-|
Module      : APIGen
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module Main ( main ) where

import           Data.Aeson
import           Data.Aeson.TypeScript.TH
import           Data.Proxy
import qualified Data.String.Utils             as S
import           NITTA.Model.Problems.Refactor
import           NITTA.Model.Types
import           NITTA.Project.Parts.TestBench
import           NITTA.Synthesis.Types
import           NITTA.UIBackend
import           NITTA.UIBackend.Marshalling
import           NITTA.UIBackend.Timeline
import           NITTA.UIBackend.VisJS
import           Numeric.Interval
import           System.Console.CmdArgs
import           System.Directory              (createDirectoryIfMissing)
import           System.FilePath.Posix         (joinPath)


data APIGen
    = APIGen
        { port  :: Int
        , opath :: FilePath
        }
    deriving ( Show, Data, Typeable )

apiGenArgs = APIGen
    { port=8080 &= help "WebUI port"
    , opath="./web/src/gen" &= typ "output path"
    }


$(deriveTypeScript defaultOptions ''ViewPointID)
$(deriveTypeScript defaultOptions ''TimelinePoint)
$(deriveTypeScript defaultOptions ''Interval)
$(deriveTypeScript defaultOptions ''TimeConstrain)
$(deriveTypeScript defaultOptions ''TimelineWithViewPoint)
$(deriveTypeScript defaultOptions ''ProcessTimelines)
$(deriveTypeScript defaultOptions ''TestbenchReport)

$(deriveTypeScript defaultOptions ''Refactor)
$(deriveTypeScript defaultOptions ''Parameters)

$(deriveTypeScript defaultOptions ''NId)
$(deriveTypeScript defaultOptions ''TreeView)
$(deriveTypeScript defaultOptions ''SynthesisNodeView)

$(deriveTypeScript defaultOptions ''DataflowEndpointView)
$(deriveTypeScript defaultOptions ''SynthesisDecisionView)
$(deriveTypeScript defaultOptions ''EdgeView)

$(deriveTypeScript defaultOptions ''GraphEdge)
$(deriveTypeScript defaultOptions ''NodeElement)
$(deriveTypeScript defaultOptions ''GraphStructure)

main = do
    APIGen{ port, opath } <- cmdArgs apiGenArgs

    putStrLn "Create output directory..."
    createDirectoryIfMissing True opath
    putStrLn "Create output directory...OK"

    putStrLn "Generate rest_api.js library..."
    prepareJSAPI port opath
    putStrLn "Generate rest_api.js library...OK"

    putStrLn "Generate typescript interface..."
    let ts = formatTSDeclarations $ foldl1 (<>)
            [ getTypeScriptDeclarations (Proxy :: Proxy ViewPointID)
            , getTypeScriptDeclarations (Proxy :: Proxy TimelinePoint)
            , getTypeScriptDeclarations (Proxy :: Proxy Interval)
            , getTypeScriptDeclarations (Proxy :: Proxy TimeConstrain)
            , getTypeScriptDeclarations (Proxy :: Proxy TimelineWithViewPoint)
            , getTypeScriptDeclarations (Proxy :: Proxy ProcessTimelines)
            , getTypeScriptDeclarations (Proxy :: Proxy TestbenchReport)

            , getTypeScriptDeclarations (Proxy :: Proxy Refactor)
            , getTypeScriptDeclarations (Proxy :: Proxy Parameters)

            , getTypeScriptDeclarations (Proxy :: Proxy NId)
            , getTypeScriptDeclarations (Proxy :: Proxy TreeView)
            , getTypeScriptDeclarations (Proxy :: Proxy SynthesisNodeView)

            , getTypeScriptDeclarations (Proxy :: Proxy DataflowEndpointView)
            , getTypeScriptDeclarations (Proxy :: Proxy SynthesisDecisionView)
            , getTypeScriptDeclarations (Proxy :: Proxy EdgeView)

            , getTypeScriptDeclarations (Proxy :: Proxy GraphEdge)
            , getTypeScriptDeclarations (Proxy :: Proxy NodeElement)
            , getTypeScriptDeclarations (Proxy :: Proxy GraphStructure)
            ]
    writeFile (joinPath [ opath, "types.ts" ])
        $ S.replace "type " "export type "           -- export all types
        $ S.replace "interface " "export interface " -- export all interfaces
        $ S.replace "[k: T1]" "[k: string]"          -- dirty hack for fixing map types for TestbenchReport
        $ S.replace "[k: T2]" "[k: string]"          -- dirty hack for fixing map types for TestbenchReport
        $ ts ++ "\n"
    putStrLn "Generate typescript interface...OK"
