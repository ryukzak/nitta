{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
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
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Refactor
import           NITTA.Project.Parts.TestBench
import           NITTA.Synthesis.Types
import           NITTA.UIBackend
import           NITTA.UIBackend.Timeline
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
$(deriveTypeScript defaultOptions ''TimelineWithViewPoint)
$(deriveTypeScript defaultOptions ''ProcessTimelines)
$(deriveTypeScript defaultOptions ''TestbenchReport)


$(deriveTypeScript defaultOptions ''I)
$(deriveTypeScript defaultOptions ''O)
$(deriveTypeScript defaultOptions ''X)
$(deriveTypeScript defaultOptions ''Loop)
$(deriveTypeScript defaultOptions ''LoopIn)
$(deriveTypeScript defaultOptions ''LoopOut)
$(deriveTypeScript defaultOptions ''Refactor)
$(deriveTypeScript defaultOptions ''Parameters)


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
            , getTypeScriptDeclarations (Proxy :: Proxy TimelineWithViewPoint)
            , getTypeScriptDeclarations (Proxy :: Proxy ProcessTimelines)
            , getTypeScriptDeclarations (Proxy :: Proxy TestbenchReport)
            ]
    writeFile (joinPath [ opath, "types.ts" ])
        $ S.replace "type " "export type "    -- export all types
        $ S.replace "[k: T1]" "[k: string]"   -- dirty hack for fixing map types for TestbenchReport
        $ ts ++ "\n"
    putStrLn "Generate typescript interface...OK"
