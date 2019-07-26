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
import qualified Data.String.Utils        as S
import           NITTA.UIBackend
import           NITTA.UIBackend.Timeline
import           Numeric.Interval
import           System.Console.CmdArgs
import           System.Directory         (createDirectoryIfMissing)
import           System.FilePath.Posix    (joinPath)


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
$(deriveTypeScript defaultOptions ''ProcessTimelines)


main = do
    APIGen{ port, opath } <- cmdArgs apiGenArgs

    putStrLn "Create output derictory..."
    createDirectoryIfMissing True opath
    putStrLn "Create output derictory...OK"

    putStrLn "Generate rest_api.js library..."
    prepareJSAPI port opath
    putStrLn "Generate rest_api.js library...OK"

    putStrLn "Generate typescript interface..."
    let ts = formatTSDeclarations $ foldl1 (<>)
            [ getTypeScriptDeclarations (Proxy :: Proxy ViewPointID)
            , getTypeScriptDeclarations (Proxy :: Proxy TimelinePoint)
            , getTypeScriptDeclarations (Proxy :: Proxy Interval)
            , getTypeScriptDeclarations (Proxy :: Proxy ProcessTimelines)
            ]
    writeFile (joinPath [ opath, "types.ts" ]) $ S.replace "type " "export type " (ts ++ "\n")
    putStrLn "Generate typescript interface...OK"
