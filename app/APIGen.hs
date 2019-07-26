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
import           NITTA.UIBackend
import           System.Directory              (createDirectoryIfMissing)
import           NITTA.UIBackend.Timeline
import           Numeric.Interval
import           System.Console.CmdArgs
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
$(deriveTypeScript defaultOptions ''ProcessTimelines)
$(deriveTypeScript defaultOptions ''TimelinePoint)
$(deriveTypeScript defaultOptions ''Interval)

main = do
    APIGen{ port, opath } <- cmdArgs apiGenArgs

    putStrLn "Create output derictory..."
    createDirectoryIfMissing True opath
    putStrLn "Create output derictory...OK"

    putStrLn "Generate rest_api.js library..."
    prepareJSAPI port opath
    putStrLn "Generate rest_api.js library...OK"

    putStrLn "Generate typescript interface..."
    writeFile (joinPath [ opath, "types.ts" ]) $ formatTSDeclarations $ foldl1 (<>)
        [ getTypeScriptDeclarations (Proxy :: Proxy ViewPointID)
        , getTypeScriptDeclarations (Proxy :: Proxy ProcessTimelines)
        , getTypeScriptDeclarations (Proxy :: Proxy TimelinePoint)
        , getTypeScriptDeclarations (Proxy :: Proxy Interval)
        ]
    putStrLn "Generate typescript interface...OK"
