{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}

{-|
Module      : NITTA.UIBackend
Description : HTTP backend for the NITTA web UI
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.UIBackend
    ( backendServer
    , prepareStaticFiles
    , prepareJSAPI
    ) where

import           Control.Exception             (SomeException, try)
import           Control.Monad                 (unless, when)
import           Data.Either
import           GHC.IO.Encoding               (setLocaleEncoding, utf8)
import           Network.Simple.TCP            (connect)
import           Network.Wai.Handler.Warp      (run)
import           Network.Wai.Middleware.Cors   (simpleCors)
import           NITTA.Synthesis.Tree
import           NITTA.UIBackend.Marshalling   ()
import           NITTA.UIBackend.REST
import           Servant
import qualified Servant.JS                    as SJS
import           System.Exit                   (ExitCode (..), die)
import           System.FilePath.Posix         (joinPath)
import           System.Process
import           Text.InterpolatedString.Perl6 (qq)


prepareJSAPI port path = do
    let prefix = [qq|import axios from 'axios';
var api = \{\};
export default api;
/* eslint no-useless-concat: "off" */|]
    let axios' = SJS.axiosWith SJS.defAxiosOptions SJS.defCommonGeneratorOptions
            { SJS.urlPrefix=[qq|http://localhost:$port|]
            , SJS.moduleName="api"
            }
    SJS.writeJSForAPI (Proxy :: Proxy (SynthesisAPI String String Int Int)) ((prefix <>) . axios') $ joinPath [ path, "rest_api.js"]


prepareStaticFiles = do
    putStrLn "Generate statis files..."
    ( exitCode, out, err )
        <- readCreateProcessWithExitCode
            (shell "npm run-script build"){ cwd=Just "web" }
            []
    putStrLn "npm output:"
    putStrLn out
    when (exitCode /= ExitSuccess || not (null err)) $ do
        putStrLn "npm error:"
        putStrLn err
        die "npm compilation failed!"
    putStrLn "Generate statis files...OK"



application receivedValues model = do
    root <- mkRootNodeIO model
    return $ serve
        ( Proxy :: Proxy
            (    SynthesisAPI _ _ _ _
            :<|> Get '[JSON] () -- root
            :<|> Raw
            )
        )
        (    synthesisServer BackendCntx{ root, receivedValues }
        :<|> throwError err301 { errHeaders = [("Location", "index.html")] }
        :<|> serveDirectoryWebApp (joinPath ["web", "build"])
        )


isLocalPortFree port
    = isLeft <$> (try $ connect "localhost" (show port) (\_ -> return ()) :: IO (Either SomeException ()))


-- |Run backend server.
backendServer port receivedValues modelState = do
    putStrLn $ "> Running NITTA server at http://localhost:" ++ show port ++ " ..."
    -- on OS X, if we run system with busy port - application ignore that.
    -- see: https://nitta.io/nitta-corp/nitta/issues/9
    isFree <- isLocalPortFree port
    unless isFree $ error "resource busy (Port already in use)"
    app <- application receivedValues modelState
    setLocaleEncoding utf8
    run port $ simpleCors app
