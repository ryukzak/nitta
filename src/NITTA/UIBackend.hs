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

import           Control.Monad                 (when)
import           Data.Monoid                   ((<>))
import           GHC.IO.Encoding               (setLocaleEncoding, utf8)
import           Network.Wai.Handler.Warp      (run)
import           Network.Wai.Middleware.Cors   (simpleCors)
import           NITTA.Synthesis.Types
import           NITTA.UIBackend.Marshalling   ()
import           NITTA.UIBackend.REST
import           Servant
import qualified Servant.JS                    as SJS
import           Servant.Server.StaticFiles    (serveDirectoryWebApp)
import           System.Directory              (createDirectoryIfMissing)
import           System.Exit                   (ExitCode (..), die)
import           System.FilePath.Posix         (joinPath)
import           System.Process
import           Text.InterpolatedString.Perl6 (qq)


prepareJSAPI port = do
    putStrLn "Generate rest_api.js library..."
    let prefix = [qq|import axios from 'axios';
var api = \{\};
export default api;|]
    let axios' = SJS.axiosWith SJS.defAxiosOptions SJS.defCommonGeneratorOptions
            { SJS.urlPrefix=[qq|http://localhost:$port|]
            , SJS.moduleName="api"
            }
    createDirectoryIfMissing True $ joinPath ["web", "src", "gen"]
    SJS.writeJSForAPI (Proxy :: Proxy (SynthesisAPI String String Int Int)) ((prefix <>) . axios') $ joinPath ["web", "src", "gen", "rest_api.js"]
    putStrLn "Generate rest_api.js library...OK"


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



application model = do
    root <- mkRootNodeIO model
    return $ serve
        ( Proxy :: Proxy
            (    SynthesisAPI _ _ _ _
            :<|> Raw
            ) )
        (    synthesisServer root
        :<|> serveDirectoryWebApp (joinPath ["web", "build"])
        )


-- |Run backend server. Parameters:
--
-- - if true - prepare static files for the web UI by @npm@;
-- - initial model state.
backendServer port modelState = do
    putStrLn $ "Running NITTA server at http://localhost:" ++ show port ++ "/index.html"
    app <- application modelState
    setLocaleEncoding utf8
    run port $ simpleCors app
