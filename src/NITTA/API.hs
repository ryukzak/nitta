{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}

{-|
Module      : NITTA.API
Description : HTTP backend for the NITTA web UI.
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.API
    ( backendServer
    ) where

import           Control.Concurrent.STM
import           Control.Monad                 (unless, when)
import           Data.Monoid                   ((<>))
import           GHC.IO.Encoding               (setLocaleEncoding, utf8)
import           Network.Wai.Handler.Warp      (run)
import           Network.Wai.Middleware.Cors   (simpleCors)
import           NITTA.API.Marshalling         ()
import           NITTA.API.REST
import           NITTA.Types.Synthesis
import           Servant
import qualified Servant.JS                    as SJS
import           Servant.Utils.StaticFiles     (serveDirectoryWebApp)
import           System.Directory              (createDirectoryIfMissing)
import           System.Exit                   (ExitCode (..), die)
import           System.FilePath.Posix         (joinPath)
import           System.Process
import           Text.InterpolatedString.Perl6 (qq)
-- import           Servant.Server.StaticFiles (serveDirectoryWebApp) -- update on servant 14+


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
    SJS.writeJSForAPI (Proxy :: Proxy (SynthesisAPI String String Int Int)) ((prefix <>) . axios') $ joinPath ["web", "src", "gen", "nitta-api.js"]
    putStrLn "Generate rest_api.js library...OK"


prepareStaticFiles = do
    -- TODO: Rebuild at each run.
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



application compilerState = do
    root <- atomically $ mkNode mempty compilerState
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
backendServer no_api_gen no_static_gen modelState = do
    let port = 8080
    unless no_api_gen $ prepareJSAPI port
    unless no_static_gen prepareStaticFiles

    putStrLn $ "Running NITTA server on port: " ++ show port

    -- let initialCompilerState = def{ state=modelState }
    app <- application modelState
    setLocaleEncoding utf8
    -- T.writeFile "web/api.txt" $ layout (Proxy :: Proxy SynthesisAPI)

    run port $ simpleCors app
