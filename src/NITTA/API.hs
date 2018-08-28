{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

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

import           NITTA.API.REST

import           Control.Concurrent.STM
import           Control.Monad                 (when)
import           Data.Default
import           Data.Monoid                   ((<>))
import NITTA.Types.Synthesis
import qualified Data.Text.IO                  as T
import           GHC.IO.Encoding               (setLocaleEncoding, utf8)
import           Network.Wai.Handler.Warp      (run)
import           Network.Wai.Middleware.Cors   (simpleCors)
import           NITTA.Compiler
import           NITTA.Utils.JSON              ()
import           Servant
import qualified Servant.JS                    as SJS
import           Servant.Utils.StaticFiles     (serveDirectoryWebApp)
import qualified STMContainers.Map             as M
import           System.Directory              (createDirectoryIfMissing)
import           System.Exit                   (ExitCode (..), die)
import           System.FilePath.Posix         (joinPath)
import           System.Process
import           Text.InterpolatedString.Perl6 (qq)
-- import           Servant.Server.StaticFiles (serveDirectoryWebApp) -- update on servant 14+


prepareServer port = do
    -- Generate JS API
    let prefix = [qq|import axios from 'axios';
var api = \{\};
export default api;|]
    let axios' = SJS.axiosWith SJS.defAxiosOptions SJS.defCommonGeneratorOptions
            { SJS.urlPrefix=[qq|http://localhost:$port|]
            , SJS.moduleName="api"
            }
    createDirectoryIfMissing True $ joinPath ["web", "src", "gen"]
    SJS.writeJSForAPI (Proxy :: Proxy SynthesisAPI) ((prefix <>) . axios') $ joinPath ["web", "src", "gen", "nitta-api.js"]

    -- Generate web app by npm.
    -- TODO: Rebuild at each run.
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


application compilerState = do
    st <- newTVarIO $ rootSynthesis compilerState
    return $ serve
        ( Proxy :: Proxy
            (    SynthesisAPI
            :<|> Raw
            ) )
        (    synthesisServer st
        :<|> serveDirectoryWebApp (joinPath ["web", "build"])
        )


-- |Run backend server. Parameters:
--
-- - if true - prepare static files for the web UI by @npm@;
-- - initial model state.
backendServer prepare modelState = do
    let port = 8080
    when prepare $ prepareServer port
    putStrLn $ "Running NITTA server on port: " ++ show port

    -- let initialCompilerState = def{ state=modelState }
    app <- application modelState
    setLocaleEncoding utf8
    -- T.writeFile "web/api.txt" $ layout (Proxy :: Proxy SynthesisAPI)

    run port $ simpleCors app
