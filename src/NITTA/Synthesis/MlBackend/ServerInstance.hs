{- |
Module      : NITTA.Synthesis.MlBackend.ServerInstance
Description : ML backend server instance management
Copyright   : (c) Ilya Burakov, 2023
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.MlBackend.ServerInstance (
    MlBackendServer (..),
    withLazyMlBackendServer,
) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text qualified as T
import GHC.IO.Handle
import NITTA.Synthesis.MlBackend.FixedCache
import NITTA.Utils.Base
import System.Directory
import System.FilePath
import System.IO
import System.Log.Logger
import System.Process

data MlBackendServer = MlBackendServer
    { baseUrl :: Maybe T.Text
    , handles :: Maybe (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
    }

instance Default MlBackendServer where
    def = MlBackendServer Nothing Nothing

-- | Reads ML backend server base URL from file (non-blocking).
readMlBackendBaseUrlFileIO :: IO (Maybe T.Text)
readMlBackendBaseUrlFileIO =
    do
        let baseUrlFilePath = ".ml_backend_base_url"

        debugM "NITTA.Synthesis" "reading ML backend base URL from file"
        withFile
            baseUrlFilePath
            ReadMode
            ( \file -> do
                baseUrl <- hGetLine file
                return $ Just $ T.pack baseUrl
            )
        `catch` \(_ :: IOException) -> do
            debugM "NITTA.Synthesis" "failed to read ML backend base URL from file"
            return Nothing

-- | Synchonously waits until ML backend server base URL file becomes available (with timeout)
waitForMlBackendBaseUrlIO retriesLeft = do
    if retriesLeft <= 0
        then return Nothing
        else do
            let secondsPerRetry = 3 :: Int

            debugM
                "NITTA.Synthesis"
                ( "waiting "
                    <> show (secondsPerRetry * retriesLeft)
                    <> " second(s) more until ML backend server base URL is available..."
                )

            threadDelay $ secondsPerRetry * 1000000

            readMlBackendBaseUrlFileIO >>= \case
                Just baseUrl -> return $ Just baseUrl
                Nothing -> waitForMlBackendBaseUrlIO (retriesLeft - 1)

-- | Tries to start ML backend server and gathers all required information about it.
tryStartMlBackendServerIO = do
    cwd <- getCurrentDirectory
    let executablePath = joinPath [cwd, "nitta-mlbackend"]
    infoM "NITTA.Synthesis" $ "Starting ML backend server, command: " <> executablePath

    maybeHandles <- catchToMaybeIO $ createProcess (proc executablePath [])
    maybeDynamicBaseUrl <- case maybeHandles of
        Nothing -> return Nothing
        Just _ -> waitForMlBackendBaseUrlIO 10

    when (isNothing maybeDynamicBaseUrl) $ do
        warningM "NITTA.Synthesis" "Failed to start ML backend server"

    return MlBackendServer{baseUrl = maybeDynamicBaseUrl, handles = maybeHandles}

-- | Makes ML backend server available with lazy initialization, memoization and proper cleanup.
withLazyMlBackendServer action =
    bracket
        -- resource initialization action, produces lazy server getter with enabled memoization
        ( let startupAction = do
                maybeExistingBaseUrl <- readMlBackendBaseUrlFileIO
                case maybeExistingBaseUrl of
                    -- if ML backend server base URL file already exists, then we assume that server is already running
                    Just existingBaseUrl -> do
                        debugM
                            "NITTA.Synthesis"
                            ( "ML backend server base URL was found ("
                                <> T.unpack existingBaseUrl
                                <> "), skipping server startup"
                            )
                        return MlBackendServer{baseUrl = Just existingBaseUrl, handles = Nothing}
                    -- coulnd't find existing base URL, trying to start the server
                    Nothing -> tryStartMlBackendServerIO
           in do
                -- cache is used to memoize server startup results and produce lazy getter
                cache <- newCache
                let serverGetter = fetch cache startupAction
                return (serverGetter, cache)
        )
        -- resource cleanup action
        ( \(_, Cache mServerVar) -> do
            tryReadMVar mServerVar >>= \case
                -- server was started
                Just (Just MlBackendServer{handles = Just serverProcessHandles@(_, _, _, procHandle)}) -> do
                    infoM "NITTA.Synthesis" "Stopping automatically started ML backend server"
                    cleanupProcess serverProcessHandles
                    _ <- waitForProcess procHandle
                    return ()
                -- server was not started
                _ -> debugM "NITTA.Synthesis" "ML backend server was not started automatically, so nothing to stop"
        )
        -- resource usage action (not exposing Cache object to it)
        (\(serverGetter, _) -> action serverGetter)