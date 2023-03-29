{-# LANGUAGE DeriveDataTypeable #-}

{- |
Module      : NITTA.Synthesis.MlBackend.FixedCache
Description : Fixed version of thread-safe write-once cache from io-memoize package
Copyright   : (c) Dan Burton <danburton.email@gmail.com>, 2014
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

Taken as is from Control.Concurrent.Cache of io-memoize package,
but with expoted Cache constructor, so we can use it in pattern matching.

This was necessary not to rewrite similar functionality. Linter errors were
fixed as well.
-}
module NITTA.Synthesis.MlBackend.FixedCache (Cache (..), newCache, fetch) where

import Control.Concurrent.MVar
import Data.Typeable (Typeable)

{- | A thread-safe write-once cache. If you need more functionality,
(e.g. multiple write, cache clearing) use an 'MVar' instead.
-}
newtype Cache a = Cache (MVar (Maybe a))
    deriving (Eq, Typeable)

{- | Fetch the value stored in the cache,
or call the supplied fallback and store the result,
if the cache is empty.
-}
fetch :: Cache a -> IO a -> IO a
fetch (Cache var) action = go
    where
        go =
            readMVar var >>= \case
                Just a -> return a
                Nothing -> do
                    modifyMVar_ var $ \case
                        Just a -> return (Just a)
                        Nothing -> fmap Just action
                    go

-- | Create an empty cache.
newCache :: IO (Cache a)
newCache = do
    var <- newMVar Nothing
    return (Cache var)
