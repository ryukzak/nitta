{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS -fno-warn-overlapping-patterns #-} -- for master/slave selection
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
import           NITTA.UIBackend.Timeline
import           Numeric.Interval

$(deriveTypeScript defaultOptions ''ViewPointID)
$(deriveTypeScript defaultOptions ''ProcessTimelines)
$(deriveTypeScript defaultOptions ''TimelinePoint)
$(deriveTypeScript defaultOptions ''Interval)

main = do
    prepareJSAPI (8080 :: Int)
    putStrLn $ formatTSDeclarations $ foldl1 (<>)
        [ getTypeScriptDeclarations (Proxy :: Proxy ViewPointID) 
        , getTypeScriptDeclarations (Proxy :: Proxy ProcessTimelines) 
        , getTypeScriptDeclarations (Proxy :: Proxy TimelinePoint) 
        , getTypeScriptDeclarations (Proxy :: Proxy Interval) 
        ]
