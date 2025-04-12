{-# LANGUAGE ConstraintKinds #-}

{- |
Module      : NITTA.Frontends.FrontendIdentifier
Description : Chooses a frontend based on source file extension or format
Copyright   : (c) Artur Gogiyan, 2022
License     : BSD3
Maintainer  : artur.gogiyan@gmail.com
Stability   : experimental
-}
module NITTA.Frontends (
    FrontendType (..),
    FrontendResult (..),
    TraceVar (..),
    Translatable,
    prettyLog,
    getTraceVarFormat,
    identifyFrontendType,
    translate,
) where

import Data.Data
import Data.Maybe
import Data.Text qualified as T
import NITTA.Frontends.Common
import NITTA.Frontends.Lua
import NITTA.Frontends.XMILE.Frontend
import NITTA.Intermediate.Variable (Var)
import System.FilePath

data FrontendType = Lua | XMILE
    deriving (Show, Data)

identifyFrontendType fileName frontendType = fromMaybe identifyByExtension frontendType
    where
        identifyByExtension =
            case snd $ splitExtension fileName of
                ".lua" -> Lua
                ".xmile" -> XMILE
                ext -> error $ "unknown file extensions: " <> ext <> " for " <> fileName

translate :: (Var v, Translatable x) => FrontendType -> T.Text -> FrontendResult v x
translate Lua = translateLua
translate XMILE = translateXMILE
