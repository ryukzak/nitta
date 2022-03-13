{- |
Module      : NITTA.Frontends.FrontendIdentifier
Description : Chooses a frontend based on source file extension or format
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Frontends.FrontendIdentifier (
    FrontendType (..),
    identifyFrontendType,
    translateFrontendResult,
) where

import NITTA.Frontends.Lua
import NITTA.Frontends.XMILE.Frontend
import System.FilePath

data FrontendType = Lua | XMILE
    deriving (Show)

identifyFrontendType _ "lua" = Lua
identifyFrontendType _ "xmile" = XMILE
identifyFrontendType fileName "dynamic" =
    case snd $ splitExtension fileName of
        ".lua" -> Lua
        ".xmile" -> XMILE
        ext -> error $ "can't parse extension for file " <> fileName <> "|" <> ext
identifyFrontendType fileName format =
    error $
        "unable to select proper source file parser for file extension '"
            <> fileName
            <> "' and format type '"
            <> format
            <> "'."

translateFrontendResult Lua = lua2functions
translateFrontendResult XMILE = xmile2functions
