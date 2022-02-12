{- |
Module      : NITTA.FrontEnds.FrontendIdentifier
Description : Chooses a frontend based on source file extension or format
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.FrontEnds.FrontendIdentifier (
    FrontendType (..),
    identifyFrontendType,
    getFrontendResult,
) where

import NITTA.FrontEnds.LuaFrontend
import NITTA.FrontEnds.XMILE.XMILEFrontend
import System.FilePath

data FrontendType = Lua | XMILE

identifyFrontendType _ "lua" = Lua
identifyFrontendType _ "xmile" = XMILE
identifyFrontendType fileName "dynamic" =
    case snd $ splitExtension $ show fileName of
        ".lua" -> Lua
        ".xmile" -> XMILE
        _ -> error $ "can't parse extension for file " <> fileName
identifyFrontendType fileName format =
    error $
        "unable to select proper source file parser for file extension '"
            <> fileName
            <> "' and format type '"
            <> format
            <> "'."

getFrontendResult src Lua = lua2functions src
getFrontendResult src XMILE = xmile2functions src
