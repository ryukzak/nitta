{-# LANGUAGE DeriveDataTypeable #-}

{- |
Module      : NITTA.Frontends.FrontendIdentifier
Description : Chooses a frontend based on source file extension or format
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Frontends (
    FrontendType (..),
    identifyFrontendType,
    translateFrontendResult,
) where

import Data.Data
import Data.Maybe
import NITTA.Frontends.Lua
import NITTA.Frontends.XMILE.Frontend
import System.FilePath

data FrontendType = Lua | XMILE
    deriving (Show, Data)

identifyFrontendType fileName frontendType = fromMaybe identifyByExtension frontendType
    where
        identifyByExtension =
            case snd $ splitExtension fileName of
                ".lua" -> Lua
                ".xmile" -> XMILE
                ext -> error $ "can't parse extension for file " <> fileName <> "|" <> ext

translateFrontendResult Lua = translateLua
translateFrontendResult XMILE = translateXMILE
