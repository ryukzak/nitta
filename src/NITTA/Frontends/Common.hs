{- |
Module      : NITTA.Frontends.Common
Description : Common types and functions for all frontend implementations
Copyright   : (c) Artur Gogiyan, 2022
License     : BSD3
Maintainer  : artur.gogiyan@gmail.com
Stability   : experimental
-}
module NITTA.Frontends.Common (
    FrontendResult (..),
    TraceVar (..),
    prettyLog,
    getTraceVarFormat,
) where

import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.String.ToString
import Data.Text qualified as T
import NITTA.Intermediate.DataFlow
import Text.Printf

data FrontendResult v x = FrontendResult
    { frDataFlow :: DataFlowGraph v x
    , frTrace :: [TraceVar]
    , frPrettyLog :: [HM.HashMap v x] -> [HM.HashMap String String]
    }

data TraceVar = TraceVar {tvFmt :: Maybe T.Text, tvVar :: T.Text}
    deriving (Show)

defaultFmt = T.pack "%.3f"

prettyLog traceVars hms = map prettyHM hms
    where
        prettyHM hm = HM.fromList $ mapMaybe prettyX $ HM.toList hm
        prettyX (v0, x) = do
            -- variables names end on #0, #1..., so we trim this suffix
            let v = takeWhile (/= '#') $ toString v0
            fmt <- v2fmt M.!? v
            Just (toString (takeWhile (/= '^') v), printx (T.unpack (getTraceVarFormat fmt)) x)
        v2fmt = M.fromList $ map (\(TraceVar fmt v) -> (toString v, fmt)) traceVars
        printx p x
            | 'f' `elem` p = printf p (fromRational (toRational x) :: Double)
            | 's' `elem` p = printf p $ show x
            | otherwise = printf p x

getTraceVarFormat Nothing = defaultFmt
getTraceVarFormat (Just fmt) = fmt
