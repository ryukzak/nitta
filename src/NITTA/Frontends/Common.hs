{- |
Module      : NITTA.Frontends.Common
Description : Common types and functions for all frontend implementations
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Frontends.Common (
    FrontendResult (..),
    TraceVar (..),
    defaultFmt,
    prettyLog,
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String.ToString
import qualified Data.Text as T
import NITTA.Intermediate.DataFlow
import Text.Printf

data FrontendResult v x = FrontendResult
    { frDataFlow :: DataFlowGraph v x
    , frTrace :: [TraceVar]
    , frPrettyLog :: [HM.HashMap v x] -> [HM.HashMap String String]
    }

data TraceVar = TraceVar {tvFmt, tvVar :: T.Text}
    deriving (Show)

defaultFmt = T.pack "%.3f"

prettyLog traceVars hms = map prettyHM hms
    where
        prettyHM hm = HM.fromList $ map (fromMaybe undefined) $ filter isJust $ map prettyX $ HM.toList hm
        prettyX (v0, x) = do
            -- variables names end on #0, #1..., so we trim this suffix
            let v = takeWhile (/= '#') $ toString v0
            fmt <- v2fmt M.!? v
            Just (toString (takeWhile (/= '^') v), printx (T.unpack fmt) x)
        v2fmt = M.fromList $ map (\(TraceVar fmt v) -> (toString v, fmt)) traceVars
        printx p x
            | 'f' `elem` p = printf p (fromRational (toRational x) :: Double)
            | 's' `elem` p = printf p $ show x
            | otherwise = printf p x
