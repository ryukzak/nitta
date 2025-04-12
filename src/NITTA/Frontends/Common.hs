{-# LANGUAGE OverloadedStrings #-}

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
    Translatable (..),
    prettyLog,
    getTraceVarFormat,
) where

import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.String.ToString
import Data.Text qualified as T
import NITTA.Intermediate.DataFlow
import NITTA.Intermediate.Functions qualified as F
import NITTA.Intermediate.Types
import NITTA.Utils.Base
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

class Val x => Translatable x where
    stat2function :: Var v => T.Text -> [T.Text] -> [[v]] -> [x] -> [Int] -> F v x

instance (FixedPointCompatible x, Val x) => Translatable x where
    stat2function "buffer" [i] [o] [] [] = F.buffer (fromText i) o
    stat2function "brokenBuffer" [i] [o] [] [] = F.brokenBuffer (fromText i) o
    stat2function "constant" [] [o] [x] [] = F.constant x o
    stat2function "send" [i] [] [] [] = F.send (fromText i)
    stat2function "add" [a, b] [c] [] [] = F.add (fromText a) (fromText b) c
    stat2function "sub" [a, b] [c] [] [] = F.sub (fromText a) (fromText b) c
    stat2function "multiply" [a, b] [c] [] [] = F.multiply (fromText a) (fromText b) c
    stat2function "divide" [d, n] [q] [] [] = F.division (fromText d) (fromText n) q []
    stat2function "divide" [d, n] [q, r] [] [] = F.division (fromText d) (fromText n) q r
    stat2function "neg" [i] [o] [] [] = F.neg (fromText i) o
    stat2function "receive" [] [o] [] [] = F.receive o
    stat2function "shiftL" [a] [c] [] [s] = F.shiftL s (fromText a) c
    stat2function "shiftR" [a] [c] [] [s] = F.shiftR s (fromText a) c
    stat2function "loop" [a] [c] [x] [] = F.loop x (fromText a) c
    stat2function f _ _ _ _ = error $ "function not found: " <> show f

instance Translatable Float where
    -- FIXME: add other functions
    stat2function "divide" [d, n] [q] [] [] = F.floatDivision (fromText d) (fromText n) q
    stat2function f _ _ _ _ = error $ "function not found: " <> show f
