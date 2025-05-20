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

stat2functionFX :: (Var v, Val x, FixedPointCompatible x) => T.Text -> [T.Text] -> [[v]] -> [x] -> [Int] -> F v x
stat2functionFX "buffer" [i] [o] [] [] = F.buffer (fromText i) o
stat2functionFX "brokenBuffer" [i] [o] [] [] = F.brokenBuffer (fromText i) o
stat2functionFX "constant" [] [o] [x] [] = F.constant x o
stat2functionFX "send" [i] [] [] [] = F.send (fromText i)
stat2functionFX "add" [a, b] [c] [] [] = F.add (fromText a) (fromText b) c
stat2functionFX "sub" [a, b] [c] [] [] = F.sub (fromText a) (fromText b) c
stat2functionFX "multiply" [a, b] [c] [] [] = F.multiply (fromText a) (fromText b) c
stat2functionFX "divide" [d, n] [q] [] [] = F.division (fromText d) (fromText n) q []
stat2functionFX "divide" [d, n] [q, r] [] [] = F.division (fromText d) (fromText n) q r
stat2functionFX "neg" [i] [o] [] [] = F.neg (fromText i) o
stat2functionFX "receive" [] [o] [] [] = F.receive o
stat2functionFX "shiftL" [a] [c] [] [s] = F.shiftL s (fromText a) c
stat2functionFX "shiftR" [a] [c] [] [s] = F.shiftR s (fromText a) c
stat2functionFX "loop" [a] [c] [x] [] = F.loop x (fromText a) c
stat2functionFX "lessThan" [a, b] [c] [] [] = F.cmp F.CmpLt (fromText a) (fromText b) c
stat2functionFX "lessThanOrEqual" [a, b] [c] [] [] = F.cmp F.CmpLte (fromText a) (fromText b) c
stat2functionFX "equal" [a, b] [c] [] [] = F.cmp F.CmpEq (fromText a) (fromText b) c
stat2functionFX "greaterThanOrEqual" [a, b] [c] [] [] = F.cmp F.CmpGte (fromText a) (fromText b) c
stat2functionFX "greaterThan" [a, b] [c] [] [] = F.cmp F.CmpGt (fromText a) (fromText b) c
stat2functionFX "and" [a, b] [c] [] [] = F.logicAnd (fromText a) (fromText b) c
stat2functionFX "or" [a, b] [c] [] [] = F.logicOr (fromText a) (fromText b) c
stat2functionFX "not" [a] [c] [] [] = F.logicNot (fromText a) c
stat2functionFX f _ _ _ _ = error $ "function not found: " <> show f

stat2functionFloat :: (Var v, Val x, Fractional x) => T.Text -> [T.Text] -> [[v]] -> [x] -> [Int] -> F v x
stat2functionFloat "divide" [d, n] [q] [] [] = F.floatDivision (fromText d) (fromText n) q []
stat2functionFloat "divide" [d, n] [q, r] [] [] = F.floatDivision (fromText d) (fromText n) q r
stat2functionFloat "buffer" [i] [o] [] [] = F.buffer (fromText i) o
stat2functionFloat "brokenBuffer" [i] [o] [] [] = F.brokenBuffer (fromText i) o
stat2functionFloat "constant" [] [o] [x] [] = F.constant x o
stat2functionFloat "send" [i] [] [] [] = F.send (fromText i)
stat2functionFloat "add" [a, b] [c] [] [] = F.add (fromText a) (fromText b) c
stat2functionFloat "sub" [a, b] [c] [] [] = F.sub (fromText a) (fromText b) c
stat2functionFloat "multiply" [a, b] [c] [] [] = F.multiply (fromText a) (fromText b) c
stat2functionFloat "neg" [i] [o] [] [] = F.neg (fromText i) o
stat2functionFloat "receive" [] [o] [] [] = F.receive o
stat2functionFloat "loop" [a] [c] [x] [] = F.loop x (fromText a) c
stat2functionFloat "lessThan" [a, b] [c] [] [] = F.cmp F.CmpLt (fromText a) (fromText b) c
stat2functionFloat "lessThanOrEqual" [a, b] [c] [] [] = F.cmp F.CmpLte (fromText a) (fromText b) c
stat2functionFloat "equal" [a, b] [c] [] [] = F.cmp F.CmpEq (fromText a) (fromText b) c
stat2functionFloat "greaterThanOrEqual" [a, b] [c] [] [] = F.cmp F.CmpGte (fromText a) (fromText b) c
stat2functionFloat "greaterThan" [a, b] [c] [] [] = F.cmp F.CmpGt (fromText a) (fromText b) c
stat2functionFloat "and" [a, b] [c] [] [] = F.logicAnd (fromText a) (fromText b) c
stat2functionFloat "or" [a, b] [c] [] [] = F.logicOr (fromText a) (fromText b) c
stat2functionFloat "not" [a] [c] [] [] = F.logicNot (fromText a) c
stat2functionFloat f _ _ _ _ = error $ "function not found: " <> show f

instance {-# OVERLAPPING #-} Translatable Float where stat2function = stat2functionFloat

instance {-# OVERLAPPING #-} Translatable (Attr Float) where stat2function = stat2functionFloat

instance {-# OVERLAPPABLE #-} (FixedPointCompatible x, Val x) => Translatable x where stat2function = stat2functionFX
