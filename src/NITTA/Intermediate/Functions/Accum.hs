{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}

{- |
Module      : NITTA.Intermediate.Functions
Description : Accum function
Copyright   : (c) Daniil Prohorov, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Intermediate.Functions.Accum (
    Acc (..),
    Action (..),
    Sign (..),

    -- * Acc to function
    acc,
    accFromStr,

    -- * Utils functions
    isPull,
    isPush,
) where

import Data.List (nub, partition)
import Data.List.Split (splitWhen)
import Data.Set qualified as S
import Data.String.ToString
import Data.String.Utils qualified as S
import Data.Text qualified as T
import Data.Typeable
import NITTA.Intermediate.Types
import NITTA.Utils.Base
import Text.Regex

data Sign = Plus | Minus deriving (Typeable, Eq)

instance Show Sign where
    show Plus = "+"
    show Minus = "-"

data Action v = Push Sign (I v) | Pull (O v) deriving (Typeable, Eq)

instance (Var v) => Show (Action v) where
    show (Push s (I v)) = show s <> toString v
    show (Pull (O vs)) = S.join " " $ map ("= " <>) $ vsToStringList vs

instance Variables (Action v) v where
    variables (Push _s i) = variables i
    variables (Pull o) = variables o

newtype Acc v x = Acc {actions :: [Action v]} deriving (Typeable, Eq)

instance (Var v) => Show (Acc v x) where
    show (Acc acts) =
        let lastElement = last acts
            initElements = init acts
            showElement inp@(Push _ _) = show inp
            showElement out@(Pull _) = show out <> ","
            elements = S.join " " $ map showElement initElements <> [show lastElement]
         in "Acc(" <> elements <> ")"

instance Label (Acc v x) where label Acc{} = "Acc"

-- | Create function with type F of Acc
acc lst = packF $ Acc lst

-- | Special function for generating Acc from string, examples in tests
accFromStr desc = packF $ accGen $ toBlocksSplit desc

isPull Pull{} = True
isPull _ = False

isPush Push{} = True
isPush _ = False

fromPush (Push _ (I v)) = v
fromPush _ = error "Error in fromPush function in acc"

fromPull (Pull (O vs)) = vs
fromPull _ = error "Error in fromPull function in acc"

instance (Ord v) => Function (Acc v x) v where
    inputs (Acc lst) = S.fromList $ map fromPush $ filter isPush lst
    outputs (Acc lst) = unionsMap fromPull $ filter isPull lst

instance (Ord v) => Patch (Acc v x) (v, v) where
    patch diff (Acc lst) =
        Acc $
            nub $
                map
                    ( \case
                        Push s v -> Push s (patch diff v)
                        Pull vs -> Pull (patch diff vs)
                    )
                    lst

exprPattern = mkRegex "[+,=,-]*[a-zA-Z0-9]+|;"
toBlocksSplit exprInput =
    let splitBySemicolon = filter (not . null) . splitWhen (== ";")
        matchAll p inpS res =
            case matchRegexAll p inpS of
                Just (_, x, xs, _) -> x : matchAll p xs res
                Nothing -> []
        filtered = subRegex (mkRegex "[ ]+") exprInput ""
     in splitBySemicolon $ matchAll exprPattern filtered []

accGen blocks =
    let partedExpr = map (partition (\(x : _) -> x /= '='))
        signPush ('+' : name) = Push Plus (I $ T.pack name)
        signPush ('-' : name) = Push Minus (I $ T.pack name)
        signPush _ = error "Error in matching + and -"
        pushCreate lst = map signPush lst
        pullCreate lst = Pull $ O $ S.fromList $ foldl (\buff (_ : name) -> T.pack name : buff) [] lst
     in Acc $ concatMap (\(push, pull) -> pushCreate push ++ [pullCreate pull]) $ partedExpr blocks

instance (Var v) => Locks (Acc v x) v where
    locks (Acc actions) =
        let (lockByActions, lockedActions) = span isPush actions
         in [ Lock{locked, lockBy}
            | locked <- S.elems $ unionsMap variables lockedActions
            , lockBy <- S.elems $ unionsMap variables lockByActions
            ]

instance (Var v, Num x) => FunctionSimulation (Acc v x) v x where
    simulate cntx (Acc ops) = snd $ foldl eval (0, []) ops
        where
            eval (buf, changes) (Push sign (I v))
                | x <- getCntx cntx v =
                    case sign of
                        Plus -> (buf + x, changes)
                        Minus -> (buf - x, changes)
            eval (buf, changes) (Pull (O vs)) = (buf, [(v, buf) | v <- S.elems vs] ++ changes)
