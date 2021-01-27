{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

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

import Data.List (partition)
import Data.List.Split (splitWhen)
import Data.Set (elems, fromList)
import qualified Data.String.Utils as S
import Data.Typeable
import NITTA.Intermediate.Types
import NITTA.Utils.Base
import Text.Regex

data Sign = Plus | Minus deriving (Typeable, Eq)

instance Show Sign where
    show Plus = "+"
    show Minus = "-"

data Action v = Push Sign (I v) | Pull (O v) deriving (Typeable, Eq)

instance (Show v) => Show (Action v) where
    show (Push s (I v)) = show s <> show v
    show (Pull (O vs)) = S.join " " (map (("= " <>) . show) $ elems vs) <> ";"

newtype Acc v x = Acc {actions :: [Action v]} deriving (Typeable, Eq)

instance (Show v) => Show (Acc v x) where
    show (Acc acts) = S.join " " $ map show acts

instance Label (Acc v x) where label Acc{} = "Acc"

-- |Create function with type F of Acc
acc lst = packF $ Acc lst

-- |Special function for generating Acc from string, examples in tests
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
    inputs (Acc lst) = fromList $ map fromPush $ filter isPush lst
    outputs (Acc lst) = unionsMap fromPull $ filter isPull lst

instance (Ord v) => Patch (Acc v x) (v, v) where
    patch diff (Acc lst) =
        Acc $
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
        signPush ('+' : name) = Push Plus (I name)
        signPush ('-' : name) = Push Minus (I name)
        signPush _ = error "Error in matching + and -"
        pushCreate lst = map signPush lst
        pullCreate lst = Pull $ O $ fromList $ foldl (\buff (_ : name) -> name : buff) [] lst
     in Acc $ concatMap (\(push, pull) -> pushCreate push ++ [pullCreate pull]) $ partedExpr blocks

instance (Var v) => Locks (Acc v x) v where
    locks accList =
        let pushGroups (Acc lst) = map (map fromPush) $ filter (not . null) $ splitWhen isPull lst

            pullGroups (Acc lst) = map (concatMap (elems . fromPull)) $ filter (not . null) $ splitWhen isPush lst

            locksPush [] buff = filter (not . null . fst) buff
            locksPush (x : xs) [] = locksPush xs [([], x)]
            locksPush (x : xs) buff@((lastL, lastLB) : _) = locksPush xs ((x, lastL ++ lastLB) : buff)

            locksPull [] buff = buff
            locksPull (x : xs) [] = locksPull xs [x]
            locksPull ((inp, out) : xs) buff@((lastL, lastLB) : _) = locksPull xs ((inp, out ++ lastL ++ lastLB) : buff)

            pushList = pushGroups accList
            pullList = pullGroups accList
            exprTuple = zip pullList pushList
            locksListPush = locksPush pushList []
            locksListPull = locksPull exprTuple []
            allLocks = locksListPush ++ locksListPull
         in concatMap
                ( \eachLock ->
                    [ Lock{locked = y, lockBy = x}
                    | x <- snd eachLock
                    , y <- fst eachLock
                    ]
                )
                allLocks

instance (Var v, Num x) => FunctionSimulation (Acc v x) v x where
    simulate cntx (Acc ops) = snd $ foldl eval (0, []) ops
        where
            eval (buf, changes) (Push sign (I v))
                | x <- getCntx cntx v =
                    case sign of
                        Plus -> (buf + x, changes)
                        Minus -> (buf - x, changes)
            eval (buf, changes) (Pull (O vs)) = (buf, [(v, buf) | v <- elems vs] ++ changes)
