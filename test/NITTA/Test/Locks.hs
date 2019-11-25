{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module NITTA.Test.Locks
    ( locksTest
    ) where

import NITTA.Intermediate.Functions
import           Data.Set                 (elems, fromList, union)
import qualified Data.List                as L
import           Text.Regex
import           NITTA.Intermediate.Types
import           Test.Tasty                    (TestTree)
import           Test.Tasty.HUnit
import           Test.Tasty.TH
import           Text.InterpolatedString.Perl6 (qc)
import           Data.List.Split          (splitWhen)

exprPattern = mkRegex "[+,=,-]*[a-zA-Z0-9]+|;"

toBlocksSplit exprInput = splitBySemicolon $ matchAll exprPattern filtered []
    where
        matchAll p inpS res =
            case matchRegexAll p inpS of
                Just (_, x, xs, _) -> x : matchAll p xs res
                Nothing            -> []
        filtered = subRegex (mkRegex "[ ]+") exprInput ""
        splitBySemicolon = filter (/= []) . splitWhen ( == ";")

accGen blocks = structure
    where
        partedExpr = map (L.partition (\(x:_) -> x /= '='))
        signPush (s:name) = case s of
            '+' -> Push Plus (I name)
            '-' -> Push Minus (I name)
            _   -> error "Error in matching + and -"
        pushCreate lst = map signPush lst
        pullCreate lst = Pull $ O $ fromList $ foldl (\buff (_:name) -> name : buff ) [] lst
        structure = Acc $ concatMap (\(push, pull) -> pushCreate push ++ [pullCreate pull]) $ partedExpr blocks

locksSet exprInput = fromList $ locks $ accGen $ toBlocksSplit exprInput

case_basicTest = do
    let
        b = locksSet "+a +b = c = e; +d + f = k "
        a = fromList
            [ Lock {locked = "c", lockBy = "a"}
            , Lock {locked = "c", lockBy = "b"}
            , Lock {locked = "d", lockBy = "a"}
            , Lock {locked = "d", lockBy = "b"}
            , Lock {locked = "e", lockBy = "a"}
            , Lock {locked = "e", lockBy = "b"}
            , Lock {locked = "f", lockBy = "a"}
            , Lock {locked = "f", lockBy = "b"}
            , Lock {locked = "k", lockBy = "a"}
            , Lock {locked = "k", lockBy = "b"}
            , Lock {locked = "k", lockBy = "c"}
            , Lock {locked = "k", lockBy = "d"}
            , Lock {locked = "k", lockBy = "e"}
            , Lock {locked = "k", lockBy = "f"}
            ]
    b @?= a


locksTest :: TestTree
locksTest = $(testGroupGenerator)
