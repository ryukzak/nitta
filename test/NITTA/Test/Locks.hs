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

case_twoExpressions = do
    let
        b = locksSet "+a + b = c = e; +d + f = k "
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

case_threeExpressions = do
    let
        b = locksSet "+a + b = c = e; +d + f = k; +g - h -o + l = m = n = p"
        a = fromList
            [ Lock {locked = "c", lockBy = "a"}
            , Lock {locked = "c", lockBy = "b"}
            , Lock {locked = "d", lockBy = "a"}
            , Lock {locked = "d", lockBy = "b"}
            , Lock {locked = "e", lockBy = "a"}
            , Lock {locked = "e", lockBy = "b"}
            , Lock {locked = "f", lockBy = "a"}
            , Lock {locked = "f", lockBy = "b"}
            , Lock {locked = "g", lockBy = "a"}
            , Lock {locked = "g", lockBy = "b"}
            , Lock {locked = "g", lockBy = "d"}
            , Lock {locked = "g", lockBy = "f"}
            , Lock {locked = "h", lockBy = "a"}
            , Lock {locked = "h", lockBy = "b"}
            , Lock {locked = "h", lockBy = "d"}
            , Lock {locked = "h", lockBy = "f"}
            , Lock {locked = "k", lockBy = "a"}
            , Lock {locked = "k", lockBy = "b"}
            , Lock {locked = "k", lockBy = "c"}
            , Lock {locked = "k", lockBy = "d"}
            , Lock {locked = "k", lockBy = "e"}
            , Lock {locked = "k", lockBy = "f"}
            , Lock {locked = "l", lockBy = "a"}
            , Lock {locked = "l", lockBy = "b"}
            , Lock {locked = "l", lockBy = "d"}
            , Lock {locked = "l", lockBy = "f"}
            , Lock {locked = "m", lockBy = "a"}
            , Lock {locked = "m", lockBy = "b"}
            , Lock {locked = "m", lockBy = "c"}
            , Lock {locked = "m", lockBy = "d"}
            , Lock {locked = "m", lockBy = "e"}
            , Lock {locked = "m", lockBy = "f"}
            , Lock {locked = "m", lockBy = "g"}
            , Lock {locked = "m", lockBy = "h"}
            , Lock {locked = "m", lockBy = "k"}
            , Lock {locked = "m", lockBy = "l"}
            , Lock {locked = "m", lockBy = "o"}
            , Lock {locked = "n", lockBy = "a"}
            , Lock {locked = "n", lockBy = "b"}
            , Lock {locked = "n", lockBy = "c"}
            , Lock {locked = "n", lockBy = "d"}
            , Lock {locked = "n", lockBy = "e"}
            , Lock {locked = "n", lockBy = "f"}
            , Lock {locked = "n", lockBy = "g"}
            , Lock {locked = "n", lockBy = "h"}
            , Lock {locked = "n", lockBy = "k"}
            , Lock {locked = "n", lockBy = "l"}
            , Lock {locked = "n", lockBy = "o"}
            , Lock {locked = "o", lockBy = "a"}
            , Lock {locked = "o", lockBy = "b"}
            , Lock {locked = "o", lockBy = "d"}
            , Lock {locked = "o", lockBy = "f"}
            , Lock {locked = "p", lockBy = "a"}
            , Lock {locked = "p", lockBy = "b"}
            , Lock {locked = "p", lockBy = "c"}
            , Lock {locked = "p", lockBy = "d"}
            , Lock {locked = "p", lockBy = "e"}
            , Lock {locked = "p", lockBy = "f"}
            , Lock {locked = "p", lockBy = "g"}
            , Lock {locked = "p", lockBy = "h"}
            , Lock {locked = "p", lockBy = "k"}
            , Lock {locked = "p", lockBy = "l"}
            , Lock {locked = "p", lockBy = "o"}
            ]
    b @?= a

case_basicTestAdd = do
    let
        b = locksSet "+a + b = c;"
        a = fromList
            [ Lock {locked = "c", lockBy = "a"}
            , Lock {locked = "c", lockBy = "b"}
            ]
    b @?= a

case_basicTestAdd2 = do
    let
        b = locksSet "+a + b = c = e;"
        a = fromList
            [ Lock {locked = "c", lockBy = "a"}
            , Lock {locked = "c", lockBy = "b"}
            , Lock {locked = "e", lockBy = "a"}
            , Lock {locked = "e", lockBy = "b"}
            ]
    b @?= a

case_basicTestSub = do
    let
        b = locksSet "+a - b = c;"
        a = fromList
            [ Lock {locked = "c", lockBy = "a"}
            , Lock {locked = "c", lockBy = "b"}
            ]
    b @?= a



locksTest :: TestTree
locksTest = $(testGroupGenerator)
