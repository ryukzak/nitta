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

import           Data.Set                     (fromList)
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           Test.Tasty                   (TestTree)
import           Test.Tasty.HUnit
import           Test.Tasty.TH


locksSet accExp = fromList $ locks $ (accFromStr accExp :: F String Int)


case_basicTestAdd = do
    let
        b = locksSet "+a + b = c;"
        a = fromList
            [ Lock { locked = "c", lockBy = "a" }
            , Lock { locked = "c", lockBy = "b" }
            ]
    b @?= a

case_basicTestAdd2 = do
    let
        b = locksSet "+a + b = c = e;"
        a = fromList
            [ Lock { locked = "c", lockBy = "a" }
            , Lock { locked = "c", lockBy = "b" }
            , Lock { locked = "e", lockBy = "a" }
            , Lock { locked = "e", lockBy = "b" }
            ]
    b @?= a

case_basicTestSub = do
    let
        b = locksSet "+a - b = c;"
        a = fromList
            [ Lock { locked = "c", lockBy = "a" }
            , Lock { locked = "c", lockBy = "b" }
            ]
    b @?= a

case_twoExpressions = do
    let
        b = locksSet "+a + b = c = e; +d + f = k "
        a = fromList
            [ Lock { locked = "c", lockBy = "a" }
            , Lock { locked = "c", lockBy = "b" }
            , Lock { locked = "d", lockBy = "a" }
            , Lock { locked = "d", lockBy = "b" }
            , Lock { locked = "e", lockBy = "a" }
            , Lock { locked = "e", lockBy = "b" }
            , Lock { locked = "f", lockBy = "a" }
            , Lock { locked = "f", lockBy = "b" }
            , Lock { locked = "k", lockBy = "a" }
            , Lock { locked = "k", lockBy = "b" }
            , Lock { locked = "k", lockBy = "c" }
            , Lock { locked = "k", lockBy = "d" }
            , Lock { locked = "k", lockBy = "e" }
            , Lock { locked = "k", lockBy = "f" }
            ]
    b @?= a


locksTest :: TestTree
locksTest = $(testGroupGenerator)
