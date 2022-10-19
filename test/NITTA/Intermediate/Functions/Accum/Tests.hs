{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : NITTA.Intermediate.Functions.Accum.Tests
Description : Accum function
Copyright   : (c) Daniil Prohorov, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Intermediate.Functions.Accum.Tests (
    tests,
) where

import Data.Set (fromList)
import Data.Text qualified as T
import NITTA.Intermediate.Functions.Accum
import NITTA.Intermediate.Types
import NITTA.Utils.Tests (testCaseM)
import Test.Tasty (testGroup)
import Test.Tasty.HUnit

tests =
    testGroup
        "Accum function"
        [ testCaseM "locks for: +a + b = c" $
            let b = locksSet "+a + b = c;"
                a =
                    fromList
                        [ Lock{locked = "c", lockBy = "a"}
                        , Lock{locked = "c", lockBy = "b"}
                        ]
             in b @?= a
        , testCaseM "locks for: +a + b = c = e;" $
            let b = locksSet " +a + b = c = e;"
                a =
                    fromList
                        [ Lock{locked = "c", lockBy = "a"}
                        , Lock{locked = "c", lockBy = "b"}
                        , Lock{locked = "e", lockBy = "a"}
                        , Lock{locked = "e", lockBy = "b"}
                        ]
             in b @?= a
        , testCaseM "locks for: +a - b = c" $
            let b = locksSet "+a - b = c;"
                a =
                    fromList
                        [ Lock{locked = "c", lockBy = "a"}
                        , Lock{locked = "c", lockBy = "b"}
                        ]
             in b @?= a
        , testCaseM "locks for: +a + b = c = e; +d + f = k" $
            let b = locksSet "+a + b = c = e; +d + f = k "
                a =
                    fromList
                        [ Lock{locked = "c", lockBy = "a"}
                        , Lock{locked = "e", lockBy = "a"}
                        , Lock{locked = "d", lockBy = "a"}
                        , Lock{locked = "f", lockBy = "a"}
                        , Lock{locked = "k", lockBy = "a"}
                        , Lock{locked = "c", lockBy = "b"}
                        , Lock{locked = "e", lockBy = "b"}
                        , Lock{locked = "d", lockBy = "b"}
                        , Lock{locked = "f", lockBy = "b"}
                        , Lock{locked = "k", lockBy = "b"}
                        ]
             in b @?= a
        ]
    where
        locksSet accExp = fromList $ locks (accFromStr accExp :: F T.Text Int)
