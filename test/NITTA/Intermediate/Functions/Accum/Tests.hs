{- FOURMOLU_DISABLE -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : NITTA.Intermediate.Functions.Accum.Tests
Description : Accum function
Copyright   : (c) Daniil Prohorov, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Intermediate.Functions.Accum.Tests
    ( tests
    ) where

import           Data.Set ( fromList )
import           NITTA.Intermediate.Functions.Accum
import           NITTA.Intermediate.Types
import           Test.Tasty ( testGroup )
import           Test.Tasty.HUnit


tests = testGroup "Accum function"
    [ testCase "locks for: +a + b = c" $ let
            b = locksSet "+a + b = c;"
            a = fromList
                [ Lock{ locked = "c", lockBy = "a" }
                , Lock{ locked = "c", lockBy = "b" }
                ]
        in b @?= a

    , testCase "locks for: +a + b = c = e;" $ let
            b = locksSet " +a + b = c = e;"
            a = fromList
                [ Lock{ locked = "c", lockBy = "a" }
                , Lock{ locked = "c", lockBy = "b" }
                , Lock{ locked = "e", lockBy = "a" }
                , Lock{ locked = "e", lockBy = "b" }
                ]
        in b @?= a

    , testCase "locks for: +a - b = c" $ let
            b = locksSet "+a - b = c;"
            a = fromList
                [ Lock{ locked = "c", lockBy = "a" }
                , Lock{ locked = "c", lockBy = "b" }
                ]
        in b @?= a

    , testCase "locks for: +a + b = c = e; +d + f = k" $ let
            b = locksSet "+a + b = c = e; +d + f = k "
            a = fromList
                [ Lock{ locked = "c", lockBy = "a" }
                , Lock{ locked = "c", lockBy = "b" }
                , Lock{ locked = "d", lockBy = "a" }
                , Lock{ locked = "d", lockBy = "b" }
                , Lock{ locked = "e", lockBy = "a" }
                , Lock{ locked = "e", lockBy = "b" }
                , Lock{ locked = "f", lockBy = "a" }
                , Lock{ locked = "f", lockBy = "b" }
                , Lock{ locked = "k", lockBy = "a" }
                , Lock{ locked = "k", lockBy = "b" }
                , Lock{ locked = "k", lockBy = "c" }
                , Lock{ locked = "k", lockBy = "d" }
                , Lock{ locked = "k", lockBy = "e" }
                , Lock{ locked = "k", lockBy = "f" }
                ]
        in b @?= a

    ]
    where
        locksSet accExp = fromList $ locks (accFromStr accExp :: F String Int)
