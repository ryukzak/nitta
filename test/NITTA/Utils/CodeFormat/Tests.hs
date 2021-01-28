{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : NITTA.Utils.CodeFormat.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Utils.CodeFormat.Tests (
    tests,
) where

import qualified Data.String.Utils as S
import NITTA.Utils.CodeFormat
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Tasty.TH
import Text.InterpolatedString.Perl6 (qc)

{-# ANN module "HLint: ignore Reduce duplication" #-}

case_justTextInCodeBlock = do
    let a =
            S.join
                "\n"
                [ "foo"
                , "    bar"
                , "    bar2"
                , ""
                ]
        b =
            codeBlock
                [qc|
            foo
                bar
                bar2
            |]
    b @?= a

case_codeBlockWithSubConst = do
    let a =
            S.join
                "\n"
                [ "foo"
                , "    bar"
                , "    bar2"
                , ""
                ]
        b =
            codeBlock
                [qc|
            foo
                bar
                {"bar2"}
            |]
    b @?= a

case_codeBlockWithSubCodeBlock = do
    let a =
            S.join
                "\n"
                [ "foo"
                , "    bar"
                , "    bar2"
                , ""
                , ""
                ]
        bar2 =
            codeBlock
                [qc|
            bar2
            |]
        b =
            codeBlock
                [qc|
            foo
                bar
                {bar2}
            |]

    b @?= a

case_codeBlockInOneLineW = do
    let a =
            S.join
                "\n"
                [ "foo"
                , "    bar"
                , "    foo bar2"
                , ""
                , ""
                ]
        bar2 =
            codeBlock
                [qc|
            bar2
            |]
        b =
            codeBlock
                [qc|
            foo
                bar
                foo {bar2}
            |]

    b @?= a

case_codeLineInOneLine = do
    let a =
            S.join
                "\n"
                [ "foo"
                , "    bar"
                , "    foo bar2"
                , ""
                , ""
                ]
        bar2 = codeLine [qc|bar2|]
        b =
            codeBlock
                [qc|
            foo
                bar
                foo {bar2}
            |]

    b @?= a

case_concatLinesWithSpaceLikeLineBefore = do
    let a =
            S.join
                "\n"
                [ "bar"
                , "    bar2"
                , ""
                , "    123"
                , "    456"
                , ""
                ]
        bar2 =
            codeBlock
                [qc|
            bar2
            |]
        nums1 = codeLine [qc|123|]
        nums2 = codeLine [qc|456|]
        nums = nums1 ++ nums2
        b =
            codeBlock
                [qc|
            bar
                {bar2}
                {inline nums}
            |]

    b @?= a

case_concatLinesWithSpaceWithoutBeforeLine = do
    let a =
            S.join
                "\n"
                [ "bar"
                , "    123"
                , "    456"
                , ""
                ]
        nums1 = codeLine [qc|123|]
        nums2 = codeLine [qc|456|]
        nums = nums1 ++ nums2
        b =
            codeBlock
                [qc|
            bar
                {inline nums}
            |]

    b @?= a

tests :: TestTree
tests = $(testGroupGenerator)
