{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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

import qualified Data.Text as T

-- TODO refactor
import Data.String.Interpolate (i)
import NITTA.Utils.CodeFormatText
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Tasty.TH

--
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

n = "\n" :: T.Text

case_justTextInCodeBlock = do
    let a =
            T.intercalate
                n
                [ "foo"
                , "    bar"
                , "    bar2"
                , ""
                ] ::
                T.Text
        b =
            codeBlock
                [i|
            foo
                bar
                bar2
            |]
    b @?= a

case_codeBlockWithSubConst = do
    let a =
            T.intercalate
                n
                [ "foo"
                , "    bar"
                , "    bar2"
                , ""
                ] ::
                T.Text
        b =
            codeBlock
                [i|
            foo
                bar
                #{"bar2" :: T.Text}
            |]

    b @?= a

case_codeBlockWithSubCodeBlock = do
    let a =
            T.intercalate
                n
                [ "foo"
                , "    bar"
                , "    bar2"
                , ""
                , ""
                ] ::
                T.Text
        bar2 =
            codeBlock
                [i|
            bar2
            |]
        b =
            codeBlock
                [i|
            foo
                bar
                #{bar2}
            |]

    b @?= a

case_codeBlockInOneLineW = do
    let a =
            T.intercalate
                n
                [ "foo"
                , "    bar"
                , "    foo bar2"
                , ""
                , ""
                ] ::
                T.Text
        bar2 =
            codeBlock
                [i|
            bar2
            |]
        b =
            codeBlock
                [i|
            foo
                bar
                foo #{bar2}
            |]

    b @?= a

case_codeLineInOneLine = do
    let a =
            T.intercalate
                n
                [ "foo"
                , "    bar"
                , "    foo bar2"
                , ""
                , ""
                ] ::
                T.Text
        bar2 = codeLine [i|bar2|]
        b =
            codeBlock
                [i|
            foo
                bar
                foo #{bar2}
            |]

    b @?= a

case_concatLinesWithSpaceLikeLineBefore = do
    let a =
            T.intercalate
                n
                [ "bar"
                , "    bar2"
                , ""
                , "    123"
                , "    456"
                , ""
                ] ::
                T.Text
        bar2 =
            codeBlock
                [i|
            bar2
            |]
        nums1 = codeLine [i|123|]
        nums2 = codeLine [i|456|]
        nums = nums1 <> nums2
        b =
            codeBlock
                [i|
            bar
                #{bar2}
                #{inline nums}
            |]

    b @?= a

case_concatLinesWithSpaceWithoutBeforeLine = do
    let a =
            T.intercalate
                n
                [ "bar"
                , "    123"
                , "    456"
                , ""
                ] ::
                T.Text
        nums1 = codeLine [i|123|]
        nums2 = codeLine [i|456|]
        nums = nums1 <> nums2
        b =
            codeBlock
                [i|
            bar
                #{inline nums}
            |]

    b @?= a

tests :: TestTree
tests = $(testGroupGenerator)
