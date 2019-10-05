{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module NITTA.Test.CodeBlock
    ( codeTests
    ) where

import           Test.Tasty       (TestTree)
import           Test.Tasty.HUnit
import           Test.Tasty.TH
import           NITTA.Project.Snippets
import           Text.InterpolatedString.Perl6    (qc)
import qualified Data.String.Utils as S


case_inline1 = do
    let
        a1 = S.join "\n"  
            [ "foo"
            , "    bar"
            , "    bar2"
            , ""
            , ""
            ] 
        b1 = codeBlock [qc|
            foo
                bar
                bar2
            |]
    b1 @?= a1

case_inline2 = do
    let
        a2 = S.join "\n"  
            [ "foo"
            , "    bar"
            , "    bar2"
            , ""
            , ""
            ] 
        b2 = codeBlock [qc|
            foo
                bar
                {"bar2"}
            |]
    b2 @?= a2

case_inline3 = do
    let
        a3 = S.join "\n"  
            [ "foo"
            , "    bar"
            , "    bar2"
            , ""
            , ""
            , ""
            , ""
            ] 
        bar2 = codeBlock [qc|
            bar2
            |]
        b3 = codeBlock [qc|
            foo
                bar
                {bar2}
            |]

    b3 @?= a3

case_inline4 = do
    let
        a4 = S.join "\n"  
            [ "foo"
            , "    bar"
            , "    lol bar2"
            , ""
            , ""
            , ""
            , ""
            ] 
        bar2 = codeBlock [qc|
            bar2
            |]
        b4 = codeBlock [qc|
            foo
                bar
                lol {bar2}
            |]

    b4 @?= a4

case_inline5 = do
    let
        a5 = S.join "\n"  
            [ "foo"
            , "    bar"
            , "    lol bar2"
            , ""
            , ""
            , ""
            ] 
        bar2 = codeLine 0 [qc|bar2|]
        b5 = codeBlock [qc|
            foo
                bar
                lol {bar2}
            |]

    b5 @?= a5

case_inline6 = do
    let
        a6 = S.join "\n"  
            [ "bar"
            , "    bar2"
            , ""
            , ""
            , "    123"
            , "    456"
            , ""
            , ""
            , ""
            ] 
        bar2 = codeBlock [qc|
            bar2
            |]
        lol1 = codeLine 0 [qc|123|]
        lol2 = codeLine 0 [qc|456|]
        lol = lol1 ++ lol2
        b6 = codeBlock [qc|
            bar
                {bar2}
                {inline lol}
            |]

    b6 @?= a6

codeTests :: TestTree
codeTests = $(testGroupGenerator)
