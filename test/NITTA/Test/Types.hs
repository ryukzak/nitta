{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module NITTA.Test.Types
    ( typesTests
    ) where

import           Data.Ratio               ((%))
import           NITTA.Intermediate.Types
import           Test.Tasty               (TestTree)
import           Test.Tasty.HUnit
import           Test.Tasty.TH


case_fixpointInternal = do
    0  @?= scalingFactorPower (FX 0 :: FX 32 32)
    2  @?= scalingFactorPower (FX 0 :: FX 30 32)
    16 @?= scalingFactorPower (FX 0 :: FX 16 32)
    1     @?= scalingFactor (FX 0 :: FX 32 32)
    4     @?= scalingFactor (FX 0 :: FX 30 32)
    65536 @?= scalingFactor (FX 0 :: FX 16 32)


case_fixpointShowRead = do
    "1.000000" @?= show (read "1.0" :: FX 32 32)
    "2.000000" @?= show (read "1.5" :: FX 32 32)
    "0.500000" @?= show (read "0.5" :: FX 31 32)
    "0.250000" @?= show (read "0.25" :: FX 30 32)
    "1.000000" @?= show (read "0.75" :: FX 31 32)
    "0.000000" @?= show (read "0.25" :: FX 31 32)


case_fixpointEnum = do
    0 @?= fromEnum (read "0.25" :: FX 30 32)
    1 @?= fromEnum (read "1.25" :: FX 30 32)
    42 @?= fromEnum (read "42.25" :: FX 30 32)
    toEnum 0 @?= (read "0.0" :: FX 30 32)
    toEnum 1 @?= (read "1.0" :: FX 30 32)
    toEnum 42 @?= (read "42.0" :: FX 30 32)


case_fixpointNum = do
    (read "4" :: FX 30 32) @?= read "2" + read "2"
    (read "4.5" :: FX 30 32) @?= read "2.25" + read "2.25"
    (read "4" :: FX 30 32) @?= read "2" * read "2"
    (read "0.25" :: FX 30 32) @?= read "0.5" * read "0.5"
    (read "0.1" :: FX 30 32) @?= read "0.5" * read "0.2"
    (read "2" :: FX 30 32) @?= abs (read "2")
    (read "2" :: FX 30 32) @?= abs (read "-2")
    -1 @?= signum (read "-2" :: FX 30 32)
    0 @?= signum (read "0" :: FX 30 32)
    1 @?= signum (read "2" :: FX 30 32)

    0 @?= (read "0.0" :: FX 30 32) -- implicitly fromInteger
    1 @?= (read "1.0" :: FX 30 32)
    42 @?= (read "42.0" :: FX 30 32)

    (read "2" :: FX 30 32) @?= negate (read "-2")
    (read "-2" :: FX 30 32) @?= negate (read "2")


case_fixpointReal = do
    1%2 @?= toRational (read "0.5" :: FX 30 32)
    3%2 @?= toRational (read "1.5" :: FX 30 32)


case_fixpointIntegral = do
    0 @?= toInteger (read "0.0" :: FX 30 32)
    1 @?= toInteger (read "1.0" :: FX 30 32)
    42 @?= toInteger (read "42.0" :: FX 30 32)

    ( read "2.0", read "1.0" :: FX 30 32 ) @?= read "5.0" `quotRem` read "2.0"
    ( read "1.0", read "0.25" :: FX 25 32 ) @?= read "0.75" `quotRem` read "0.5"
    ( read "1.0", read "0.25" :: FX 30 32 ) @?= read "0.75" `quotRem` read "0.5"
    ( read "0.0", read "0.25" :: FX 25 32 ) @?= read "0.25" `quotRem` read "0.75"


typesTests :: TestTree
typesTests = $(testGroupGenerator)
