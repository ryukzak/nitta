{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-type-defaults -fno-warn-orphans #-}

{-|
Module      : NITTA.Intermediate.Value.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Intermediate.Value.Tests
    ( tests
    ) where

import           Data.Bits
import           Data.Default
import           Data.GenValidity
import           Data.Proxy
import           Data.Ratio
import           GHC.TypeLits
import           NITTA.Intermediate.Types
import           Test.QuickCheck
import           Test.Tasty ( TestTree )
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck ( testProperty )
import           Test.Tasty.TH
import           Test.Validity.Property


case_fx_Internal = do
    0  @?= scalingFactorPower (FX 0 :: FX 32 32)
    2  @?= scalingFactorPower (FX 0 :: FX 30 32)
    16 @?= scalingFactorPower (FX 0 :: FX 16 32)
    1     @?= scalingFactor (FX 0 :: FX 32 32)
    4     @?= scalingFactor (FX 0 :: FX 30 32)
    65536 @?= scalingFactor (FX 0 :: FX 16 32)


case_fx_Show_and_Read = do
    "1.000000" @?= show (read "1.0" :: FX 32 32)
    "2.000000" @?= show (read "1.5" :: FX 32 32)
    "0.500000" @?= show (read "0.5" :: FX 31 32)
    "0.250000" @?= show (read "0.25" :: FX 30 32)
    "1.000000" @?= show (read "0.75" :: FX 31 32)
    "0.000000" @?= show (read "0.25" :: FX 31 32)


case_fx_Enum = do
    0 @?= fromEnum (read "0.25" :: FX 30 32)
    1 @?= fromEnum (read "1.25" :: FX 30 32)
    42 @?= fromEnum (read "42.25" :: FX 30 32)
    toEnum 0 @?= (read "0.0" :: FX 30 32)
    toEnum 1 @?= (read "1.0" :: FX 30 32)
    toEnum 42 @?= (read "42.0" :: FX 30 32)


case_fx_Num = do
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


-- FIXME: valid data generation
--
-- Fail example:
-- (1 + 19) + 109 -> 20 + 109 -> 0 (overflow)
-- 1 + (19 + 109) -> 1 + 0 (overflow) -> 1
-- prop_fx_add_associativity = associativeOnValids ((+) @(FX 4 8))

prop_fx_add_commutative = commutative ((+) @(FX 4 8))

prop_fx_fromInteger_0_additive_identity = forAllValid (\(x :: FX 4 8) -> x + fromInteger 0 == x)

prop_fx_negate_additive_inverse = forAllValid (\(x :: FX 4 8) -> x + negate x == fromInteger 0)

-- prop_fx_mul_associativity = associativeOnValids ((*) @(FX 4 8))

prop_fx_mul_commutative = commutative ((*) @(FX 4 8))

prop_fx_fromInteger_1_muliplicative_identity = forAllValid (\(x :: FX 4 8) -> x * fromInteger 1 == x)

prop_distributivity = forAllValid $ num_distributivity (Proxy :: Proxy (FX 4 4)) (Proxy :: Proxy (FX 8 8))

num_distributivity ::
    ( KnownNat m1, KnownNat b1, KnownNat m2, KnownNat b2
    ) => Proxy (FX m1 b1) -> Proxy (FX m2 b2) -> ( FX m1 b1, FX m1 b1, FX m1 b1 ) -> Bool
num_distributivity _ p (a0, b0, c0) = let
        a = repack a0 p
        b = repack b0 p
        c = repack c0 p
    in a * (b + c) == (a * b) + (a * c)
       && (b + c) * a == (b * a) + (c * a)


prop_fx_add_integrity
    = forAllValid $ addIntegrity (Proxy :: Proxy (FX 8 8) ) ( Proxy :: Proxy (FX 9 9) )

prop_fx_mul_integrity
    = forAllValid $ mulIntegrity (Proxy :: Proxy (FX 8 8) ) ( Proxy :: Proxy (FX 16 16) )

prop_fx_mul_integrity_fraction
    = forAllValid $ mulIntegrity (Proxy :: Proxy (FX 2 4) ) ( Proxy :: Proxy (FX 6 8) )


case_fx_Real = do
    1%2 @?= toRational (read "0.5" :: FX 30 32)
    3%2 @?= toRational (read "1.5" :: FX 30 32)


case_fx_Integral = do
    0 @?= toInteger (read "0.0" :: FX 30 32)
    1 @?= toInteger (read "1.0" :: FX 30 32)
    42 @?= toInteger (read "42.0" :: FX 30 32)

    ( read "2.0", read "1.0" :: FX 30 32 ) @?= read "5.0" `quotRem` read "2.0"
    ( read "1.0", read "0.25" :: FX 25 32 ) @?= read "0.75" `quotRem` read "0.5"
    ( read "1.0", read "0.25" :: FX 30 32 ) @?= read "0.75" `quotRem` read "0.5"
    ( read "0.0", read "0.25" :: FX 25 32 ) @?= read "0.25" `quotRem` read "0.75"



-- *Utils

instance ( KnownNat b ) => Validity (FX m b) where
    validate t@(FX raw) = let
            ( minRaw, maxRaw ) = minMaxRaw t
        in check (minRaw <= raw && raw <= maxRaw) "value is not out of range"

instance GenUnchecked (FX m b)

instance ( KnownNat b ) => GenValid (FX m b) where
    genValid = FX <$> choose (minMaxRaw (def :: FX m b))
    shrinkValid = filter isValid . shrinkUnchecked


addIntegrity ::
    ( KnownNat m1, KnownNat b1, KnownNat m2, KnownNat b2
    ) => Proxy (FX m1 b1) -> Proxy (FX m2 b2) -> ( FX m1 b1, FX m1 b1 ) -> Bool
addIntegrity _ p (a, b) = let
        a' = repack a p
        b' = repack b p
    in toRational (a' + b') == toRational a + toRational b


mulIntegrity ::
    ( KnownNat m1, KnownNat b1, KnownNat m2, KnownNat b2
    ) => Proxy (FX m1 b1) -> Proxy (FX m2 b2) -> ( FX m1 b1, FX m1 b1 ) -> Bool
mulIntegrity _ p (a, b) = let
        a' = repack a p
        b' = repack b p
        minPart = 1 % (2 ^ scalingFactorPower (def `asProxyTypeOf` p))
    in toRational (a' * b') == ratioFloor minPart (toRational a * toRational b)


ratioFloor minPart x
    | denominator x > denominator minPart
    = ratioFloor minPart (x - (1 % denominator x))
    | otherwise = x

case_fx_ratioFloor = do
    0 @?= ratioFloor (1%4) (1%8)
    1%4 @?= ratioFloor (1%4) (3%8)


repack :: forall m1 b1 m2 b2 .
    ( KnownNat m1, KnownNat b1, KnownNat m2, KnownNat b2
    ) => FX m1 b1 -> Proxy (FX m2 b2) -> FX m2 b2
repack a _ = let
        n1 = fromIntegral $ scalingFactorPower a
        n2 = fromIntegral $ scalingFactorPower (def :: FX m2 b2)
        offset = n2 - n1
    in FX ( rawFX a `shiftL` offset)

case_fx_repack = do
    ( read "0.5" :: FX 2 4 ) @?= repack ( read "0.5" :: FX 3 4 ) Proxy
    ( read "0.5" :: FX 3 4 ) @?= repack ( read "0.5" :: FX 2 4 ) Proxy
    ( read "-8"  :: FX 8 8 ) @?= repack ( read "-8"  :: FX 4 4 ) Proxy


tests :: TestTree
tests = $(testGroupGenerator)
