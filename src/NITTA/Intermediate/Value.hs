{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{- |
Module      : NITTA.Intermediate.Value
Description : Processed value representation
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Intermediate.Value (
    -- * Type classes
    Val (..),
    DefaultX (..),
    FixedPointCompatible (..),
    scalingFactor,
    minMaxRaw,

    -- * Compositional type
    Attr (..),

    -- * Value types
    IntX (..),
    FX (..),
) where

import Control.Applicative
import Data.Aeson
import Data.Bits
import Data.Default
import Data.Maybe
import Data.Proxy
import Data.Ratio
import Data.String.Interpolate
import Data.Text qualified as T
import Data.Typeable
import Data.Validity hiding (invalid)
import GHC.Generics
import GHC.TypeLits
import NITTA.Utils.Base
import Numeric
import Text.Printf
import Text.Regex

-- | Type class for representation of processing values. A value should include two parts: data and attribute.
class
    ( Typeable x
    , Show x
    , Read x
    , PrintfArg x
    , Default x
    , Integral x
    , Enum x
    , Eq x
    , Num x
    , Bits x
    , Validity x
    , FixedPointCompatible x
    ) =>
    Val x
    where
    -- | data bus width
    dataWidth :: x -> Int

    -- | attribute bus width
    attrWidth :: x -> Int
    attrWidth _ = 4

    -- | raw representation of data (@Integer@)
    rawData :: x -> Integer

    -- | raw representation of attributes (@Integer@)
    rawAttr :: x -> Integer

    -- | construct a value from raw data and attributes
    fromRaw :: Integer -> Integer -> x

    -- | convert a value to Verilog literal with data
    dataLiteral :: x -> T.Text

    -- | convert a value to Verilog literal with attributes
    attrLiteral :: x -> T.Text
    attrLiteral x = showText (attrWidth x) <> "'d0000"

    -- | helper functions to work with values in Verilog (trace and assert)
    verilogHelper :: x -> T.Text
    verilogHelper x =
        [__i|
            task traceWithAttr;
                input integer cycle;
                input integer tick;
                input [#{ dataWidth x }-1:0] actualData;
                input [#{ attrWidth x }-1:0] actualAttr;
                begin
                    $write("%0d:%0d\t", cycle, tick);
                    $write("actual: %d %d\t", actualData, actualAttr);
                    $display();
                end
            endtask // traceWithAttr

            task assertWithAttr;
                input integer cycle;
                input integer tick;
                input [#{ dataWidth x }-1:0] actualData;
                input [#{ attrWidth x }-1:0] actualAttr;
                input [#{ dataWidth x }-1:0] expectData;
                input [#{ attrWidth x }-1:0] expectAttr;
                input [256*8-1:0] var; // string
                begin
                    $write("%0d:%0d\t", cycle, tick);
                    $write("actual: %d %d\t", actualData, actualAttr);
                    $write("expect: %d %d\t", expectData, expectAttr);
                    $write("var: %0s\t", var);
                    if ( actualData != expectData || actualAttr != expectAttr
                        || ( actualData === 'dx && !actualAttr[0] )
                        ) $write("FAIL");
                    $display();
                end
            endtask // assertWithAttr
        |]

    -- | RE for extraction assertion data from a testbench log
    verilogAssertRE :: x -> Regex
    verilogAssertRE _ =
        mkRegex $
            concat
                [ "([[:digit:]]+):([[:digit:]]+)[\t ]+"
                , "actual:[\t ]+(-?[[:digit:]]+)[\t ]+[x[:digit:]]+[\t ]+"
                , "expect:[\t ]+(-?[[:digit:]]+)[\t ]+[x[:digit:]]+[\t ]+"
                , "var: ([^ \t\n]+)"
                ]

-- | Minimal and maximal raw value.
minMaxRaw x =
    let n = dataWidth x
     in minMaxRaw' n

minMaxRaw' n =
    let maxRaw = 2 ^ (n - 1) - 1
        minRaw = negate (maxRaw + 1)
     in (minRaw, maxRaw)

invalidRaw x = snd (minMaxRaw x) + 1

crop x
    | abs x == x = x .&. valueMask x
    | otherwise = x .|. complement (valueMask x)

valueMask :: Val x => x -> x
valueMask x = fromRaw (setBit (0 :: Integer) (dataWidth x - 1) - 1) 0

-- TODO: try to avoid this class
class (Default x) => DefaultX u x | u -> x where
    defX :: u -> x
    defX _ = def

{- | Type class for values, which contain information about fractional part of
value (for fixed point arithmetics).
-}
class FixedPointCompatible a where
    scalingFactorPower :: a -> Integer
    fractionalBitSize :: a -> Int

scalingFactor x = 2 ** fromIntegral (scalingFactorPower x)

-- | All values with attributes.
data Attr x = Attr {value :: x, invalid :: Bool} deriving (Eq, Ord)

instance (Validity x) => Validity (Attr x) where
    validate Attr{value} = validate value

setInvalidAttr Attr{value, invalid} = Attr{value, invalid = invalid || isInvalid value}

instance Functor Attr where
    fmap f Attr{value, invalid} = Attr{value = f value, invalid}

instance Applicative Attr where
    pure x = Attr{value = x, invalid = False}

    liftA2 f Attr{value = x, invalid = x'} Attr{value = y, invalid = y'} =
        let value = f x y
         in Attr{value, invalid = x' || y'}

instance (Show x) => Show (Attr x) where
    show Attr{invalid = True} = "NaN"
    show Attr{value, invalid = False} = show value

instance (Read x) => Read (Attr x) where
    readsPrec d r = case readsPrec d r of
        [(x, r')] -> [(pure x, r')]
        _ -> error $ "can not read IntX from: " ++ r

instance (PrintfArg x) => PrintfArg (Attr x) where
    formatArg Attr{value} = formatArg value

instance (Default x) => Default (Attr x) where
    def = pure def

instance (Enum x, Validity x) => Enum (Attr x) where
    toEnum = setInvalidAttr . pure . toEnum
    fromEnum Attr{value} = fromEnum value

instance (Num x, Validity x) => Num (Attr x) where
    a + b = setInvalidAttr $ liftA2 (+) a b
    a * b = setInvalidAttr $ liftA2 (*) a b
    abs = setInvalidAttr . fmap abs
    signum = setInvalidAttr . fmap signum
    fromInteger = setInvalidAttr . pure . fromInteger
    negate = setInvalidAttr . fmap negate

instance (Ord x, Real x, Validity x) => Real (Attr x) where
    toRational Attr{value} = toRational value

instance (Integral x, Validity x, Val x) => Integral (Attr x) where
    toInteger Attr{value} = toInteger value
    Attr{value = a} `quotRem` Attr{value = b} =
        let (minB, maxB) = minMaxRaw' (dataWidth b `shiftR` 1)
            (a', b') =
                if b == 0 || b < minB || maxB < b
                    then (fromRaw (invalidRaw b) def, fromRaw (invalidRaw b) def)
                    else a `quotRem` b
         in (setInvalidAttr $ pure a', setInvalidAttr $ pure b')

instance (Bits x, Validity x) => Bits (Attr x) where
    a .&. b = setInvalidAttr $ liftA2 (.&.) a b
    a .|. b = setInvalidAttr $ liftA2 (.|.) a b
    xor a b = setInvalidAttr $ liftA2 xor a b
    complement = setInvalidAttr . fmap complement
    shift Attr{value} ix = setInvalidAttr $ pure $ shift value ix
    rotate Attr{value} ix = setInvalidAttr $ pure $ rotate value ix
    bitSize Attr{value} = fromMaybe undefined $ bitSizeMaybe value
    bitSizeMaybe Attr{value} = bitSizeMaybe value
    isSigned Attr{value} = isSigned value
    testBit Attr{value} = testBit value
    bit ix = pure $ bit ix
    popCount Attr{value} = popCount value

instance (Val x, Integral x) => Val (Attr x) where
    dataWidth Attr{value} = dataWidth value

    rawData Attr{value} = rawData value
    rawAttr Attr{invalid = True} = 1
    rawAttr Attr{invalid = False} = 0

    fromRaw x a = setInvalidAttr $ pure $ fromRaw x a

    dataLiteral Attr{value, invalid = True} = showText (dataWidth value) <> "'dx"
    dataLiteral Attr{value} = dataLiteral value
    attrLiteral x@Attr{invalid} =
        showText (attrWidth x) <> "'b000" <> if invalid then "1" else "0"

    verilogHelper Attr{value} = verilogHelper value
    verilogAssertRE Attr{value} = verilogAssertRE value

instance (FixedPointCompatible x) => FixedPointCompatible (Attr x) where
    scalingFactorPower Attr{value} = scalingFactorPower value
    fractionalBitSize Attr{value} = fractionalBitSize value

instance (ToJSON x) => ToJSON (Attr x) where
    toJSON Attr{value} = toJSON value

-- * Integer

instance FixedPointCompatible Int where
    scalingFactorPower _ = 0
    fractionalBitSize _ = 0

instance Val Int where
    dataWidth x = finiteBitSize x

    rawData x = fromIntegral x
    rawAttr _ = 0
    fromRaw x _ = fromEnum x

    dataLiteral = showText

-- | Integer number with specific bit width.
newtype IntX (w :: Nat) = IntX {intX :: Integer}
    deriving (Show, Eq, Ord)

instance (KnownNat m) => Validity (IntX m) where
    validate x@(IntX raw) =
        let (minRaw, maxRaw) = minMaxRaw x
         in check (minRaw <= raw && raw <= maxRaw) "value is not out of range"

instance Read (IntX w) where
    readsPrec d r = case readsPrec d r of
        [(x, r')] -> [(IntX x, r')]
        _ -> error $ "can not read IntX from: " ++ r

instance PrintfArg (IntX w) where
    formatArg (IntX x) = formatInteger x

instance Default (IntX w) where
    def = IntX 0

instance Enum (IntX w) where
    toEnum = IntX . toInteger
    fromEnum (IntX x) = fromInteger x

instance Num (IntX w) where
    (IntX a) + (IntX b) = IntX (a + b)
    (IntX a) * (IntX b) = IntX (a * b)
    abs (IntX a) = IntX $ abs a
    signum (IntX a) = IntX $ signum a
    fromInteger a = IntX $ fromInteger a
    negate (IntX a) = IntX $ negate a

instance Real (IntX w) where
    toRational (IntX x) = toRational x

instance Integral (IntX w) where
    toInteger (IntX x) = toInteger x
    (IntX a) `quotRem` (IntX b) =
        let (a', b') = a `quotRem` b
         in (IntX a', IntX b')

instance (KnownNat w) => Bits (IntX w) where
    (IntX a) .&. (IntX b) = IntX (a .&. b)
    (IntX a) .|. (IntX b) = IntX (a .|. b)
    (IntX a) `xor` (IntX b) = IntX (a `xor` b)
    complement (IntX a) = IntX $ complement a
    shift (IntX a) ix = crop $ IntX $ shift a ix
    rotate (IntX a) ix = crop $ IntX $ rotate a ix

    bitSize (IntX a) = fromMaybe undefined $ bitSizeMaybe a
    bitSizeMaybe (IntX a) = bitSizeMaybe a
    isSigned (IntX a) = isSigned a
    testBit (IntX a) = testBit a
    bit ix = IntX $ bit ix
    popCount (IntX a) = popCount a

instance (KnownNat w) => Val (IntX w) where
    dataWidth _ = fromInteger $ natVal (Proxy :: Proxy w)

    rawData (IntX x) = fromIntegral x
    rawAttr x = if isInvalid x then 1 else 0

    fromRaw x _ = IntX x

    dataLiteral (IntX x) = showText x

instance FixedPointCompatible (IntX w) where
    scalingFactorPower _ = 0
    fractionalBitSize _ = 0

instance ToJSON (IntX w) where
    toJSON (IntX x) = toJSON x

-- * Fixed point

{- | Number with fixed point. FX m b where

- m the number of magnitude or integer bits
- b the total number of bits

fxm.b: The "fx" prefix is similar to the above, but uses the word length as
the second item in the dotted pair. For example, fx1.16 describes a number
with 1 magnitude bit and 15 fractional bits in a 16 bit word.
-}
newtype FX (m :: Nat) (b :: Nat) = FX {rawFX :: Integer}
    deriving (Eq, Ord, Generic)

instance (KnownNat b, KnownNat m) => Validity (FX m b) where
    validate t@(FX raw) =
        let (minRaw, maxRaw) = minMaxRaw t
         in check (minRaw <= raw && raw <= maxRaw) "value is not out of range"

instance (KnownNat m, KnownNat b) => Read (FX m b) where
    readsPrec d r =
        let [(x :: Double, "")] = readsPrec d r
            result = FX $ round (x * scalingFactor result)
         in [(result, "")]

instance (KnownNat m, KnownNat b) => PrintfArg (FX m b) where
    formatArg (FX x) = formatInteger x

instance (KnownNat m, KnownNat b) => Show (FX m b) where
    show t@(FX x) = showFFloat (Just 6) (fromIntegral x / scalingFactor t :: Double) ""

instance Default (FX m b) where
    def = FX 0

instance (KnownNat m, KnownNat b) => Enum (FX m b) where
    toEnum x =
        let res = FX $ toInteger (x * truncate (scalingFactor res :: Double))
         in res
    fromEnum t@(FX x) = truncate (fromIntegral x / scalingFactor t :: Double)

instance (KnownNat m, KnownNat b) => Num (FX m b) where
    (FX a) + (FX b) = FX (a + b)
    t@(FX a) * (FX b) = FX ((a * b) `shiftR` fromInteger (scalingFactorPower t))
    abs (FX a) = FX $ abs a
    signum (FX a) = fromInteger $ signum a
    fromInteger x = FX $ shiftL x $ fromInteger $ scalingFactorPower (def :: FX m b)
    negate (FX a) = FX $ negate a

instance (KnownNat m, KnownNat b) => Integral (FX m b) where
    toInteger t = toInteger $ fromEnum t
    t@(FX a) `quotRem` (FX b) =
        let (a', b') = a `quotRem` b
            sf = scalingFactor t
         in (FX $ truncate (fromIntegral a' * sf :: Double), FX b')

instance (KnownNat m, KnownNat b) => Bits (FX m b) where
    (FX a) .&. (FX b) = FX (a .&. b)
    (FX a) .|. (FX b) = FX (a .|. b)
    (FX a) `xor` (FX b) = FX (a `xor` b)
    complement (FX a) = FX $ complement a
    shift (FX a) ix = crop $ FX $ shift a ix
    rotate (FX a) ix = crop $ FX $ rotate a ix
    bitSize = dataWidth
    bitSizeMaybe = Just . dataWidth
    isSigned (FX a) = isSigned a
    testBit (FX a) = testBit a
    bit ix = FX $ bit ix
    popCount (FX a) = popCount a

instance (KnownNat m, KnownNat b) => Val (FX m b) where
    dataWidth _ = fromInteger $ natVal (Proxy :: Proxy b)

    rawData (FX x) = x
    rawAttr x = if isInvalid x then 1 else 0
    fromRaw x _ = FX x

    dataLiteral (FX x) = showText x
    attrLiteral x = showText (attrWidth x) <> "'d000" <> showText (rawAttr x)

    verilogHelper x =
        [__i|
            task traceWithAttr;
                input integer cycle;
                input integer tick;
                input [#{ dataWidth x }-1:0] actualData;
                input [#{ attrWidth x }-1:0] actualAttr;
                begin
                    $write("%0d:%0d\t", cycle, tick);
                    $write("actual: %.3f %d\t", fxtor(actualData), actualAttr);
                    $display();
                end
            endtask // traceWithAttr

            task assertWithAttr;
                input integer cycle;
                input integer tick;
                input [#{ dataWidth x }-1:0] actualData;
                input [#{ attrWidth x }-1:0] actualAttr;
                input [#{ dataWidth x }-1:0] expectData;
                input [#{ attrWidth x }-1:0] expectAttr;
                        input [256*8-1:0] var; // string
                begin
                    $write("%0d:%0d\t", cycle, tick);
                    $write("actual: %.3f %d \t", fxtor(actualData), actualAttr);
                    $write("expect: %.3f %d \t", fxtor(expectData), expectAttr);
                    $write("var: %0s\t", var);
                    if ( actualData !== expectData || actualAttr != expectAttr ) $write("FAIL");
                    $display();
                end
            endtask // assertWithAttr

            function real fxtor(input integer x);
                begin
                    fxtor = $itor(x) / $itor(1 << #{ scalingFactorPower x });
                end
            endfunction // fxtor
        |]

    verilogAssertRE _ =
        mkRegex $
            concat
                [ "([[:digit:]]+):([[:digit:]]+)[\t ]"
                , "actual:[\t ](-?[[:digit:]]+\\.[[:digit:]]+)[\t ]+[x[:digit:]]+[\t ]+"
                , "expect:[\t ](-?[[:digit:]]+\\.[[:digit:]]+)[\t ]+[x[:digit:]]+[\t ]+"
                , "var: ([^ \t\n]+)"
                ]

instance (KnownNat m, KnownNat b) => FixedPointCompatible (FX m b) where
    fractionalBitSize x = dataWidth x - fromInteger (natVal (Proxy :: Proxy m))
    scalingFactorPower _ =
        let m = natVal (Proxy :: Proxy m)
            b = natVal (Proxy :: Proxy b)
         in b - m

instance (KnownNat m, KnownNat b) => Real (FX m b) where
    toRational x@FX{rawFX} = rawFX % 2 ^ scalingFactorPower x

instance (KnownNat m, KnownNat b) => ToJSON (FX m b) where
    toJSON x@FX{} = toJSON (read $ show x :: Double)
