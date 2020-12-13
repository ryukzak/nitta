{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Intermediate.Value
Description : Transferable over nets values.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

-- TODO: instance Bounded (IntX w)
-- TODO: instance Bounded (FX m b w)
-- TODO: instance Arbitrary (IntX w)
-- TODO: instance Arbitrary (FX m b w)

module NITTA.Intermediate.Value
  ( -- *Type classes
    FixedPointCompatible(..)
  , Val(..)
  , DefaultX(..)
  , scalingFactor
    -- *Value types
  , IntX(..)
  , FX(..), minMaxRaw
  ) where

import           Control.Applicative
import           Data.Bits
import           Data.Default
import           Data.Maybe
import           Data.Proxy
import           Data.Ratio
import           Data.Typeable
import           GHC.Generics
import           GHC.TypeLits
import           Numeric
import           Text.InterpolatedString.Perl6 ( qc )
import           Text.Printf
import           Text.Regex


data Attr t = Attr{ value :: t, invalid :: Bool } deriving ( Eq, Ord )

instance Functor Attr where
    fmap f Attr{ value, invalid } = Attr{ value=f value, invalid }

instance Applicative Attr where
    pure x = Attr{ value=x, invalid=True }
    liftA2 f Attr{ value=x, invalid=x' } Attr{ value=y, invalid=y' }
        = Attr{ value=f x y, invalid=x' && y' }

instance ( Read x ) => Read ( Attr x ) where
    readsPrec d r = case readsPrec d r of
        [(x, r')] -> [(pure x, r')]
        _         -> error $ "can not read IntX from: " ++ r

instance ( PrintfArg x ) => PrintfArg ( Attr x ) where
    formatArg Attr{ value } = formatArg value

instance ( Default x ) => Default ( Attr x ) where
    def = pure def

instance ( Enum x ) => Enum ( Attr x ) where
    toEnum = pure . toEnum
    fromEnum Attr{ value } = fromEnum value

instance ( Num x ) => Num ( Attr x ) where
    (+) = liftA2 (+)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger
    negate = fmap negate


instance ( Ord x, Real x ) => Real ( Attr x ) where
    toRational Attr{ value } = toRational value

instance ( Integral x ) => Integral ( Attr x ) where
    toInteger Attr{ value } = toInteger value
    Attr{ value=a } `quotRem` Attr{ value=b }
        = let (a', b') =  a `quotRem` b
        in ( pure a', pure b' )

instance ( Bits x ) => Bits ( Attr x ) where
    (.&.) = liftA2 (.&.)
    (.|.) = liftA2 (.|.)
    xor = liftA2 xor
    complement = fmap complement
    shift Attr{ value } i = pure $ shift value i
    rotate Attr{ value } i = pure $ rotate value i
    bitSize Attr{ value } = fromMaybe undefined $ bitSizeMaybe value
    bitSizeMaybe Attr{ value } = bitSizeMaybe value
    isSigned Attr{ value } = isSigned value
    testBit Attr{ value } = testBit value
    bit i = pure $ bit i
    popCount Attr{ value } = popCount value

-- instance ( KnownNat w ) => FiniteBits ( IntX w ) where
--     finiteBitSize _ = fromInteger $ natVal (Proxy :: Proxy w)

-- instance ( Val w ) => Val ( Attr x ) where
--     serialize (IntX x) = fromIntegral x
--     verilogLiteral (IntX x) = show x

-- instance ( KnownNat w ) => FixedPointCompatible (IntX w) where
--     scalingFactorPower _ = 0
--     fractionalBitSize _ = 0


class ( Default x ) => DefaultX u x | u -> x where
    defX :: u -> x
    defX _ = def


-- |Type class for Value types.
class ( Typeable x, Show x, Read x, Default x, PrintfArg x
      , Eq x, Num x, Bits x, FixedPointCompatible x
      ) => Val x where
    -- |Serialize value and attributes into binary form inside Integer type
    serialize :: x -> Integer

    -- |Convert value to applicable for Verilog literal
    verilogLiteral :: x -> String

    dataWidth :: x -> Int
    attrWidth :: x -> Int
    attrWidth _ = 4

    -- |Helper functions to work with type in verilog
    verilogHelper :: x -> String
    verilogHelper x = [qc|
task trace;
    input integer cycle;
    input integer tick;
    input [{ dataWidth x }-1:0] actual;
    begin
        $write("%0d:%0d\t", cycle, tick);
        $write("actual: %d\t", actual);
        $display();
    end
endtask // trace

task assert;
    input integer cycle;
    input integer tick;
    input [{ dataWidth x }-1:0] actual;
    input [{ dataWidth x }-1:0] expect;
    input [256*8-1:0] var; // string
    begin
        $write("%0d:%0d\t", cycle, tick);
        $write("actual: %d\t", actual);
        $write("expect: %d\t", expect);
        $write("var: %0s\t", var);
        if ( !( actual === expect ) ) $write("FAIL");
        $display();
    end
endtask // assert

task traceWithAttr;
    input integer cycle;
    input integer tick;
    input [{ dataWidth x }-1:0] actualData;
    input [{ attrWidth x }-1:0] actualAttr;
    begin
        $write("%0d:%0d\t", cycle, tick);
        $write("actual: %d %d\t", actualData, actualAttr);
        $display();
    end
endtask // traceWithAttr

task assertWithAttr;
    input integer cycle;
    input integer tick;
    input [{ dataWidth x }-1:0] actualData;
    input [{ attrWidth x }-1:0] actualAttr;
    input [{ dataWidth x }-1:0] expectData;
    input [{ attrWidth x }-1:0] expectAttr;
    input [256*8-1:0] var; // string
    begin
        $write("%0d:%0d\t", cycle, tick);
        $write("actual: %d %d\t", actualData, actualAttr);
        $write("expect: %d %d\t", expectData, expectAttr);
        $write("var: %0s\t", var);
        if ( actualData !== expectData || actualAttr != expectAttr ) $write("FAIL");
        $display();
    end
endtask // assertWithAttr
|]

    -- |RE for extraction assertion data from a testbench log
    verilogAssertRE :: x -> Regex
    verilogAssertRE _ = mkRegex $ concat
        [ "([[:digit:]]+):([[:digit:]]+)\t"
        , "actual: (-?[[:digit:]]+)\t"
        , "expect: (-?[[:digit:]]+)\t"
        , "var: ([^ \t\n]+)"
        ]


-- |Type class for values, which contain information about fractional part of
-- value (for fixed point arithmetics).
class FixedPointCompatible a where
    scalingFactorPower :: a -> Integer
    fractionalBitSize :: a -> Int


scalingFactor x = 2 ** fromIntegral (scalingFactorPower x)


-- for Int
instance Val Int where
    serialize = fromIntegral
    verilogLiteral = show
    dataWidth x = finiteBitSize x

instance FixedPointCompatible Int where
    scalingFactorPower _ = 0
    fractionalBitSize _ = 0



-- |Integer number with specific bit width.
newtype IntX (w :: Nat) = IntX Integer
    deriving ( Show, Eq, Ord )

instance Read ( IntX w ) where
    readsPrec d r = case readsPrec d r of
        [(x, r')] -> [(IntX x, r')]
        _         -> error $ "can not read IntX from: " ++ r

instance PrintfArg ( IntX w ) where
    formatArg (IntX x) = formatInteger x

instance Default ( IntX w ) where
    def = IntX 0

instance Enum ( IntX w ) where
    toEnum = IntX . toInteger
    fromEnum (IntX x) = fromInteger x

instance Num ( IntX w ) where
    ( IntX a ) + ( IntX b ) = IntX ( a + b )
    ( IntX a ) * ( IntX b ) = IntX ( a * b )
    abs ( IntX a ) = IntX $ abs a
    signum ( IntX a ) = IntX $ signum a
    fromInteger a = IntX $ fromInteger a
    negate ( IntX a ) = IntX $ negate a

instance Real ( IntX w ) where
    toRational ( IntX x ) = toRational x

instance Integral ( IntX w ) where
    toInteger ( IntX x ) = toInteger x
    ( IntX a ) `quotRem` ( IntX b )
        = let (a', b') =  a `quotRem` b
        in ( IntX a', IntX b' )

instance Bits ( IntX w ) where
    ( IntX a ) .&. ( IntX b ) = IntX ( a .&. b )
    ( IntX a ) .|. ( IntX b ) = IntX ( a .|. b )
    ( IntX a ) `xor` ( IntX b ) = IntX ( a `xor` b )
    complement ( IntX a ) = IntX $ complement a
    shift ( IntX a ) i = IntX $ shift a i
    rotate ( IntX a ) i = IntX $ rotate a i

    bitSize ( IntX a ) = fromMaybe undefined $ bitSizeMaybe a
    bitSizeMaybe ( IntX a ) = bitSizeMaybe a
    isSigned ( IntX a ) = isSigned a
    testBit ( IntX a ) = testBit a
    bit i = IntX $ bit i
    popCount ( IntX a ) = popCount a

instance ( KnownNat w ) => Val ( IntX w ) where
    serialize (IntX x) = fromIntegral x
    verilogLiteral (IntX x) = show x
    dataWidth _ = fromInteger $ natVal (Proxy :: Proxy w)

instance FixedPointCompatible (IntX w) where
    scalingFactorPower _ = 0
    fractionalBitSize _ = 0



-- |Number with fixed point. FX m b where
--   - m the number of magnitude or integer bits
--   - b the total number of bits
--
-- fxm.b: The "fx" prefix is similar to the above, but uses the word length as
-- the second item in the dotted pair. For example, fx1.16 describes a number
-- with 1 magnitude bit and 15 fractional bits in a 16 bit word.[3]
newtype FX (m :: Nat) (b :: Nat) = FX{ rawFX :: Integer }
    deriving ( Eq, Ord, Generic )

instance ( KnownNat m, KnownNat b ) => Read ( FX m b ) where
    readsPrec d r
        = let
            [(x :: Double, "")] = readsPrec d r
            result = FX $ round (x * scalingFactor result)
        in [ (result, "") ]

instance ( KnownNat m, KnownNat b ) => PrintfArg ( FX m b ) where
    formatArg (FX x) = formatInteger x

instance ( KnownNat m, KnownNat b ) => Show ( FX m b ) where
    show t@(FX x) = showFFloat (Just 6) (fromIntegral x / scalingFactor t :: Double) ""

instance Default ( FX m b ) where
    def = FX 0

instance ( KnownNat m, KnownNat b ) => Enum ( FX m b ) where
    toEnum x
        = let res = FX $ toInteger (x * truncate (scalingFactor res :: Double))
        in res
    fromEnum t@(FX x) = truncate (fromIntegral x / scalingFactor t :: Double)

instance ( KnownNat m, KnownNat b ) => Num ( FX m b ) where
    ( FX a ) + ( FX b ) = zeroOnOverflow $ FX (a + b)
    t@( FX a ) * ( FX b ) = FX ( (a * b) `shiftR` fromInteger (scalingFactorPower t) )
    abs ( FX a ) = FX $ abs a
    signum ( FX a ) = fromInteger $ signum a
    fromInteger x = FX $ shiftL x $ fromInteger $ scalingFactorPower (def :: FX m b)
    negate ( FX a ) = FX $ negate a


minMaxRaw t@FX{} = let
        n = dataWidth t
        maxRaw = 2^(n - 1) - 1
        minRaw = negate (maxRaw + 1)
    in ( minRaw, maxRaw )


zeroOnOverflow t@FX{ rawFX }
    | let ( minRaw, maxRaw ) = minMaxRaw t
    , minRaw <= rawFX && rawFX <= maxRaw
    = t
    | otherwise = 0


instance ( KnownNat m, KnownNat b ) => Integral ( FX m b ) where
    toInteger t = toInteger $ fromEnum t
    t@( FX a ) `quotRem` ( FX b )
        = let
            (a', b') = a `quotRem` b
            sf = scalingFactor t
        in ( FX $ truncate (fromIntegral a' * sf :: Double), FX b' )

instance Bits ( FX m b ) where
    (.&.) = undefined
    -- ( FX a ) .&. ( FX b ) = FX ( a .&. b )
    (.|.) = undefined
    -- ( FX a ) .|. ( FX b ) = FX ( a .|. b )
    xor = undefined
    -- ( FX a ) `xor` ( FX b ) = FX ( a `xor` b )
    complement = undefined
    -- complement ( FX a ) = FX $ complement a
    shift ( FX a ) i = FX $ shift a i
    rotate ( FX a ) i = FX $ rotate a i

    bitSize = undefined
    -- bitSize ( FX a ) = fromMaybe undefined $ bitSizeMaybe a
    bitSizeMaybe = undefined
    -- bitSizeMaybe ( FX a ) = bitSizeMaybe a
    isSigned = undefined
    -- isSigned ( FX a ) = isSigned a
    testBit = undefined
    -- testBit ( FX a ) = testBit a
    bit = undefined
    -- bit i = FX $ bit i
    popCount = undefined
    -- popCount ( FX a ) = popCount a

instance ( KnownNat m, KnownNat b ) => Val ( FX m b ) where
    serialize (FX x) = fromIntegral x
    verilogLiteral (FX x) = show x
    dataWidth _ = fromInteger $ natVal (Proxy :: Proxy b)
    verilogHelper x = [qc|
task trace;
    input integer cycle;
    input integer tick;
    input [{ dataWidth x }-1:0] actual;
    begin
        $write("%0d:%0d\t", cycle, tick);
        $write("actual: %.3f\t", fxtor(actual));
        $display();
    end
endtask // trace

task assert;
    input integer cycle;
    input integer tick;
    input [{ dataWidth x }-1:0] actual;
    input [{ dataWidth x }-1:0] expect;
    input [256*8-1:0] var; // string
    begin
        $write("%0d:%0d\t", cycle, tick);
        $write("actual: %.3f\t", fxtor(actual));
        $write("expect: %.3f\t", fxtor(expect));
        $write("var: %0s\t", var);
        if ( !( actual === expect ) ) $write("FAIL");
        $display();
    end
endtask // assert

task traceWithAttr;
    input integer cycle;
    input integer tick;
    input [{ dataWidth x }-1:0] actualData;
    input [{ attrWidth x }-1:0] actualAttr;
    begin
        $write("%0d:%0d\t", cycle, tick);
        $write("actual: %.3f %d\t", fxtor(actualData), actualAttr);
        $display();
    end
endtask // traceWithAttr

task assertWithAttr;
    input integer cycle;
    input integer tick;
    input [{ dataWidth x }-1:0] actualData;
    input [{ attrWidth x }-1:0] actualAttr;
    input [{ dataWidth x }-1:0] expectData;
    input [{ attrWidth x }-1:0] expectAttr;
    input [256*8-1:0] var; // string
    begin
        $write("%0d:%0d\t", cycle, tick);
        $write("actual: %.3f\t", fxtor(actualData), actualAttr);
        $write("expect: %.3f\t", fxtor(expectData), expectAttr);
        $write("var: %0s\t", var);
        if ( actualData !== expectData || actualAttr != expectAttr ) $write("FAIL");
        $display();
    end
endtask // assertWithAttr

function real fxtor(input integer x);
    begin
        fxtor = $itor(x) / $itor(1 << { scalingFactorPower x });
    end
endfunction // fxtor
|]

    verilogAssertRE _ = mkRegex $ concat
        [ "([[:digit:]]+):([[:digit:]]+)\t"
        , "actual: (-?[[:digit:]]+\\.[[:digit:]]+)\t"
        , "expect: (-?[[:digit:]]+\\.[[:digit:]]+)\t"
        , "var: ([^ \t\n]+)"
        ]


instance ( KnownNat m, KnownNat b ) => FixedPointCompatible ( FX m b ) where
    fractionalBitSize x = dataWidth x - fromInteger (natVal (Proxy :: Proxy m))
    scalingFactorPower _
        = let
            m = natVal (Proxy :: Proxy m)
            b = natVal (Proxy :: Proxy b)
        in b - m


instance ( KnownNat m, KnownNat b ) => Real ( FX m b ) where
    toRational x@FX{ rawFX } = rawFX % 2 ^ scalingFactorPower x
