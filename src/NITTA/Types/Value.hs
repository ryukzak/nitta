{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.Types.Value
Description : Transferable over nets values.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Types.Value
  ( -- *Type classes
    FixedPointCompatible(..)
  , Val(..)
  , scalingFactor
    -- *Value types
  , IntX(..)
  , FX(..)
  ) where

import           Data.Bits
import           Data.Default
import           Data.Maybe
import           Data.Proxy
import           Data.Typeable
import           GHC.TypeLits


-- |Type class for Value types.
class ( Typeable x, Show x, Read x, FixedPointCompatible x, Default x ) => Val x where
    showTypeOf :: Proxy x -> String
    verilogInteger :: x -> Integer


-- |Type class for values, which contain information about fractional part of value (for fixed point arithmetics).
class ( FiniteBits a ) => FixedPointCompatible a where
    scalingFactorPower :: a -> Integer
    fractionalBitSize :: a -> Int


scalingFactor x = 2 ** fromIntegral (scalingFactorPower x)


-- for Int
instance Val Int where
    showTypeOf _ = "Int"
    verilogInteger = fromIntegral

instance FixedPointCompatible Int where
    scalingFactorPower _ = 0
    fractionalBitSize _ = 0



-- |Integer number with specific bit width.
newtype IntX (w :: Nat) = IntX Integer
    deriving ( Show, Eq, Ord )

instance Read ( IntX w ) where
    readsPrec d r = let [(x, "")] = readsPrec d r
        in [(IntX x, "")]

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

instance ( KnownNat w ) => FiniteBits ( IntX w ) where
    finiteBitSize _ = fromInteger $ natVal (Proxy :: Proxy w)

instance ( KnownNat w ) => Val ( IntX w ) where
    showTypeOf _ = "IntX" ++ show (natVal (Proxy :: Proxy w))
    verilogInteger (IntX x) = fromIntegral x

instance ( KnownNat w ) => FixedPointCompatible (IntX w) where
    scalingFactorPower _ = 0
    fractionalBitSize _ = 0



-- |Number with fixed point. FX m b where
--   - m the number of magnitude or integer bits
--   - b the total number of bits
--
-- fxm.b: The "fx" prefix is similar to the above, but uses the word length as
-- the second item in the dotted pair. For example, fx1.16 describes a number
-- with 1 magnitude bit and 15 fractional bits in a 16 bit word.[3]
newtype FX (m :: Nat) (b :: Nat) = FX Integer
    deriving ( Eq, Ord )

instance ( KnownNat m, KnownNat b ) => Show ( FX m b ) where
    show t@(FX x) = show (fromIntegral x / scalingFactor t :: Double)

instance ( KnownNat m, KnownNat b ) => Read ( FX m b ) where
    readsPrec d r
        = let
            [(x, "")] = readsPrec d r
            result = FX $ truncate (x * scalingFactor result :: Double)
        in [(result, "")]

instance Default ( FX m b ) where
    def = FX 0

instance ( KnownNat m, KnownNat b ) => Enum ( FX m b ) where
    toEnum x
        = let res = FX $ toInteger (x * truncate (scalingFactor res :: Double))
        in res
    fromEnum t@(FX x) = truncate (fromIntegral x / scalingFactor t :: Double)

instance ( KnownNat m, KnownNat b ) => Num ( FX m b ) where
    ( FX a ) + ( FX b ) = FX ( a + b )
    t@( FX a ) * ( FX b ) = FX ( truncate (fromIntegral (a * b) / scalingFactor t :: Double) )
    abs ( FX a ) = FX $ abs a
    signum ( FX a ) = fromInteger $ signum a
    fromInteger x
        = let res = FX $ fromIntegral (x * truncate (scalingFactor res :: Double))
        in res
    negate ( FX a ) = FX $ negate a

instance ( KnownNat m, KnownNat b ) => Real ( FX m b ) where
    toRational t@( FX x ) = toRational (fromIntegral x / scalingFactor t :: Double)

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

instance ( KnownNat b ) => FiniteBits ( FX m b ) where
    finiteBitSize _ = fromInteger $ natVal (Proxy :: Proxy b)

instance ( KnownNat m, KnownNat b ) => Val ( FX m b ) where
    showTypeOf _ = let
            m = natVal (Proxy :: Proxy m)
            b = natVal (Proxy :: Proxy b)
        in "FX" ++ show m ++ "_" ++ show b
    verilogInteger (FX x) = fromIntegral x

instance ( KnownNat m, KnownNat b ) => FixedPointCompatible ( FX m b ) where
    fractionalBitSize x = finiteBitSize x - fromInteger (natVal (Proxy :: Proxy m))
    scalingFactorPower _
        = let
            m = natVal (Proxy :: Proxy m)
            b = natVal (Proxy :: Proxy b)
        in b - m
