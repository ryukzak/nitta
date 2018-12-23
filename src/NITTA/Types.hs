{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.Types
Description :
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Types
  ( module NITTA.Types.Base
  , module NITTA.Types.Network
  , module NITTA.Types.Poly
  , IntX(..)
  , FX(..)
  , Val(..)
  , widthX
  , scalingFactor
  , scalingFactorPowerOfProxy
  ) where

import           NITTA.Types.Base
import           NITTA.Types.Network
import           NITTA.Types.Poly

import           Data.Bits
import           Data.Default
import           Data.Maybe
import           Data.Proxy
import           Data.Typeable
import           GHC.TypeLits


class ( Typeable x, Read x, WithX x x ) => Val x where
    showTypeOf :: Proxy x -> String
    valueWidth :: Proxy x -> Integer
    scalingFactorPower :: x -> Integer
    verilogInt :: x -> Int

widthX pu = valueWidth $ proxyX pu
scalingFactorPowerOfProxy p = scalingFactorPower (undefined `asProxyTypeOf` p)


-- for Int
instance WithX Int Int

instance Val Int where
    showTypeOf _ = "Int"
    valueWidth _ = 32
    scalingFactorPower _ = 0
    verilogInt = id



-- for Integer
instance WithX Integer Integer

instance Val Integer where
    showTypeOf _ = "Integer"
    valueWidth _ = 32
    scalingFactorPower _ = 0
    verilogInt = fromInteger



-- for IntX width
newtype IntX (w :: Nat) = IntX Int
    deriving ( Show, Eq, Ord )

instance Read ( IntX w ) where
    readsPrec d r = let [(x, "")] = readsPrec d r
        in [(IntX x, "")]

instance Default ( IntX w ) where
    def = IntX 0

instance Enum ( IntX w ) where
    toEnum = IntX
    fromEnum (IntX x) = x

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

instance WithX (IntX w) (IntX w)

instance ( KnownNat w ) => Val ( IntX w ) where
    showTypeOf p = "IntX" ++ show (valueWidth p)
    valueWidth _ = natVal (Proxy :: Proxy w)
    scalingFactorPower _ = 0
    verilogInt (IntX x) = x



-- FX m b where
--   m the number of magnitude or integer bits
--   b the total number of bits
--
-- fxm.b: The "fx" prefix is similar to the above, but uses the word length as
-- the second item in the dotted pair. For example, fx1.16 describes a number
-- with 1 magnitude bit and 15 fractional bits in a 16 bit word.[3]
newtype FX (m :: Nat) (b :: Nat) = FX Int
    deriving ( Eq, Ord )

fxMB x
    = let
        proxyM :: FX m b -> Proxy m
        proxyM _ = Proxy
        proxyB :: FX m b -> Proxy b
        proxyB _ = Proxy
    in (natVal $ proxyM x, natVal $ proxyB x)


scalingFactor x = 2 ** fromIntegral (scalingFactorPower x)

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
        = let res = FX (x * truncate (scalingFactor res :: Double))
        in res
    fromEnum t@(FX x) = truncate (fromIntegral x / scalingFactor t :: Double)

instance ( KnownNat m, KnownNat b ) => Num ( FX m b ) where
    ( FX a ) + ( FX b ) = FX ( a + b )
    t@( FX a ) * ( FX b ) = FX ( truncate (fromIntegral (a * b) / scalingFactor t :: Double) )
    abs ( FX a ) = FX $ abs a
    signum ( FX a ) = toEnum $ signum a
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


instance WithX ( FX m b ) ( FX m b )

instance ( KnownNat m, KnownNat b ) => Val ( FX m b ) where
    showTypeOf _ = let
            (m, b) = fxMB (undefined :: FX m b)
        in "FX" ++ show m ++ "_" ++ show b
    valueWidth _ = natVal (Proxy :: Proxy b)
    scalingFactorPower x
        = let
            (m, b) = fxMB x
        in b - m
    verilogInt (FX x) = x
