{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NITTA.Types
  ( module NITTA.Types.Base
  , module NITTA.Types.Network
  , module NITTA.Types.Poly
  , IntX(..)
  , FX(..)
  , Val(..)
  , widthX
  , scalingFactorPower
  , scalingFactor
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


class ( Typeable x, Read x ) => Val x where
    showTypeOf :: Proxy x -> String
    valueWidth :: Proxy x -> Integer

widthX pu = valueWidth $ proxyX pu


-- for Int
instance Val Int where
    showTypeOf _ = "Int"
    valueWidth _ = 32

-- for Integer
instance Val Integer where
    showTypeOf _ = "Integer"
    valueWidth _ = 32

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

instance ( KnownNat w ) => Val ( IntX w ) where
    showTypeOf p = "IntX" ++ show (valueWidth p)
    valueWidth p = natVal (Proxy :: Proxy w)


-- FX m b where
--   m the number of magnitude or integer bits
--   b the total number of bits
--
-- fxm.b: The "fx" prefix is similar to the above, but uses the word length as
-- the second item in the dotted pair. For example, fx1.16 describes a number
-- with 1 magnitude bit and 15 fractional bits in a 16 bit word.[3]
newtype FX (m :: Nat) (b :: Nat) = FX Int
    deriving ( Eq, Ord )

scalingFactorPower x
    = let
        proxyM :: ( KnownNat m ) => FX m b -> Proxy m
        proxyM _ = Proxy
        m = natVal $ proxyM x
        proxyB :: ( KnownNat m ) => FX m b -> Proxy b
        proxyB _ = Proxy
        b = natVal $ proxyB x
    in b - m

scalingFactor x = 2 ** fromIntegral (scalingFactorPower x)

instance ( KnownNat m, KnownNat b ) => Show ( FX m b ) where
    show t@(FX x) = show (fromIntegral x / scalingFactor t)

instance ( KnownNat m, KnownNat b ) => Read ( FX m b ) where
    readsPrec d r
        = let
            [(x, "")] = readsPrec d r
            result = FX $ truncate (x * scalingFactor result)
        in [(result, "")]

instance Default ( FX m b ) where
    def = FX 0

instance ( KnownNat m, KnownNat b ) => Enum ( FX m b ) where
    toEnum x
        = let res = FX (x * truncate (scalingFactor res))
        in res
    fromEnum t@(FX x) = truncate (fromIntegral x / scalingFactor t)

instance ( KnownNat m, KnownNat b ) => Num ( FX m b ) where
    ( FX a ) + ( FX b ) = FX ( a + b )
    t@( FX a ) * ( FX b ) = FX ( truncate (fromIntegral (a * b) / scalingFactor t) )
    abs ( FX a ) = FX $ abs a
    signum ( FX a ) = toEnum $ signum a
    fromInteger x
        = let res = FX $ fromIntegral (x * truncate (scalingFactor res))
        in res
    negate ( FX a ) = FX $ negate a

-- instance ( KnownNat m, KnownNat b ) => Real ( FX m b ) where
--     toRational ( FX x ) = toRational x

-- instance ( KnownNat m, KnownNat b ) => Integral ( FX m b ) where
--     toInteger ( FX x ) = toInteger x
--     ( FX a ) `quotRem` ( FX b )
--         = let (a', b') =  a `quotRem` b
--         in ( FX a', FX b' )

-- instance Bits ( FX m b ) where
--     ( FX a ) .&. ( FX b ) = FX ( a .&. b )
--     ( FX a ) .|. ( FX b ) = FX ( a .|. b )
--     ( FX a ) `xor` ( FX b ) = FX ( a `xor` b )
--     complement ( FX a ) = FX $ complement a
--     shift ( FX a ) i = FX $ shift a i
--     rotate ( FX a ) i = FX $ rotate a i

--     bitSize ( FX a ) = fromMaybe undefined $ bitSizeMaybe a
--     bitSizeMaybe ( FX a ) = bitSizeMaybe a
--     isSigned ( FX a ) = isSigned a
--     testBit ( FX a ) = testBit a
--     bit i = FX $ bit i
--     popCount ( FX a ) = popCount a

instance ( KnownNat m, KnownNat b ) => Val ( FX m b ) where
    showTypeOf p = "FX" ++ show (valueWidth p)
    valueWidth p = natVal (Proxy :: Proxy b)


-- transformToFixPoint algArithmetic x
--         | IntRepr               <- rt = maybe (Left "Float values is unsupported") checkInt $ readMaybe x
--         | (DecimalFixedPoint n) <- rt = maybe (readDouble 10 n x) (checkInt . (* 10^n)) $ readMaybe x
--         | (BinaryFixedPoint  n) <- rt = maybe (readDouble 2  n x) (checkInt . (* 2 ^n)) $ readMaybe x
--     where
--         rt             = reprType algArithmetic
--         maxNum         = 2 ^ valueWidth algArithmetic - 1
--         minNum         = bool 0 (-maxNum - 1) $ isValueSigned algArithmetic
--         readDouble t n = checkInt . fst . properFraction . (* t^n) . (read :: String -> Double)
--         checkInt v     | v <= maxNum && v >= minNum = Right $ fromInteger v
--                        | otherwise                  = Left  $ unpack [qq|The value is outside the allowed limits [$minNum, $maxNum]: $v ($x)|]
