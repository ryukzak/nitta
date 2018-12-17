{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NITTA.Types
  ( module NITTA.Types.Base
  , module NITTA.Types.Network
  , module NITTA.Types.Poly
  , IntX(..)
  , FixPointX(..)
  , Val(..)
  , widthX
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

instance Read (IntX w) where
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


-- FixPointX width neg_radix_power_of_2
newtype FixPointX (w :: Nat) (r :: Nat) = FixPointX Int
    deriving ( Eq, Ord )

radix x
    = let
        proxy :: ( KnownNat r ) => FixPointX w r -> Proxy r
        proxy _ = Proxy
        radixPower = negate $ fromIntegral $ natVal $ proxy x
    in 2 ** radixPower

instance ( KnownNat r ) => Show ( FixPointX w r) where
    show t@(FixPointX x) = show (fromIntegral x * radix t)

instance ( KnownNat r ) => Read (FixPointX w r) where
    readsPrec d r
        = let
            [(x, "")] = readsPrec d r
            result = FixPointX $ round (x / radix result)
        in [(result, "")]

instance Default ( FixPointX w r ) where
    def = FixPointX 0

instance Enum ( FixPointX w r ) where
    toEnum = FixPointX
    fromEnum (FixPointX x) = x

instance ( KnownNat r ) => Num ( FixPointX w r ) where
    ( FixPointX a ) + ( FixPointX b ) = FixPointX ( a + b )
    t@( FixPointX a ) * ( FixPointX b ) = FixPointX ( round (fromIntegral (a * b) * radix t) )
    abs ( FixPointX a ) = FixPointX $ abs a
    signum ( FixPointX a ) = FixPointX $ signum a
    fromInteger a = FixPointX $ fromInteger a
    negate ( FixPointX a ) = FixPointX $ negate a

instance ( KnownNat r ) => Real ( FixPointX w r ) where
    toRational ( FixPointX x ) = toRational x

instance ( KnownNat r ) => Integral ( FixPointX w r ) where
    toInteger ( FixPointX x ) = toInteger x
    ( FixPointX a ) `quotRem` ( FixPointX b )
        = let (a', b') =  a `quotRem` b
        in ( FixPointX a', FixPointX b' )

instance Bits ( FixPointX w r ) where
    ( FixPointX a ) .&. ( FixPointX b ) = FixPointX ( a .&. b )
    ( FixPointX a ) .|. ( FixPointX b ) = FixPointX ( a .|. b )
    ( FixPointX a ) `xor` ( FixPointX b ) = FixPointX ( a `xor` b )
    complement ( FixPointX a ) = FixPointX $ complement a
    shift ( FixPointX a ) i = FixPointX $ shift a i
    rotate ( FixPointX a ) i = FixPointX $ rotate a i

    bitSize ( FixPointX a ) = fromMaybe undefined $ bitSizeMaybe a
    bitSizeMaybe ( FixPointX a ) = bitSizeMaybe a
    isSigned ( FixPointX a ) = isSigned a
    testBit ( FixPointX a ) = testBit a
    bit i = FixPointX $ bit i
    popCount ( FixPointX a ) = popCount a

instance ( KnownNat w, KnownNat r ) => Val ( FixPointX w r ) where
    showTypeOf p = "FixPointX" ++ show (valueWidth p)
    valueWidth p = natVal (Proxy :: Proxy w)


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
