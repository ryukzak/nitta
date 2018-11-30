{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module NITTA.Types
  ( module NITTA.Types.Base
  , module NITTA.Types.Network
  , module NITTA.Types.Poly
  , IntX(..)
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

newtype IntX (w :: Nat) = IntX Int
    deriving ( Show, Eq, Ord )

instance Default ( IntX w ) where
    def = IntX 0

instance Num ( IntX w ) where
    ( IntX a ) + ( IntX b ) = IntX ( a + b )
    ( IntX a ) * ( IntX b ) = IntX ( a * b )
    abs ( IntX a ) = IntX $ abs a
    signum ( IntX a ) = IntX $ signum a
    fromInteger a = IntX $ fromInteger a
    negate ( IntX a ) = IntX $ negate a

instance Real ( IntX w ) where
    toRational ( IntX x ) = toRational x

instance Enum ( IntX x ) where
    toEnum = IntX
    fromEnum (IntX x) = x

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



class ( Typeable x, Read x ) => Val x where
    showTypeOf :: Proxy x -> String
    valueWidth :: Proxy x -> Integer


instance Read (IntX w) where
    readsPrec d r = let [(x, "")] = readsPrec d r
        in [(IntX x, "")]

instance Val Int where
    showTypeOf _ = "Int"
    valueWidth _ = 32

instance Val Integer where
    showTypeOf _ = "Integer"
    valueWidth _ = 32

instance ( KnownNat w ) => Val (IntX w) where
    showTypeOf p = "IntX" ++ show (valueWidth p)
    valueWidth p = natVal $ widthProxy p
        where
            widthProxy :: Proxy (IntX w) -> Proxy w
            widthProxy _ = Proxy

widthX pu = valueWidth $ proxyX pu

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
