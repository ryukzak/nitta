{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module NITTA.Types
  ( module NITTA.Types.Base
  , module NITTA.Types.Network
  , module NITTA.Types.Poly
  , IntX(..)
  , ToX(..)
  ) where

import           NITTA.Types.Base
import           NITTA.Types.Network
import           NITTA.Types.Poly

import           GHC.TypeLits

newtype IntX (w :: Nat) = IntX Int
    deriving (Eq)

class ToX x where
    toX :: String -> x

instance ToX Int where
    toX = read

instance ToX Integer where
    toX = read

instance ToX (IntX w) where
    toX = IntX . read

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
