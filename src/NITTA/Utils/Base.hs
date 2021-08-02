{- |
Module      : NITTA.Utils.Base
Description : Utils for external libraries
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Utils.Base (
    unionsMap,
    oneOf,
    minimumOn,
    maximumOn,
    toText,
    showText,
    readText,
    fromText,
    vsToStringList,
) where

import Data.List (maximumBy, minimumBy)
import Data.Set (elems, unions)
import Data.String
import Data.String.ToString
import qualified Data.Text as T

unionsMap f lst = unions $ map f lst

oneOf = head . elems

minimumOn f = minimumBy (\a b -> f a `compare` f b)

maximumOn f = maximumBy (\a b -> f a `compare` f b)

fromText t = fromString $ T.unpack t

toText v = T.pack $ toString v

readText t = read $ T.unpack t

showText v = T.pack $ show v

vsToStringList vs = map toString $ elems vs
