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
    fromText,
    showText,
    readText,
    vsToStringList,
    catchToMaybeIO,
) where

import Control.Exception
import Data.Functor
import Data.List (maximumBy, minimumBy)
import Data.Set (elems, unions)
import Data.String
import Data.String.ToString
import Data.Text qualified as T
import System.Log.Logger (warningM)

unionsMap f lst = unions $ map f lst

oneOf = head . elems

minimumOn f = minimumBy (\a b -> f a `compare` f b)

maximumOn f = maximumBy (\a b -> f a `compare` f b)

toText v = T.pack $ toString v

fromText v = fromString $ T.unpack v

readText t = read $ T.unpack t

showText v = T.pack $ show v

vsToStringList vs = map toString $ elems vs

catchToMaybeIO action =
    catch
        (action <&> Just)
        ( \(e :: IOException) -> do
            warningM "NITTA" ("IO Exception: " <> show e)
            return Nothing
        )
