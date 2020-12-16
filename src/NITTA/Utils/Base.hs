{- FOURMOLU_DISABLE -}
{-|
Module      : NITTA.Utils.Base
Description : Utils for external libraries
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Utils.Base
    ( unionsMap
    , oneOf
    , minimumOn, maximumOn
    ) where

import           Data.List ( maximumBy, minimumBy )
import           Data.Set ( elems, unions )


unionsMap f lst = unions $ map f lst

oneOf = head . elems

minimumOn f = minimumBy (\a b -> f a `compare` f b)

maximumOn f = maximumBy (\a b -> f a `compare` f b)
