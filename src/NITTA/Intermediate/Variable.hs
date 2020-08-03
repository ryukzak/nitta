{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : NITTA.Intermediate.Variable
Description : Types for variable representation
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Intermediate.Variable
    ( Var, Variables(..), Suffix(..)
    ) where

import           Data.List
import qualified Data.Set      as S
import           Data.Typeable


-- |Variable identifier. Used for simplify type description.
type Var v = ( Typeable v, Ord v, Show v, Suffix v )


-- |Type class of something, which is related to variables.
class Variables a v | a -> v where
    -- |Get all related variables.
    variables :: a -> S.Set v


-- |The type class for variable identifier modifications.
class Suffix v where
    -- |Make a buffered version of the variable. For example:
    --
    -- > "v" -> "v@buf"
    bufferSuffix :: v -> v
    -- |Buffer sequence length of a variable:
    --
    -- > "v" -> 0
    -- > "v@buf" -> 1
    -- > "b@buf@buf" -> 2
    countSuffix :: v -> Int


-- FIXME: unsafe, because can create duplicate variable. Solution options:
--
-- - unsafeIO and counter;
-- - restriction on user's variable names + checking collision for each variable generation.
instance Suffix String where
    bufferSuffix s = s ++ "@buf"
    countSuffix [] = 0
    countSuffix s
        | Just s' <- stripPrefix "@buf" s = 1 + countSuffix s'
        | otherwise = countSuffix $ drop 1 s
