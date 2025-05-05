{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Model.Time
Description : Types for time description
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Time (
    VarValTime,
    VarValTimeJSON,
    Time,
    TimeConstraint (..),
    TaggedTime (..),
) where

import Data.Aeson (ToJSON, ToJSONKey)
import Data.Default
import Data.Typeable
import GHC.Generics
import NITTA.Intermediate.Types
import Numeric.Interval.NonEmpty

-- | Shortcut for variable ('v'), value ('x') and time ('t') type constrains.
type VarValTime v x t = (Var v, Val x, Time t)

type VarValTimeJSON v x t = (VarValTime v x t, ToJSONKey v, ToJSON v, ToJSON x, ToJSON t)

-- | Shortcut for time type constrain.
type Time t = (Default t, Num t, Bounded t, Ord t, Show t, Typeable t, Enum t, Integral t)

instance ToJSON t => ToJSON (Interval t)

-- | Time constrain for processor activity.
data TimeConstraint t = TimeConstraint
    { tcAvailable :: Interval t
    -- ^ Inclusive interval, when value available to transfer.
    , tcDuration :: Interval t
    -- ^ Inclusive interval, possible for value transfers.
    }
    deriving (Eq, Generic)

instance (Show t, Eq t, Bounded t) => Show (TimeConstraint t) where
    show TimeConstraint{tcAvailable, tcDuration} = showInf tcAvailable ++ " /P " ++ showInf tcDuration
        where
            showInf i =
                let a = inf i
                    b = sup i
                 in if b == maxBound
                        then show a ++ "..INF"
                        else show a ++ ".." ++ show b

instance ToJSON tp => ToJSON (TimeConstraint tp)

-- | Forgoten implementation of tagged time for speculative if statement. Current - dead code.
data TaggedTime tag t = TaggedTime
    { tTag :: Maybe tag
    , tClock :: t
    }
    deriving (Typeable, Generic)

instance Default t => Default (TaggedTime tag t) where
    def = TaggedTime Nothing def

instance (Time t, Show tag) => Show (TaggedTime tag t) where
    show (TaggedTime tag t) = show t ++ maybe "" (("!" ++) . show) tag

instance {-# OVERLAPS #-} Time t => Show (TaggedTime String t) where
    show (TaggedTime tag t) = show t ++ maybe "" ("!" ++) tag

instance Eq t => Eq (TaggedTime tag t) where
    (TaggedTime _ a) == (TaggedTime _ b) = a == b

instance Ord t => Ord (TaggedTime tag t) where
    (TaggedTime _ a) `compare` (TaggedTime _ b) = a `compare` b

instance Enum t => Enum (TaggedTime tag t) where
    toEnum i = TaggedTime Nothing $ toEnum i
    fromEnum (TaggedTime _ i) = fromEnum i

instance Num t => Bounded (TaggedTime tag t) where
    minBound = TaggedTime Nothing 0
    maxBound = TaggedTime Nothing 1000

instance (Num t, Show tag, Eq tag) => Num (TaggedTime tag t) where
    (TaggedTime Nothing a) + (TaggedTime Nothing b) = TaggedTime Nothing (a + b)
    (TaggedTime (Just tag) a) + (TaggedTime Nothing b) = TaggedTime (Just tag) (a + b)
    (TaggedTime Nothing a) + (TaggedTime (Just tag) b) = TaggedTime (Just tag) (a + b)
    (TaggedTime tag_a a) + (TaggedTime tag_b b)
        | tag_a == tag_b = TaggedTime tag_a (a + b)
        | otherwise = error $ "Not equal time tag! " ++ show tag_a ++ " " ++ show tag_b
    fromInteger = TaggedTime Nothing . fromInteger
    negate t = t{tClock = negate $ tClock t}
    (*) = undefined
    abs = undefined
    signum = undefined
