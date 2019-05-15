{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Model.Types
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Types
    ( VarValTime, Time
    , TimeConstrain(..), TaggedTime(..)
    ) where

import           Data.Default
import           Data.Typeable
import           GHC.Generics
import           NITTA.Intermediate.Types
import           Numeric.Interval


-- |Shortcut for variable, value and time type constrains.
type VarValTime v x t = ( Var v, Val x, Time t )


-- *Time

-- |Type class for time description.
type Time t = ( Default t, Num t, Bounded t, Ord t, Show t, Typeable t, Enum t, Integral t )


-- | Описание временных ограничений на активности (Ativity). Используется при описании доступных
-- опций для планирования вычислительного процесса.
data TimeConstrain t
    = TimeConstrain
        { tcAvailable :: Interval t -- ^ Замкнутый интервал, в рамках которого можно выполнить активность.
        , tcDuration  :: Interval t -- ^ Замкнутый интервал допустимой длительности активности.
        } deriving ( Eq )

instance ( Show t, Eq t, Bounded t ) => Show (TimeConstrain t) where
    show TimeConstrain{ tcAvailable, tcDuration } = showInf tcAvailable ++ " /P " ++ showInf tcDuration
        where
            showInf i = let
                    a = inf i
                    b = sup i
                in if b == maxBound
                    then show a ++ "..∞"
                    else show a ++ ".." ++ show b

-- | Изначально, для описания времени использовался тип Int. Время отсчитывалось с 0, было линейным
-- и совпадало с адресами памяти команд. К сожалению, это никуда не годится в случае если:
--
--     1. В вычислительном процессе присутствуют циклы и ветвления.
--     2. Вычислитель может включаться, выключаться...
--
-- По этому было принято решение добавить тег, идентифицирующий к какой ветку развития
-- вычислительного процесса относится данная точка.
--
-- TODO: Убрать Maybe из описания тега. На самом деле тег есть всегда, вопрос лишь в том какой тег по
-- умолчению и как ими оперировать. Также есть вопрос о том, как быть с арфметикой при разных тегах.
data TaggedTime tag t
    = TaggedTime
    { tag   :: Maybe tag -- ^ Идентификатор ветки вычислительного процесса.
    , clock :: t
    }
    deriving ( Typeable, Generic )

instance ( Default t ) => Default (TaggedTime tag t) where
    def = TaggedTime Nothing def

instance ( Time t, Show tag ) => Show (TaggedTime tag t) where
    show (TaggedTime tag t) = show t ++ maybe "" (("!" ++) . show) tag

instance {-# OVERLAPS #-} ( Time t ) => Show (TaggedTime String t) where
    show (TaggedTime tag t) = show t ++ maybe "" ("!" ++) tag

instance ( Eq t ) => Eq (TaggedTime tag t) where
    (TaggedTime _ a) == (TaggedTime _ b) = a == b

instance ( Ord t ) => Ord (TaggedTime tag t) where
    (TaggedTime _ a) `compare` (TaggedTime _ b) = a `compare` b

instance ( Enum t ) => Enum (TaggedTime tag t) where
    toEnum i = TaggedTime Nothing $ toEnum i
    fromEnum (TaggedTime _ i) = fromEnum i

instance ( Num t ) => Bounded (TaggedTime tag t) where
    minBound = TaggedTime Nothing 0
    maxBound = TaggedTime Nothing 1000

instance ( Num t, Show tag, Eq tag ) => Num (TaggedTime tag t) where
    (TaggedTime Nothing a) + (TaggedTime Nothing b) = TaggedTime Nothing (a + b)
    (TaggedTime (Just tag) a) + (TaggedTime Nothing b) = TaggedTime (Just tag) (a + b)
    (TaggedTime Nothing a) + (TaggedTime (Just tag) b) = TaggedTime (Just tag) (a + b)
    (TaggedTime tag_a a) + (TaggedTime tag_b b)
        | tag_a == tag_b = TaggedTime tag_a (a + b)
        | otherwise = error $ "Not equal time tag! " ++ show tag_a ++ " " ++ show tag_b
    fromInteger = TaggedTime Nothing . fromInteger
    negate t = t{ clock=negate $ clock t }
    (*) = undefined
    abs = undefined
    signum = undefined
