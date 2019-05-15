{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Model.Problems.Endpoint
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Endpoint
    ( EndpointDT, Option(..), Decision(..), endpointDT
    , EndpointRole(..), (<<), (\\\)
    , endpointOptionToDecision
    ) where

import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Proxy
import qualified Data.Set                   as S
import qualified Data.String.Utils          as S
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Types
import           NITTA.Model.Types
import           Numeric.Interval


-- |Решение об использовании вычислительных блоков в роли источника или пункта назначения для
-- данных. В один момент времени может быть произведена только одна операция (вероятно, это
-- искусственное ограничение, навязанное архитектурой NL3). PU не может самостоятельно принимать
-- решение относительно своих взаимодействий с окружающим миром, он искючительно выполняет сказанные
-- ему операции.
data EndpointDT v t
endpointDT = Proxy :: Proxy EndpointDT

instance DecisionType (EndpointDT v t) where
    data Option (EndpointDT v t)
        = EndpointO
        { epoRole :: EndpointRole v -- ^ Чтение данных из входного регистра PU или запись данных в него.
        , epoAt :: TimeConstrain t -- ^ Временные ограничения на операцию.
        }
    data Decision (EndpointDT v t)
        = EndpointD
        { epdRole :: EndpointRole v -- ^ Выбранная операция для взаимодействия с окружающим миром.
        , epdAt :: Interval t -- ^ Положение операции во времени.
        }

instance Variables (Option (EndpointDT v t)) v where
    variables EndpointO{ epoRole } = variables epoRole
instance Variables (Decision (EndpointDT v t)) v where
    variables EndpointD{ epdRole } = variables epdRole

instance ( Show v, Show t, Eq t, Bounded t ) => Show (Option (EndpointDT v t)) where
    show EndpointO{ epoRole, epoAt } = "?" ++ show epoRole ++ "@(" ++ show epoAt ++ ")"
instance ( Show v, Show t, Eq t, Bounded t ) => Show (Decision (EndpointDT v t)) where
    show EndpointD{ epdRole, epdAt } = "!" ++ show epdRole ++ "@(" ++ show epdAt ++ ")"

instance ( Ord v ) => Patch (Option (EndpointDT v t)) (Diff v) where
    patch diff ep@EndpointO{ epoRole } = ep{ epoRole=patch diff epoRole }
instance ( Ord v ) => Patch (Decision (EndpointDT v t)) (Diff v) where
    patch diff ep@EndpointD{ epdRole } = ep{ epdRole=patch diff epdRole }


-- |Взаимодействие PU с окружением. Подразумевается, что в один момент времени может быть только
-- одно взаимодействие, при этом у PU только один канал для взаимодействия, что в общем то
-- ограничение. В перспективе должно быть расширено для работы с конвейра.

-- TODO: В настоящий момен Source определяет множество вариантов выгрузки переменной. Это
-- неправильно и требует комплексной переработки.
data EndpointRole v
    = Source (S.Set v) -- ^ Выгрузка данных из PU.
    | Target v   -- ^ Загрузка данных в PU.
    deriving ( Eq, Ord )

instance {-# OVERLAPPABLE #-} ( Show v ) => Show (EndpointRole v) where
    show (Source vs) = "Source " ++ S.join "," (map show $ S.elems vs)
    show (Target v)  = "Target " ++ show v

instance {-# OVERLAPS #-} Show (EndpointRole String) where
    show (Source vs) = "Source " ++ S.join "," (S.elems vs)
    show (Target v)  = "Target " ++ v

instance ( Ord v ) => Patch (EndpointRole v) (Diff v) where
    patch Diff{ diffI } (Target v) = Target $ fromMaybe v $ diffI M.!? v
    patch Diff{ diffO } (Source vs)
        = Source $ S.fromList $ map (\v -> fromMaybe v $ diffO M.!? v) $ S.elems vs

instance Variables (EndpointRole v) v where
    variables (Source vs) = vs
    variables (Target v)  = S.singleton v


(Target a) << (Target b) | a == b = True
(Source a) << (Source b)          = all (`S.member` a) b
_        << _                     = False

(Source a) `sourceDifference` (Source b) = Source $ S.difference a b
a `sourceDifference` b = error $ "Can't get sub endpoint for " ++ show a ++ " " ++ show b

(\\\) a b = sourceDifference a b


-- |The simplest way to convert an endpoint synthesis option to a endpoint
-- decision.
endpointOptionToDecision EndpointO{ epoRole, epoAt }
    = let
        a = inf $ tcAvailable epoAt
        -- "-1" - is necessary for reduction transfer time
        b = a + (inf $ tcDuration epoAt) - 1
    in EndpointD epoRole (a ... b)
