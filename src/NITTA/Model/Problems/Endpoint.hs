{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE IncoherentInstances    #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE UndecidableInstances   #-}
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
    ( EndpointOption(..), EndpointDecision(..), EndpointProblem(..)
    , EndpointRole(..), (<<), (\\\)
    , endpointOptionToDecision
    ) where

import qualified Data.Map                 as M
import           Data.Maybe               (fromMaybe)
import qualified Data.Set                 as S
import qualified Data.String.Utils        as S
import           NITTA.Intermediate.Types
import           NITTA.Model.Types
import           Numeric.Interval


data EndpointOption v t
    = EndpointO
        { epoRole :: EndpointRole v -- ^use processor unit as source or target of data
        , epoAt   :: TimeConstrain t
        }

data EndpointDecision v t
    = EndpointD
        { epdRole :: EndpointRole v -- ^use processor unit as source or target of data
        , epdAt   :: Interval t -- ^time of operation
        }

instance Variables (EndpointOption v t) v where
    variables EndpointO{ epoRole } = variables epoRole
instance Variables (EndpointDecision v t) v where
    variables EndpointD{ epdRole } = variables epdRole

instance ( Show v, Show t, Eq t, Bounded t ) => Show (EndpointOption v t) where
    show EndpointO{ epoRole, epoAt } = "?" ++ show epoRole ++ "@(" ++ show epoAt ++ ")"
instance ( Show v, Show t, Eq t, Bounded t ) => Show (EndpointDecision v t) where
    show EndpointD{ epdRole, epdAt } = "!" ++ show epdRole ++ "@(" ++ show epdAt ++ ")"

instance ( Ord v ) => Patch (EndpointOption v t) (Diff v) where
    patch diff ep@EndpointO{ epoRole } = ep{ epoRole=patch diff epoRole }
instance ( Ord v ) => Patch (EndpointDecision v t) (Diff v) where
    patch diff ep@EndpointD{ epdRole } = ep{ epdRole=patch diff epdRole }


class EndpointProblem u v t | u -> v t where
    endpointOptions :: u -> [ EndpointOption v t ]
    endpointDecision :: u -> EndpointDecision v t -> u


data EndpointRole v
    = Source (S.Set v) -- ^get data from PU
    | Target v   -- ^put data to PU
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
        = Source $ S.unions $ map (\v -> fromMaybe (S.singleton v) $ diffO M.!? v) $ S.elems vs

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
