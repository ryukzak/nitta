{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Model.Problems.Endpoint
Description : Isolated processor unit interaction
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Endpoint (
    EndpointSt (..),
    EndpointProblem (..),
    EndpointRole (..),
    endpointOptionToDecision,
) where

import Data.Aeson (ToJSON)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.String.Utils as S
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Types
import Numeric.Interval.NonEmpty

data EndpointSt v tp = EndpointSt
    { -- |use processor unit as source or target of data
      epRole :: EndpointRole v
    , -- |time of operation
      epAt :: tp
    }
    deriving (Generic)

instance Variables (EndpointSt v t) v where
    variables EndpointSt{epRole} = variables epRole

instance (Show v, Time t) => Show (EndpointSt v (TimeConstrain t)) where
    show EndpointSt{epRole, epAt} = "?" ++ show epRole ++ "@(" ++ show epAt ++ ")"
instance (Show v, Time t) => Show (EndpointSt v (Interval t)) where
    show EndpointSt{epRole, epAt} = "!" ++ show epRole ++ "@(" ++ show epAt ++ ")"

instance (Ord v) => Patch (EndpointSt v tp) (Changeset v) where
    patch diff ep@EndpointSt{epRole} = ep{epRole = patch diff epRole}

instance (ToJSON v, ToJSON tp) => ToJSON (EndpointSt v tp)

class EndpointProblem u v t | u -> v t where
    endpointOptions :: u -> [EndpointSt v (TimeConstrain t)]
    endpointDecision :: u -> EndpointSt v (Interval t) -> u

data EndpointRole v
    = -- |get data from PU
      Source (S.Set v)
    | -- |put data to PU
      Target v
    deriving (Eq, Ord, Generic)

instance {-# OVERLAPPABLE #-} (Show v) => Show (EndpointRole v) where
    show (Source vs) = "Source " ++ S.join "," (map show $ S.elems vs)
    show (Target v) = "Target " ++ show v

instance {-# OVERLAPS #-} Show (EndpointRole String) where
    show (Source vs) = "Source " ++ S.join "," (S.elems vs)
    show (Target v) = "Target " ++ v

instance (Ord v) => Patch (EndpointRole v) (Changeset v) where
    patch Changeset{changeI} (Target v) = Target $ fromMaybe v $ changeI M.!? v
    patch Changeset{changeO} (Source vs) =
        Source $ S.unions $ map (\v -> fromMaybe (S.singleton v) $ changeO M.!? v) $ S.elems vs

instance Variables (EndpointRole v) v where
    variables (Source vs) = vs
    variables (Target v) = S.singleton v

instance (ToJSON v) => ToJSON (EndpointRole v)

{- |The simplest way to convert an endpoint synthesis option to a endpoint
decision.
-}
endpointOptionToDecision EndpointSt{epRole, epAt} =
    let a = inf $ tcAvailable epAt
        -- "-1" - is necessary for reduction transfer time
        b = a + inf (tcDuration epAt) - 1
     in EndpointSt epRole (a ... b)
