{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

module NITTA.Model.Problems.ViewHelper (
    DecisionView (..),
    IntervalView (..),
) where

import Data.Aeson
import Data.Bifunctor
import qualified Data.Set as S
import Data.String.ToString
import qualified Data.Text as T
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Model.Types
import NITTA.UIBackend.ViewHelperCls
import Numeric.Interval.NonEmpty

newtype IntervalView = IntervalView T.Text
    deriving (Generic)

instance (Time t) => Viewable (Interval t) IntervalView where
    view = IntervalView . T.replace (T.pack $ show (maxBound :: t)) "∞" . T.pack . show

instance ToJSON IntervalView

data DecisionView
    = RootView
    | BindDecisionView
        { function :: FView
        , pu :: T.Text
        }
    | DataflowDecisionView
        { source :: (T.Text, EndpointSt T.Text (Interval Int))
        , targets :: [(T.Text, EndpointSt T.Text (Interval Int))]
        }
    | BreakLoopView
        { value :: T.Text
        , outputs :: [T.Text]
        , input :: T.Text
        }
    | OptimizeAccumView
        { old :: [FView]
        , new :: [FView]
        }
    | ResolveDeadlockView
        { newBuffer :: T.Text
        , changeset :: T.Text
        }
    deriving (Generic)

instance (UnitTag tag) => Viewable (Bind tag v x) DecisionView where
    view (Bind f pu) =
        BindDecisionView
            { function = view f
            , pu = (T.pack . toString) pu
            }

instance (UnitTag tag, Var v, Time t) => Viewable (DataflowSt tag v (Interval t)) DecisionView where
    view DataflowSt{dfSource, dfTargets} =
        DataflowDecisionView
            { source = view' dfSource
            , targets = map view' dfTargets
            }
        where
            view' = bimap (T.pack . toString) epdView
            epdView EndpointSt{epRole, epAt} =
                EndpointSt
                    { epRole = case epRole of
                        Source vs -> Source $ S.map show' vs
                        Target v -> Target $ show' v
                    , epAt = fromEnum (sup epAt) ... fromEnum (inf epAt)
                    }

instance (Show v, Show x) => Viewable (BreakLoop v x) DecisionView where
    view BreakLoop{loopX, loopO, loopI} =
        BreakLoopView
            { value = show' loopX
            , outputs = map show' $ S.elems loopO
            , input = show' loopI
            }

instance Viewable (OptimizeAccum v x) DecisionView where
    view OptimizeAccum{refOld, refNew} =
        OptimizeAccumView
            { old = map view refOld
            , new = map view refNew
            }

instance (Show v) => Viewable (ResolveDeadlock v x) DecisionView where
    view ResolveDeadlock{newBuffer, changeset} =
        ResolveDeadlockView
            { newBuffer = show' newBuffer
            , changeset = show' changeset
            }

instance ToJSON DecisionView

show' = T.replace "\"" "" . T.pack . show
