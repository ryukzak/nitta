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
    | ConstantFoldingView
        { cRefOld :: [FView]
        , cRefNew :: [FView]
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
                        Source vs -> Source $ S.map toText vs
                        Target v -> Target $ toText v
                    , epAt = fromEnum (sup epAt) ... fromEnum (inf epAt)
                    }

instance (Var v, Val x) => Viewable (BreakLoop v x) DecisionView where
    view BreakLoop{loopX, loopO, loopI} =
        BreakLoopView
            { value = T.pack $ show loopX
            , outputs = map toText $ S.elems loopO
            , input = toText loopI
            }

instance Viewable (ConstantFolding v x) DecisionView where
    view ConstantFolding{cRefOld, cRefNew} =
        ConstantFoldingView
            { cRefOld = map view cRefOld
            , cRefNew = map view cRefNew
            }

instance Viewable (OptimizeAccum v x) DecisionView where
    view OptimizeAccum{refOld, refNew} =
        OptimizeAccumView
            { old = map view refOld
            , new = map view refNew
            }

instance (Var v) => Viewable (ResolveDeadlock v x) DecisionView where
    view ResolveDeadlock{newBuffer, changeset} =
        ResolveDeadlockView
            { newBuffer = T.pack $ show newBuffer
            , changeset = T.pack $ show changeset
            }

instance ToJSON DecisionView

toText = T.pack . toString
