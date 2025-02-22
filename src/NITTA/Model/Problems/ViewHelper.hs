{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

module NITTA.Model.Problems.ViewHelper (
    DecisionView (..),
    IntervalView (..),
) where

import Data.Aeson
import Data.Bifunctor (Bifunctor (bimap))
import Data.HashMap.Strict qualified as HM
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.UIBackend.ViewHelperCls
import NITTA.Utils
import Numeric.Interval.NonEmpty

newtype IntervalView = IntervalView T.Text
    deriving (Generic)

instance Time t => Viewable (Interval t) IntervalView where
    view = IntervalView . T.replace (showText (maxBound :: t)) "INF" . showText

instance ToJSON IntervalView

data DecisionView
    = RootView
    | SingleBindView
        { function :: FView
        , pu :: T.Text
        }
    | GroupBindView
        { bindGroup :: HM.HashMap T.Text [FView]
        }
    | AllocationView
        { networkTag :: T.Text
        , processUnitTag :: T.Text
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
    | OptimizeLutView
        { lOld :: [FView]
        , lNew :: [FView]
        }
    | ResolveDeadlockView
        { newBuffer :: T.Text
        , changeset :: T.Text
        }
    deriving (Generic)

instance UnitTag tag => Viewable (Bind tag v x) DecisionView where
    view (SingleBind uTag f) =
        SingleBindView
            { function = view f
            , pu = toText uTag
            }
    view GroupBind{bindGroup} = GroupBindView $ HM.fromList $ map (bimap toText (map view)) $ M.assocs bindGroup

instance UnitTag tag => Viewable (Allocation tag) DecisionView where
    view Allocation{networkTag, processUnitTag} =
        AllocationView
            { networkTag = toText networkTag
            , processUnitTag = toText processUnitTag
            }

instance (UnitTag tag, Var v, Time t) => Viewable (DataflowSt tag v (Interval t)) DecisionView where
    view DataflowSt{dfSource, dfTargets} =
        DataflowDecisionView
            { source = view' dfSource
            , targets = map view' dfTargets
            }
        where
            view' = bimap toText epdView
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
            { value = showText loopX
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

instance Viewable (OptimizeLut v x) DecisionView where
    view OptimizeLut{rOld, rNew} =
        OptimizeLutView
            { lOld = map view rOld
            , lNew = map view rNew
            }

instance Var v => Viewable (ResolveDeadlock v x) DecisionView where
    view ResolveDeadlock{newBuffer, changeset} =
        ResolveDeadlockView
            { newBuffer = showText newBuffer
            , changeset = showText changeset
            }

instance ToJSON DecisionView
