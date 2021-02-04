{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -fno-warn-orphans #-}

module NITTA.Model.Problems.ViewHelper (
    DecisionView (..),
    IntervalView (..),
) where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.String.Utils as S
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Model.Types
import NITTA.UIBackend.ViewHelperCls
import Numeric.Interval.NonEmpty

newtype IntervalView = IntervalView String
    deriving (Generic)

instance (Time t) => Viewable (Interval t) IntervalView where
    view = IntervalView . S.replace (show (maxBound :: t)) "∞" . show

instance ToJSON IntervalView

data DecisionView
    = RootView
    | BindDecisionView
        { function :: FView
        , pu :: String
        }
    | DataflowDecisionView
        { source :: String
        , targets :: HM.HashMap String (Maybe (String, Interval Int))
        }
    | BreakLoopView
        { value :: String
        , outputs :: [String]
        , input :: String
        }
    | OptimizeAccumView
        { old :: [FView]
        , new :: [FView]
        }
    | ResolveDeadlockView
        { newBuffer :: String
        , changeset :: String
        }
    deriving (Generic)

instance (UnitTag tag) => Viewable (Bind tag v x) DecisionView where
    view (Bind f pu) =
        BindDecisionView
            { function = view f
            , pu = showNoQ pu
            }

instance (UnitTag tag, Var v, Time t) => Viewable (DataflowSt tag v (Interval t)) DecisionView where
    view DataflowSt{dfSource = (source, _st), dfTargets} =
        DataflowDecisionView
            { source = showNoQ source
            , targets =
                HM.fromList $
                    map
                        ( \case
                            (v, Just (target, i)) -> (showNoQ v, Just (showNoQ target, fromEnum (sup i) ... fromEnum (inf i)))
                            (v, Nothing) -> (showNoQ v, Nothing)
                        )
                        $ M.assocs dfTargets
            }

instance (Show v, Show x) => Viewable (BreakLoop v x) DecisionView where
    view BreakLoop{loopX, loopO, loopI} =
        BreakLoopView
            { value = showNoQ loopX
            , outputs = map showNoQ $ S.elems loopO
            , input = showNoQ loopI
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
            { newBuffer = showNoQ newBuffer
            , changeset = showNoQ changeset
            }

instance ToJSON DecisionView

showNoQ :: (Show a) => a -> String
showNoQ = S.replace "\"" "" . show
