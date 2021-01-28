{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
        { buffer :: String
        , changeset :: String
        }
    deriving (Generic)

instance (UnitTag tag) => Viewable (Bind tag v x) DecisionView where
    view (Bind f pu) =
        BindDecisionView
            { function = view f
            , pu = show pu
            }

instance (UnitTag tag, Var v, Time t) => Viewable (DataflowSt tag v (Interval t)) DecisionView where
    view DataflowSt{dfSource = (source, _st), dfTargets} =
        DataflowDecisionView
            { source = show source
            , targets =
                HM.fromList $
                    map
                        ( \case
                            (v, Just (target, i)) -> (show v, Just (show target, fromEnum (sup i) ... fromEnum (inf i)))
                            (v, Nothing) -> (show v, Nothing)
                        )
                        $ M.assocs dfTargets
            }

instance (Show v, Show x) => Viewable (BreakLoop v x) DecisionView where
    view BreakLoop{loopX, loopO, loopI} =
        BreakLoopView
            { value = show loopX
            , outputs = map show $ S.elems loopO
            , input = show loopI
            }

instance Viewable (OptimizeAccum v x) DecisionView where
    view OptimizeAccum{refOld, refNew} =
        OptimizeAccumView
            { old = map view refOld
            , new = map view refNew
            }

instance (Show v) => Viewable (ResolveDeadlock v x) DecisionView where
    view ResolveDeadlock{buffer, changeset} =
        ResolveDeadlockView
            { buffer = show buffer
            , changeset = show changeset
            }

instance ToJSON DecisionView
