{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -fno-warn-orphans #-}

module NITTA.Model.Problems.ViewHelper (
    DecisionView (RootView),
    IntervalView,
) where

import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.String.Utils as S
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Model.Types
import NITTA.UIBackend.ViewHelperCls
import Numeric.Interval

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
        , targets :: HM.HashMap String (Maybe (String, IntervalView))
        }
    | RefactorDecisionView String
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
                            (v, Just (target, i)) -> (show v, Just (show target, view i))
                            (v, Nothing) -> (show v, Nothing)
                        )
                        $ M.assocs dfTargets
            }

instance (Show v, Show x) => Viewable (Refactor v x) DecisionView where
    view ref = RefactorDecisionView $ show ref

instance ToJSON DecisionView
