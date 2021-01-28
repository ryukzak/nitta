{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Model.Problems.Dataflow
Description : Sending data between processor units over a network
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Dataflow (
    DataflowSt (..),
    DataflowProblem (..),
    dataflowOption2decision,
) where

import Control.Arrow (second)
import qualified Data.Map.Strict as M
import Data.Maybe
import GHC.Generics
import NITTA.Intermediate.Variable
import NITTA.Model.Types
import Numeric.Interval.NonEmpty

{- |Dataflow option (@tp ~ TimeConstrain t@) or decision (@tp Z Interval t@)
statement. Describe sending data between processor units over a network. Any
'DataflowSt' has implicently linked "NITTA.Model.Problems.Endpoint".
-}
data DataflowSt tag v tp = DataflowSt
    { -- |A source processor unit of data flow transaction, and it's time
      -- constrains which defines when data can be sended.
      dfSource :: (tag, tp)
    , -- |All possible targets of dataflow transaction. If some of targets
      -- can be not available (Nothing).
      dfTargets :: M.Map v (Maybe (tag, tp))
    }
    deriving (Show, Generic)

{- |Implemented for any things, which can send data between processor units over
the network.
-}
class DataflowProblem u tag v t | u -> tag v t where
    dataflowOptions :: u -> [DataflowSt tag v (TimeConstrain t)]
    dataflowDecision :: u -> DataflowSt tag v (Interval t) -> u

-- |Convert dataflow option to decision.
dataflowOption2decision :: (Var v, Time t) => DataflowSt tag v (TimeConstrain t) -> DataflowSt tag v (Interval t)
dataflowOption2decision (DataflowSt src trg) =
    let pushTimeConstrains = map snd $ catMaybes $ M.elems trg
        pullStart = maximum $ map (inf . tcAvailable) $ snd src : pushTimeConstrains
        pullDuration = maximum $ map (inf . tcDuration) $ snd src : pushTimeConstrains
        pullEnd = pullStart + pullDuration - 1
        pushStart = pullStart
        mkEvent (from_, tc) = Just (from_, pushStart ... (pushStart + inf (tcDuration tc) - 1))
        pushs = map (second $ maybe Nothing mkEvent) $ M.assocs trg
     in DataflowSt (fst src, pullStart ... pullEnd) $ M.fromList pushs
