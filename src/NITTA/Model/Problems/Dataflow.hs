{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import NITTA.Model.Problems.Endpoint
import NITTA.Model.Types
import Numeric.Interval.NonEmpty

{- |Dataflow option (@tp ~ TimeConstrain t@) or decision (@tp Z Interval t@)
statement. Describe sending data between processor units over a network. Any
'DataflowSt' has implicently linked "NITTA.Model.Problems.Endpoint".
-}
data DataflowSt tag v tp = DataflowSt
    { -- |A source processor unit of data flow transaction, and it's time
      -- constrains which defines when data can be sended.
      dfSource :: (tag, EndpointSt v tp)
    , -- |All possible targets of dataflow transaction. If some of targets
      -- can be not available (Nothing).
      dfTargets :: M.Map v (Maybe (tag, EndpointSt v tp))
    }
    deriving (Generic)

deriving instance (Show tag, Show v, Show (EndpointSt v tp)) => Show (DataflowSt tag v tp)

{- |Implemented for any things, which can send data between processor units over
the network.
-}
class DataflowProblem u tag v t | u -> tag v t where
    dataflowOptions :: u -> [DataflowSt tag v (TimeConstrain t)]
    dataflowDecision :: u -> DataflowSt tag v (Interval t) -> u

-- |Convert dataflow option to decision.
dataflowOption2decision :: (Time t) => DataflowSt tag v (TimeConstrain t) -> DataflowSt tag v (Interval t)
dataflowOption2decision (DataflowSt (srcTag, srcEp) trgs) =
    let targetsAt = map (epAt . snd) $ catMaybes $ M.elems trgs

        srcStart = maximum $ map (inf . tcAvailable) $ (epAt srcEp) : targetsAt
        srcDuration = maximum $ map (inf . tcDuration) $ (epAt srcEp) : targetsAt
        srcEnd = srcStart + srcDuration - 1
     in DataflowSt
            { dfSource = (srcTag, setAt (srcStart ... srcEnd) srcEp)
            , dfTargets = M.map (fmap (second (updAt (\tc -> srcStart ... (srcStart + inf (tcDuration tc) - 1))))) trgs
            }
