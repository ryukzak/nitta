{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}

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

import Data.Bifunctor
import Data.String.ToString
import GHC.Generics
import NITTA.Intermediate.Variable
import NITTA.Model.Problems.Endpoint
import NITTA.Model.Time
import NITTA.Utils.Base
import Numeric.Interval.NonEmpty

{- |Dataflow option (@tp ~ TimeConstraint t@) or decision (@tp Z Interval t@)
statement. Describe sending data between processor units over a network. Any
'DataflowSt' has implicently linked "NITTA.Model.Problems.Endpoint".
-}
data DataflowSt tag v tp = DataflowSt
    { dfSource :: (tag, EndpointSt v tp)
    -- ^A source processor unit of data flow transaction, and it's time
    -- constrains which defines when data can be sended.
    , dfTargets :: [(tag, EndpointSt v tp)]
    -- ^All possible targets of dataflow transaction.
    }
    deriving (Generic)

instance (ToString tag, Show (EndpointSt v tp)) => Show (DataflowSt tag v tp) where
    show DataflowSt{dfSource, dfTargets} =
        "DataflowSt{ dfSource=" <> show' dfSource <> ", dfTargets=" <> show (map show' dfTargets) <> "}"
        where
            show' (tag, ep) = "(" <> toString tag <> ", " <> show ep <> ")"

instance (Ord v) => Variables (DataflowSt tag v tp) v where
    variables DataflowSt{dfTargets} = unionsMap (variables . snd) dfTargets

{- |Implemented for any things, which can send data between processor units over
the network.
-}
class DataflowProblem u tag v t | u -> tag v t where
    dataflowOptions :: u -> [DataflowSt tag v (TimeConstraint t)]
    dataflowDecision :: u -> DataflowSt tag v (Interval t) -> u

-- |Convert dataflow option to decision.
dataflowOption2decision :: (Time t) => DataflowSt tag v (TimeConstraint t) -> DataflowSt tag v (Interval t)
dataflowOption2decision (DataflowSt (srcTag, srcEp) trgs) =
    let targetsAt = map (epAt . snd) trgs

        srcStart = maximum $ map (inf . tcAvailable) $ epAt srcEp : targetsAt
        srcDuration = maximum $ map (inf . tcDuration) $ epAt srcEp : targetsAt
        srcEnd = srcStart + srcDuration - 1
     in DataflowSt
            { dfSource = (srcTag, setAt (srcStart ... srcEnd) srcEp)
            , dfTargets = map (second (updAt (\tc -> srcStart ... (srcStart + inf (tcDuration tc) - 1)))) trgs
            }
