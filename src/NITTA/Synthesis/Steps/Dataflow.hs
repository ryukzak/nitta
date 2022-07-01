{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.Steps.Dataflow
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Steps.Dataflow (
    DataflowMetrics (..),
) where

import Data.Aeson (ToJSON)
import Data.Set qualified as S
import GHC.Generics (Generic)
import NITTA.Intermediate.Analysis (ProcessWave (..))
import NITTA.Intermediate.Types (
    Function (inputs),
    Variables (variables),
    WithFunctions (functions),
 )
import NITTA.Model.Networks.Bus (BusNetwork)
import NITTA.Model.Problems.Dataflow (
    DataflowProblem (dataflowDecision),
    DataflowSt (..),
    dataflowOption2decision,
 )
import NITTA.Model.Problems.Endpoint (EndpointSt (epAt))
import NITTA.Model.ProcessorUnits.Types (UnitTag)
import NITTA.Model.TargetSystem (TargetSystem (mUnit))
import NITTA.Model.Time (TimeConstraint (..), VarValTime)
import NITTA.Synthesis.Types (
    SynthesisDecisionCls (..),
    SynthesisState (
        SynthesisState,
        numberOfDataflowOptions,
        processWaves,
        sTarget,
        transferableVars
    ),
    (<?>),
 )
import NITTA.Utils.Base (unionsMap)
import Numeric.Interval.NonEmpty (Interval, inf, sup)

data DataflowMetrics = DataflowMetrics
    { pWaitTime :: Float
    , pRestrictedTime :: Bool
    , pNotTransferableInputs :: [Float]
    -- ^number of variables, which is not transferable for affected
    -- functions.
    , pFirstWaveOfTargetUse :: Float
    -- ^number of the first wave in which one of the target variables is used
    }
    deriving (Generic)

instance ToJSON DataflowMetrics

instance
    (UnitTag tag, VarValTime v x t) =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) tag v x t) tag v x t)
        (TargetSystem (BusNetwork tag v x t) tag v x t)
        (DataflowSt tag v (TimeConstraint t))
        (DataflowSt tag v (Interval t))
        DataflowMetrics
    where
    decisions SynthesisState{sTarget} o = let d = dataflowOption2decision o in [(d, dataflowDecision sTarget d)]

    parameters SynthesisState{transferableVars, sTarget, processWaves} DataflowSt{dfSource, dfTargets} _ =
        let TimeConstraint{tcAvailable, tcDuration} = epAt $ snd dfSource
            vs = unionsMap (variables . snd) dfTargets
            lvs = length vs
            waveNum =
                length
                    . takeWhile (\ProcessWave{pwFs} -> lvs == length (vs `S.difference` unionsMap inputs pwFs))
                    $ processWaves
         in DataflowMetrics
                { pWaitTime = fromIntegral (inf tcAvailable)
                , pRestrictedTime = fromEnum (sup tcDuration) /= maxBound
                , pNotTransferableInputs =
                    let fs = functions $ mUnit sTarget
                        affectedFunctions = filter (\f -> not $ null (inputs f `S.intersection` vs)) fs
                        notTransferableVars = map (\f -> inputs f S.\\ transferableVars) affectedFunctions
                     in map (fromIntegral . length) notTransferableVars
                , pFirstWaveOfTargetUse = fromIntegral waveNum :: Float
                }

    estimate
        SynthesisState{numberOfDataflowOptions}
        _o
        _d
        DataflowMetrics
            { pWaitTime
            , pNotTransferableInputs
            , pRestrictedTime
            , pFirstWaveOfTargetUse
            } =
            2000
                + (numberOfDataflowOptions >= threshold)
                <?> 1000
                + pRestrictedTime
                <?> 200
                - sum pNotTransferableInputs
                * 5
                - pWaitTime
                - pFirstWaveOfTargetUse

threshold = 20
