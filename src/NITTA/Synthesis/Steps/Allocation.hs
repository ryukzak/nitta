{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.Steps.Allocation
Description : Implementation of SynthesisDecisionCls that allows to allocate PUs
Copyright   : (c) Aleksandr Penskoi, Vitaliy Zakusilo, 2022
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Steps.Allocation (
    AllocationMetrics (..),
) where

import Data.Aeson (ToJSON)
import Data.Map qualified as M
import GHC.Generics (Generic)
import NITTA.Intermediate.Analysis (ProcessWave (ProcessWave, pwFs))
import NITTA.Model.Networks.Bus (BusNetwork (bnPUPrototypes, bnPus, bnRemains))
import NITTA.Model.Networks.Types (PU (PU, unit), PUPrototype (..))
import NITTA.Model.Problems.Allocation (
    Allocation (Allocation, processUnitTag),
    AllocationProblem (allocationDecision),
 )
import NITTA.Model.ProcessorUnits.Types (
    ParallelismType (..),
    ProcessorUnit (parallelismType),
    UnitTag,
    allowToProcess,
 )
import NITTA.Model.TargetSystem (TargetSystem (TargetSystem, mUnit))
import NITTA.Synthesis.Types (
    SynthesisDecisionCls (..),
    SynthesisState (SynthesisState, numberOfProcessWaves, processWaves, sTarget),
 )

data AllocationMetrics = AllocationMetrics
    { mParallelism :: ParallelismType
    -- ^ PU prototype parallelism type
    , mRelatedRemains :: Float
    -- ^ The number of remaining functions that can be bound to pu
    , mMinPusForRemains :: Float
    -- ^ The minimum number of PUs for each of the remaining functions that can process it
    , mMaxParallels :: Float
    -- ^ The maximum number of functions that could be processed in parallel
    , mAvgParallels :: Float
    -- ^ The number of functions that can be processed in parallel on average
    }
    deriving (Generic)

instance ToJSON AllocationMetrics

instance
    UnitTag tag =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) tag v x t) tag v x t)
        (TargetSystem (BusNetwork tag v x t) tag v x t)
        (Allocation tag)
        (Allocation tag)
        AllocationMetrics
    where
    decisions SynthesisState{sTarget} o = [(o, allocationDecision sTarget o)]

    parameters SynthesisState{sTarget = TargetSystem{mUnit}, processWaves, numberOfProcessWaves} Allocation{processUnitTag} _ =
        let pus = M.elems $ bnPus mUnit
            tmp = bnPUPrototypes mUnit M.! processUnitTag
            mParallelism PUPrototype{pProto} = parallelismType pProto
            canProcessTmp PUPrototype{pProto} f = allowToProcess f pProto
            canProcessPU PU{unit} f = allowToProcess f unit
            relatedRemains = filter (canProcessTmp tmp) $ bnRemains mUnit
            fCountByWaves = map (\ProcessWave{pwFs} -> length $ filter (canProcessTmp tmp) pwFs) processWaves
         in AllocationMetrics
                { mParallelism = mParallelism tmp
                , mRelatedRemains = fromIntegral $ length relatedRemains
                , mMinPusForRemains = fromIntegral $ foldr (min . (\f -> length $ filter (`canProcessPU` f) pus)) (maxBound :: Int) relatedRemains
                , mMaxParallels = fromIntegral $ maximum fCountByWaves
                , mAvgParallels = (fromIntegral (sum fCountByWaves) :: Float) / (fromIntegral numberOfProcessWaves :: Float)
                }

    estimate _ctx _o _d AllocationMetrics{mParallelism, mMinPusForRemains, mAvgParallels}
        | mMinPusForRemains == 0 = 5000
        | mParallelism == Full = -1
        | mParallelism == Pipeline && (mAvgParallels / mMinPusForRemains >= 3) = 4900
        | mAvgParallels / mMinPusForRemains >= 2 = 4900
        | otherwise = -1
