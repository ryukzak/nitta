{- |
Module      : NITTA.Synthesis.Steps
Description : Synthesis tree representation
Copyright   : (c) Aleksandr Penskoi, 2022
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Steps (
    module NITTA.Synthesis.Steps.Allocation,
    module NITTA.Synthesis.Steps.Bind,
    module NITTA.Synthesis.Steps.Dataflow,
    module NITTA.Synthesis.Steps.BreakLoop,
    module NITTA.Synthesis.Steps.OptimizeAccum,
    module NITTA.Synthesis.Steps.OptimizeLut,
    module NITTA.Synthesis.Steps.ResolveDeadlock,
    module NITTA.Synthesis.Steps.ConstantFolding,
    isRefactor,
) where

import Data.Maybe (isJust)
import Data.Typeable (cast)
import NITTA.Synthesis.Steps.Allocation
import NITTA.Synthesis.Steps.Bind
import NITTA.Synthesis.Steps.BreakLoop
import NITTA.Synthesis.Steps.ConstantFolding
import NITTA.Synthesis.Steps.Dataflow
import NITTA.Synthesis.Steps.OptimizeAccum
import NITTA.Synthesis.Steps.OptimizeLut
import NITTA.Synthesis.Steps.ResolveDeadlock
import NITTA.Synthesis.Types (SynthesisDecision (SynthesisDecision, metrics))

isRefactor SynthesisDecision{metrics}
    | isJust (cast metrics :: Maybe BreakLoopMetrics) = True
    | isJust (cast metrics :: Maybe OptimizeAccumMetrics) = True
    | isJust (cast metrics :: Maybe OptimizeLutMetrics) = True
    | isJust (cast metrics :: Maybe ResolveDeadlockMetrics) = True
    | isJust (cast metrics :: Maybe ConstantFoldingMetrics) = True
isRefactor _ = False
