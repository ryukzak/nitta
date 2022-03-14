{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.Steps.Refactor
Description : Synthesis tree representation
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Steps.Refactor (
    module NITTA.Synthesis.Steps.BreakLoop,
    module NITTA.Synthesis.Steps.OptimizeAccum,
    module NITTA.Synthesis.Steps.ResolveDeadlock,
    module NITTA.Synthesis.Steps.ConstantFolding,
    isRefactor,
) where

import Data.Maybe
import Data.Typeable
import NITTA.Synthesis.Steps.BreakLoop
import NITTA.Synthesis.Steps.ConstantFolding
import NITTA.Synthesis.Steps.OptimizeAccum
import NITTA.Synthesis.Steps.ResolveDeadlock
import NITTA.Synthesis.Types

isRefactor SynthesisDecision{metrics}
    | isJust (cast metrics :: Maybe BreakLoopMetrics) = True
    | isJust (cast metrics :: Maybe OptimizeAccumMetrics) = True
    | isJust (cast metrics :: Maybe ResolveDeadlockMetrics) = True
    | isJust (cast metrics :: Maybe ConstantFoldingMetrics) = True
isRefactor _ = False
