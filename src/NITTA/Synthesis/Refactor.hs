{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.Refactor
Description : Synthesis tree representation
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Refactor (
    module NITTA.Synthesis.BreakLoop,
    module NITTA.Synthesis.OptimizeAccum,
    module NITTA.Synthesis.ResolveDeadlock,
    isRefactor,
) where

import Data.Maybe
import Data.Typeable
import NITTA.Synthesis.BreakLoop
import NITTA.Synthesis.OptimizeAccum
import NITTA.Synthesis.ResolveDeadlock
import NITTA.Synthesis.Types

isRefactor SynthesisDecision{metrics}
    | isJust (cast metrics :: Maybe BreakLoopMetrics) = True
    | isJust (cast metrics :: Maybe OptimizeAccumMetrics) = True
    | isJust (cast metrics :: Maybe ResolveDeadlockMetrics) = True
isRefactor _ = False
