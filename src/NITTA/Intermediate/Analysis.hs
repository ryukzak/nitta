{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.Intermediate.Analysis
Description : Analysis of the process execution flow
Copyright   : (c) Aleksandr Penskoi, 2022
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

Much of the work in this module focuses on building waves of process execution.
In this case, we call a wave a set of functions that are ready for execution (all input vars are ready to use)
and that can be performed independently of each other.
Example:

Lua code:

@
function sum(a)
    local d = a + 1
    sum(d)
end
sum(0)
@

After building DataFlowGraph, we get the following set of functions:

@
- const(1) = !1#0
- loop(0, d^0#0) = a^0#0
- a^0#0 + !1#0 = d^0#0
@

Const and Loop function can be executed at the first wave because all input variables are ready to use.
After executing first wave a^0#0 and !1#0 will be ready, so we will be able to execute add function.
So the resulting process waves are the following:

@
[
    ProcessWave {
        pwFs = [const(1) = !1#0,loop(0, d^0#0) = a^0#0],
        pwOut = fromList ["!1#0","a^0#0"]
    },
    ProcessWave {
        pwFs = [a^0#0 + !1#0 = d^0#0],
        pwOut = fromList ["d^0#0"]
    }
]
@
-}
module NITTA.Intermediate.Analysis (
    ProcessWave (..),
    buildProcessWaves,
    reorderAlgorithm,
    estimateVarWaves,
) where

import Data.List qualified as L
import Data.Map qualified as M
import Data.Set qualified as S
import GHC.Generics
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.Utils.Base

data ProcessWave v x = ProcessWave
    { pwFs :: [F v x]
    -- ^Functions that can be executed at this wave
    , pwOut :: S.Set v
    -- ^Set of output variables related to the functions from this step
    }
    deriving (Show, Generic)

data Builder v x = Builder
    { pwRemains :: S.Set (F v x)
    -- ^Functions that can be calculated due to lack of ready input values
    , pwIn :: S.Set v
    -- ^Variables that defined at the beginning of the process
    , pwReadyIn :: S.Set v
    -- ^Variables that is ready to be used us inputs
    , pwGraph :: [ProcessWave v x]
    -- ^Resulting process flow
    }
    deriving (Show, Generic)

{- |
Sort functions in order of execution.
Note that some function could be executed in parallel, in this case we save order from the source list.
-}
reorderAlgorithm :: (Var v, Val x) => [F v x] -> [F v x]
reorderAlgorithm alg = concatMap pwFs $ buildProcessWaves [] alg

{- |
Functions can be divided into waves of execution.
For each output variable, we define the wave number on which the variable will be defined.
-}
estimateVarWaves :: (Var v, Val x, Num a) => [v] -> [F v x] -> M.Map v a
estimateVarWaves alreadyVars fs = M.fromList $ go 0 $ buildProcessWaves alreadyVars fs
    where
        go n (ProcessWave{pwFs} : pss) = go (n + 1) pss <> [(out, n) | out <- S.toList $ unionsMap outputs pwFs]
        go _ [] = []

-- |Divide function into execution waves.
buildProcessWaves :: (Var v, Val x) => [v] -> [F v x] -> [ProcessWave v x]
buildProcessWaves vars fs =
    let pwIn = S.fromList vars
        (loops, other) = L.partition isLoop fs
        beginning =
            -- Place all loops at the beginning of the algorithm to avoid circular dependencies in functions.
            [ ProcessWave
                { pwFs = loops
                , pwOut = unionsMap outputs loops
                }
            | not (null loops)
            ]
        builder =
            Builder
                { pwRemains = S.fromList other
                , pwGraph = beginning
                , pwIn
                , pwReadyIn = pwIn `S.union` unionsMap pwOut beginning
                }
     in pwGraph $ execBuilder builder 0

execBuilder :: Ord v => Builder v x -> Int -> Builder v x
execBuilder builder@Builder{pwRemains} prev
    | S.null pwRemains = builder
    | S.size pwRemains == prev = error "Process waves construction stuck in a loop"
    | otherwise = execBuilder (foldl applyRemaining builder pwRemains) $ S.size pwRemains

applyRemaining :: Ord v => Builder v x -> F v x -> Builder v x
applyRemaining builder@Builder{pwRemains, pwGraph, pwIn, pwReadyIn} func =
    let fIn = inputs func
        fOut = outputs func
        pendingIn = S.difference fIn pwReadyIn
     in if not $ null pendingIn
            then builder
            else
                builder
                    { pwReadyIn = S.union fOut pwReadyIn
                    , pwGraph = insertF func (S.difference fIn pwIn) fOut pwGraph
                    , pwRemains = S.delete func pwRemains
                    }

insertF :: Ord v => F v x -> S.Set v -> S.Set v -> [ProcessWave v x] -> [ProcessWave v x]
insertF f fIn fOut []
    | null fIn = [ProcessWave{pwFs = [f], pwOut = fOut}]
    | otherwise = error "Cannot calculate process wave for the function"
insertF f fIn fOut (ps@ProcessWave{pwFs, pwOut} : pss)
    | null fIn = ps{pwFs = f : pwFs, pwOut = S.union fOut pwOut} : pss
    | otherwise = ps : insertF f (S.difference fIn pwOut) fOut pss
