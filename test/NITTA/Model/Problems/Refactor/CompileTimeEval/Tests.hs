{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : NITTA.Model.Problems.Refactor.CompileTimeEval.Tests
Description :
Copyright   : (c) Daniil Prohorov, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Refactor.CompileTimeEval.Tests (
    tests,
) where

import qualified Data.Set as S
import NITTA.Intermediate.DataFlow
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.Model.Problems.Refactor
import Test.Tasty (testGroup)
import Test.Tasty.HUnit

refactorTo startFs resultFs = dfRefactored @?= dfRes
    where
        df = fsToDataFlowGraph (startFs :: [F String Int])
        dfRes = fsToDataFlowGraph (resultFs :: [F String Int])
        dfRefactored = simpleRefactor df

simpleRefactor dfg =
    case compileTimeEvalOptions dfg of
        [] -> dfg
        (r : _) -> simpleRefactor $ compileTimeEvalDecision dfg r

tests =
    testGroup
        "Refactor problem (Compile time evaluation)"
        [ testCase "simple sum 2 numbers" $
            let -- Start algorithm:
                -- a = 1
                -- b = 2
                -- tmp1 = a + b
                -- res = tmp1
                --
                -- Result algorithm:
                -- tmp1 = 3
                -- res = tmp1
                a = constant 1 ["a"]
                b = constant 2 ["b"]
                tmp1 = add "a" "b" ["tmp1"]
                res = buffer "tmp1" ["res"]
                calcTmp = constant 3 ["tmp1"]
             in [a, b, tmp1, res] `refactorTo` [res, calcTmp]
        ]
