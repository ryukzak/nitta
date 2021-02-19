{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : NITTA.Model.Problems.Refactor.ConstantFolding.Tests
Description :
Copyright   : (c) Daniil Prohorov, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Refactor.ConstantFolding.Tests (
    tests,
) where

import NITTA.Intermediate.DataFlow
import NITTA.Model.Problems.Refactor
import Test.Tasty (testGroup)
import Test.Tasty.HUnit

import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.LuaFrontend.Tests.Utils
import Text.InterpolatedString.Perl6 (qc)

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
                loopRes = loop 1 "e" ["res"]
                resRes = constant 3 ["res"]
             in [a, b, tmp1, res, loopRes] `refactorTo` [loopRes, resRes]
        , testCase "sum 4 numbers" $
            let -- Start algorithm:
                -- a = 1
                -- b = 2
                -- c = 3
                -- d = 4
                -- tmp1 = a + b
                -- tmp2 = c + d
                -- res = tmp1 + tmp2
                --
                -- Result algorithm:
                -- tmp1 = 3
                -- res = tmp1
                a = constant 1 ["a"]
                b = constant 2 ["b"]
                c = constant 3 ["c"]
                d = constant 4 ["d"]
                tmp1 = add "a" "b" ["tmp1"]
                tmp2 = add "c" "d" ["tmp2"]
                summ = add "tmp1" "tmp2" ["sum"]
                res = buffer "sum" ["res"]
                calcTmp = constant 10 ["sum"]
                loopRes = loop 1 "e" ["sum"]
             in [a, b, c, d, tmp1, tmp2, summ, res, loopRes] `refactorTo` [calcTmp, loopRes]
        , luaTestCase
            "Constants folding optimisation"
            [qc|
            function compileTimeEvaluation(i)
                local c = 3
                local v = 1 + 2 + c
                local res = i + v
                compileTimeEvaluation(res)
            end
            compileTimeEvaluation(0)
            |]
        ]
