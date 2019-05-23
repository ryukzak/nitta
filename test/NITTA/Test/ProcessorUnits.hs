{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}

{-|
Module      : NITTA.Test.ProcessorUnits
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Test.ProcessorUnits
    ( processUnitTests
    ) where

import           Data.Atomics.Counter                  (incrCounter)
import           Data.Default
import qualified Data.Map                              as M
import           Data.Set                              (difference, elems,
                                                        empty, fromList,
                                                        intersection, union)
import           Debug.Trace
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Simulation
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.Problems.Types
import           NITTA.Model.ProcessorUnits.Fram
import           NITTA.Model.ProcessorUnits.Multiplier
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Types
import           NITTA.Project.Utils
import           NITTA.Test.FunctionSimulation         ()
import           NITTA.Test.LuaFrontend
import           NITTA.Test.Microarchitectures
import           NITTA.Utils
import           System.FilePath.Posix                 (joinPath)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty                            (TestTree, testGroup)
import           Test.Tasty.HUnit                      (testCase, (@?))
import           Test.Tasty.QuickCheck                 (Gen, arbitrary,
                                                        testProperty)
import           Test.Tasty.TH
import           Text.InterpolatedString.Perl6         (qc)


test_fram =
    [ unitCoSimulationTestCase "register" u [("a", 42)]
        [ reg "a" ["b"]
        ]
    , unitCoSimulationTestCase "constant" u []
        [ constant 11 ["ovj"]
        ]
    , unitCoSimulationTestCase "loop" u [("b", 42)]
        [ loop 10 "b" ["a"]
        ]
    -- not available, because needed self transaction
    -- , unitCoSimulationTestCase "loop_reg" u []
    --     [ reg "a" ["b"]
    --     , loop 10 "b" ["a"]
    --     ]
    , isUnitSynthesisFinishTestProperty "isFinish" u fsGen
    , coSimulationTestProperty "fram_coSimulation" u fsGen
    ]
    where
        u = def :: Fram String Int Int
        fsGen = algGen
            [ fmap F (arbitrary :: Gen (Constant _ _))
            , fmap F (arbitrary :: Gen (Loop _ _))
            , fmap F (arbitrary :: Gen (Reg _ _))
            ]


test_shift =
    [ algTestCase "left_right" march
        [ loop 16 "g1" ["f1"]
        , shiftL "f1" ["g1"]
        , loop 16 "g2" ["f2"]
        , shiftR "f2" ["g2"]
        ]
    ]


test_multiplier =
    [ algTestCase "simple_mul" march
        [ constant 2 ["a"]
        , loop 1 "c" ["b"]
        , multiply "a" "b" ["c"]

        , constant 3 ["x"]
        , loop 1 "z" ["y"]
        , multiply "y" "x" ["z"]
        ]
    , isUnitSynthesisFinishTestProperty "isFinish" u fsGen
    , coSimulationTestProperty "multiplier_coSimulation" u fsGen
    ]
    where
        u = multiplier True :: Multiplier String Int Int
        fsGen = algGen
            [ fmap F (arbitrary :: Gen (Multiply _ _))
            ]


test_divider =
    [ algTestCase "simple_div" march
        [ constant 100 ["a"]
        , loop 2 "e" ["b"]
        , division "a" "b" ["c"] ["d"]
        , add "c" "d" ["e"]

        , constant 200 ["a1"]
        , loop 2 "e1" ["b1"]
        , division "a1" "b1" ["c1"] ["d1"]
        , add "c1" "d1" ["e1"]
        ]
    , intLuaTestCases "single" "single"
        [qc|function f(a)
                a, _b = a / 2
                f(a)
            end
            f(1024)
        |]
    , intLuaTestCases "pair" "pair"
        [qc|function f(a, b)
                a, _ = a / 2
                b, _ = b / 3
                f(a, b)
            end
            f(1024, 1024)
        |]
    -- FIXME: Auto text can't work correctly, because processGen don't take into account the
    -- facts that some variables may go out.
    -- , testProperty "isUnitSynthesisFinish" $ isUnitSynthesisFinish <$> dividerGen
    -- , testProperty "coSimulation" $ fmap (coSimulation "prop_simulation_divider") $ initialCycleCntxGen =<< dividerGen
    ]
    -- where
        -- _gen = processAlgOnEndpointGen (divider 4 True :: Divider String Int Int)
        --     [ fmap F (arbitrary :: Gen (Division _ _))
        --     ]


processUnitTests :: TestTree
processUnitTests = $(testGroupGenerator)


-- *Utils & Property

unitCoSimulationTestCase name u cntxCycle alg
    = testCase name $ do
        let prj = Project
                { pName=name
                , pLibPath=joinPath ["..", ".."]
                , pPath=joinPath ["hdl", "gen", name]
                , pUnit=bindAllAndNaiveSynthesis alg u
                , pTestCntx=simulateAlg (CycleCntx $ M.fromList cntxCycle) [] alg
                }
        (tbStatus <$> writeAndRunTestbench prj) @? name


-- |Bind all functions to processor unit and synthesis process with endpoint
-- decisions.
bindAllAndNaiveSynthesis alg u0 = naiveSynthesis $ foldl (flip bind) u0 alg
    where
        naiveSynthesis u
            | opt : _ <- options endpointDT u
            = naiveSynthesis $ decision endpointDT u $ endpointOptionToDecision opt
            | otherwise = u


-- |Is unit synthesis process complete (by function and variables).
isUnitSynthesisFinishTestProperty name u0 fsGen
    = testProperty name $ do
        (u, fs) <- processAlgOnEndpointGen u0 fsGen
        let
            p = process u
            processedVs = unionsMap variables $ getEndpoints p
            algVs = unionsMap variables fs
            fs' = fromList fs
            processedFs = fromList $ functions p
        return $ fs' == processedFs -- all algorithm functions present in process
            && algVs == processedVs -- all algorithm variables present in process
            && null (options endpointDT u)
            || trace (  "delta vars: " ++ show (algVs `difference` processedVs) ++ "\n"
                    ++ "fs: " ++ concatMap (\fb -> (if fb `elem` processedFs then "+" else "-") ++ "\t" ++ show fb ++ "\n" ) fs' ++ "\n"
                    ++ "fs: " ++ show processedFs ++ "\n"
                    ++ "algVs: " ++ show algVs ++ "\n"
                    ++ "processedVs: " ++ show processedVs ++ "\n"
                    ) False


-- |CoSimulation - generation unit testbench by data from the functional
-- simulation and run it.
coSimulationTestProperty name u fsGen
    = testProperty name $ do
        (pUnit, fs) <- processAlgOnEndpointGen u fsGen
        pTestCntx <- initialCycleCntxGen fs
        return $ monadicIO $ do
            i <- run $ incrCounter 1 externalTestCntr
            res <- run $ writeAndRunTestbench Project
                { pName=name
                , pLibPath=joinPath ["..", ".."]
                , pPath=joinPath ["hdl", "gen", name ++ "_" ++ show i]
                , pUnit
                , pTestCntx
                }
            assert $ tbStatus res

initialCycleCntxGen fs = do
    let vs = elems $ unionsMap inputs fs
    xs <- infiniteListOf $ choose (0, 1000)
    let vxs = M.fromList $ zip vs xs
        cntx0 = simulateAlg (CycleCntx vxs) [] fs
    return cntx0


-- *Generators

algGen fListGen = fmap avoidDupVariables $ listOf1 $ oneof fListGen
    where
        avoidDupVariables alg
            = snd $ foldl ( \(takenVs, fs) f ->
                    let vs = variables f
                    in if null (vs `intersection` takenVs)
                        then ( vs `union` takenVs, f:fs )
                        else ( takenVs, fs )
                ) (empty, []) alg

data Opt a b = EndpointOpt a | BindOpt b


-- |Автоматическое планирование вычислительного процесса, в рамках которого решения принимаются
-- случайным образом. В случае если какой-либо функциональный блок не может быть привязан к
-- вычислительному блоку (например по причине закончившихся внутренних ресурсов), то он просто
-- отбрасывается.
processAlgOnEndpointGen pu0 algGen' = do
        alg <- algGen'
        processAlgOnEndpointGen' pu0 alg []
    where
        processAlgOnEndpointGen' pu fRemain fPassed = do
            let opts = map BindOpt fRemain
                    ++ map EndpointOpt (options endpointDT pu)
            i <- choose (0, length opts - 1)
            if null opts
                then return (pu, fPassed)
                else case opts !! i of
                    BindOpt f ->  let
                            fRemain' = filter (/= f) fRemain
                        in case tryBind f pu of
                                Right pu' -> processAlgOnEndpointGen' pu' fRemain' (f : fPassed)
                                Left _err -> processAlgOnEndpointGen' pu fRemain' fPassed
                    EndpointOpt o -> do
                        d <- fmap endpointOptionToDecision $ endpointGen o
                        let pu' = decision endpointDT pu d
                        processAlgOnEndpointGen' pu' fRemain fPassed
            where
                endpointGen option@EndpointO{ epoRole=Source vs } = do
                    vs' <- suchThat (sublistOf $ elems vs) (not . null)
                    return option{ epoRole=Source $ fromList vs' }
                endpointGen o = return o
