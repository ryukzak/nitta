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
import           Data.List                             (delete)
import qualified Data.Map                              as M
import           Data.Set                              (difference, elems,
                                                        empty, fromList,
                                                        intersection, union)
import           Debug.Trace
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Simulation
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems                  hiding (Bind, Refactor)
import           NITTA.Model.ProcessorUnits.Multiplier
import           NITTA.Model.ProcessorUnits.Time
import           NITTA.Project
import           NITTA.Test.FunctionSimulation         ()
import           NITTA.Test.Microarchitectures
import           NITTA.Utils
import           System.FilePath.Posix                 (joinPath)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty                            (TestTree, testGroup)
import           Test.Tasty.QuickCheck                 (testProperty)
import           Test.Tasty.TH


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
            [ fmap packF (arbitrary :: Gen (Multiply _ _))
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

-- |Is unit synthesis process complete (by function and variables).
isUnitSynthesisFinishTestProperty name u0 fsGen
    = testProperty name $ do
        (u, fs) <- processAlgOnEndpointGen u0 fsGen
        let
            p = process u
            processedVs = unionsMap variables $ getEndpoints p
            algVs = unionsMap variables fs
        return $ algVs == processedVs -- all algorithm variables present in process
            && null (endpointOptions u)
            || trace (unlines
                [ ""
                , "difference between exaceptation and fact: " ++ show (algVs `difference` processedVs)
                , "algorithm variables: " ++ show algVs
                , "processed variables: " ++ show processedVs
                ]) False


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
                , pLibPath=joinPath ["..", "..", "hdl"]
                , pPath=joinPath ["gen", name ++ "_" ++ show i]
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



-- |Automatic synthesis evaluation process with random decisions.
-- If we can't bind function to PU then we skip it.
processAlgOnEndpointGen pu0 algGen' = do
        alg <- algGen'
        algSynthesisGen alg [] pu0

data PUSynthesisTask r f e = Refactor r | Bind f | Transport e

algSynthesisGen fRemain fPassed pu = select tasksList
    where
        tasksList = concat
            [ map Refactor $ refactorOptions pu
            , map Bind fRemain
            , map Transport $ endpointOptions pu
            ]

        select []    = return ( pu, fPassed )
        select tasks = taskPattern =<< elements tasks

        taskPattern (Refactor r) = algSynthesisGen fRemain fPassed $ refactorDecision pu r

        taskPattern (Bind f) = case tryBind f pu of
            (Right pu') -> algSynthesisGen fRemain' (f : fPassed) pu'
            (Left _err) -> algSynthesisGen fRemain' fPassed pu
            where
                fRemain' = delete f fRemain

        taskPattern (Transport e) = do
            d <- endpointOptionToDecision <$> endpointGen e
            let pu' = endpointDecision pu d
            algSynthesisGen fRemain fPassed pu'

        endpointGen option@EndpointSt{ epRole=Source vs } = do
            vs' <- suchThat (sublistOf $ elems vs) (not . null)
            return option{ epRole=Source $ fromList vs' }
        endpointGen o = return o
