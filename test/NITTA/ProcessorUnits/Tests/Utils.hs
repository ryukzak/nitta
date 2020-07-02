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
{-# OPTIONS -Wall -Wcompat -fno-warn-redundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}

{-|
Module      : NITTA.ProcessorUnits.Tests.Utils
Description : Utils for processor unit testing
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.ProcessorUnits.Tests.Utils
    ( puCoSimTestCase
    , nittaCoSimTestCase

    , finitePUSynthesisProp
    , puCoSimProp

    , algGen
    ) where

import           Control.Monad                   (void)
import           Data.Atomics.Counter            (incrCounter)
import           Data.CallStack
import           Data.Default
import           Data.List                       (delete)
import qualified Data.Map                        as M
import           Data.Set                        (difference, elems, empty,
                                                  fromList, intersection, union)
import           Debug.Trace
import           NITTA.Intermediate.Simulation
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Networks.Types
import           NITTA.Model.Problems            hiding (Bind, Refactor)
import           NITTA.Model.ProcessorUnits.Time
import           NITTA.Model.TargetSystem
import           NITTA.Project
import qualified NITTA.Project                   as N
import           NITTA.TargetSynthesis
import           NITTA.Test.FunctionSimulation   ()
import           NITTA.Test.Microarchitectures
import           NITTA.Utils
import           System.FilePath.Posix           (joinPath)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty                      (TestTree)
import           Test.Tasty.HUnit                (testCase, (@?))
import           Test.Tasty.QuickCheck           (testProperty)


-- *Test cases

-- |Execute co-simulation test for the specific process unit
puCoSimTestCase ::
    ( HasCallStack
    , PUClasses (pu String x Int) String x Int
    , WithFunctions (pu String x Int) ( F String x )
    , N.Testable (pu String x Int) String x
    ) => String -> pu String x Int -> [(String, x)] -> [F String x] -> TestTree
puCoSimTestCase name u cntxCycle alg
    = testCase name $ do
        let mname = toModuleName name
            prj = Project
                { pName=mname
                , pLibPath=joinPath ["..", "..", "hdl"]
                , pPath=joinPath ["gen", mname]
                , pUnit=naiveSynthesis alg u
                , pTestCntx=simulateAlg (CycleCntx $ M.fromList cntxCycle) [] alg
                }
        (tbStatus <$> writeAndRunTestbench prj) @? name


-- |Bind all functions to processor unit and synthesis process with endpoint
-- decisions.
naiveSynthesis alg u0 = naiveSynthesis' $ foldl (flip bind) u0 alg
    where
        naiveSynthesis' u
            | opt : _ <- endpointOptions u
            = naiveSynthesis' $ endpointDecision u $ endpointOptionToDecision opt
            | otherwise = u


-- |Execute co-simulation test for the specific microarchitecture and algorithm
nittaCoSimTestCase ::
    ( HasCallStack
    , Val x, Integral x
    ) => String -> ( BusNetwork String String x Int ) -> [ F String x ] -> TestTree
nittaCoSimTestCase n tMicroArch alg
    = testCase n $ void $ runTargetSynthesis' def
        { tName=n
        , tMicroArch
        , tDFG=fsToDataFlowGraph alg
        }


-- *Properties

-- |Is unit synthesis process complete (by function and variables).
finitePUSynthesisProp name u0 fsGen
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


-- |A computational process of functional (Haskell) and logical (Verilog) simulation
-- should be identical for any correct algorithm.
puCoSimProp name u fsGen
    = testProperty name $ do
        (pUnit, fs) <- processAlgOnEndpointGen u fsGen
        pTestCntx <- initialCycleCntxGen fs
        return $ monadicIO $ do
            i <- run $ incrCounter 1 externalTestCntr
            res <- run $ writeAndRunTestbench Project
                { pName=toModuleName name
                , pLibPath=joinPath ["..", "..", "hdl"]
                , pPath=joinPath ["gen", toModuleName name ++ "_" ++ show i]
                , pUnit
                , pTestCntx
                }
            assert $ tbStatus res


algGen fsGen = fmap avoidDupVariables $ listOf1 $ oneof fsGen
    where
        avoidDupVariables alg
            = snd $ foldl ( \(takenVs, fs) f ->
                    let vs = variables f
                    in if null (vs `intersection` takenVs)
                        then ( vs `union` takenVs, f:fs )
                        else ( takenVs, fs )
                ) (empty, []) alg


initialCycleCntxGen fs = do
    let vs = elems $ unionsMap inputs fs
    xs <- infiniteListOf $ choose (0, 1000)
    let vxs = M.fromList $ zip vs xs
        cntx0 = simulateAlg (CycleCntx vxs) [] fs
    return cntx0


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
