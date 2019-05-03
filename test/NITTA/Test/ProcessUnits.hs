{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-partial-type-signatures #-}

{-|
Module      : NITTA.Test.ProcessUnits
Description :
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Test.ProcessUnits
    ( processUnitTests
    ) where

import           Data.Atomics.Counter          (incrCounter)
import           Data.Default
import qualified Data.Map                      as M
import           Data.Proxy
import           Data.Set                      (difference, elems, empty,
                                                fromList, intersection, union)
import           Debug.Trace
import           NITTA.Functions
import qualified NITTA.Functions               as F
import           NITTA.Model                   (endpointOptionToDecision)
import           NITTA.ProcessUnits.Fram
import           NITTA.ProcessUnits.Multiplier
import           NITTA.Project
import           NITTA.Test.Functions          ()
import           NITTA.Test.LuaFrontend
import           NITTA.Test.Microarchitectures
import           NITTA.Types
import           NITTA.Types.Project
import           NITTA.Utils
import           System.FilePath.Posix         (joinPath)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty                    (TestTree, testGroup)
import           Test.Tasty.HUnit              (testCase, (@?))
import           Test.Tasty.QuickCheck         (Gen, arbitrary, testProperty)
import           Test.Tasty.TH
import           Text.InterpolatedString.Perl6 (qc)



test_fram =
    [ testCase "reg_out" $ unitTestBench "reg_out" proxy
        (Just def{ cntxVars=M.fromList [("aa", [42]), ("ac", [0x1003])] })
        [ F.reg "aa" ["ab"]
        , F.framOutput 9 "ac"
        ]
    , testCase "reg_constant" $ unitTestBench "reg_constant" proxy
        (Just def{ cntxVars=M.fromList [("dzw", [975])] })
        [ F.reg "dzw" ["act","mqt"]
        , F.constant 11 ["ovj"]
        ]
    , testProperty "isFinished" $ fmap isFinished gen
    , testProperty "coSimulation" $ fmap (coSimulation "prop_simulation_fram") $ inputsGen =<< gen
    ]
    where
        proxy = Proxy :: Proxy (Fram String Int Int)
        gen = processAlgOnEndpointGen (def :: (Fram String Int Int)) $ algorithmGen
            [ fmap F (arbitrary :: Gen (Constant _ _))
            , fmap F (arbitrary :: Gen (FramInput _ _))
            , fmap F (arbitrary :: Gen (FramOutput _ _))
            , fmap F (arbitrary :: Gen (Loop _ _))
            , fmap F (arbitrary :: Gen (Reg _ _))
            ]



test_shift =
    [ algTestCase "left_right" march
        [ F.loop 16 "g1" ["f1"]
        , F.shiftL "f1" ["g1"]
        , F.loop 16 "g2" ["f2"]
        , F.shiftR "f2" ["g2"]
        ]
    ]



test_multiplier =
    [ algTestCase "simple_mul" march
        [ F.constant 2 ["a"]
        , F.loop 1 "c" ["b"]
        , F.multiply "a" "b" ["c"]

        , F.constant 3 ["x"]
        , F.loop 1 "z" ["y"]
        , F.multiply "y" "x" ["z"]
        ]
    , testProperty "isFinished" $ isFinished <$> gen
    , testProperty "coSimulation" $ fmap (coSimulation "prop_simulation_multiplier") $ inputsGen =<< gen
    ]
    where
        gen = processAlgOnEndpointGen (multiplier True :: Multiplier String Int Int) $ algorithmGen
            [ fmap F (arbitrary :: Gen (Multiply _ _))
            ]



test_divider =
    [ algTestCase "simple_div" march
        [ F.constant 100 ["a"]
        , F.loop 2 "e" ["b"]
        , F.division "a" "b" ["c"] ["d"]
        , F.add "c" "d" ["e"]

        , F.constant 200 ["a1"]
        , F.loop 2 "e1" ["b1"]
        , F.division "a1" "b1" ["c1"] ["d1"]
        , F.add "c1" "d1" ["e1"]
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
    -- , testProperty "isFinished" $ isFinished <$> dividerGen
    -- , testProperty "coSimulation" $ fmap (coSimulation "prop_simulation_divider") $ inputsGen =<< dividerGen
    ]
    -- where
        -- _gen = processAlgOnEndpointGen (divider 4 True :: Divider String Int Int)
        --     [ fmap F (arbitrary :: Gen (Division _ _))
        --     ]



processUnitTests :: TestTree
processUnitTests = $(testGroupGenerator)


-----------------------------------------------------------


algorithmGen fGenList = fmap onlyUniqueVar $ listOf1 $ oneof fGenList
    where
        onlyUniqueVar = snd . foldl (\(used, fbs) fb -> let vs = variables fb
                                                in if null (vs `intersection` used)
                                                    then ( vs `union` used, fb:fbs )
                                                    else ( used, fbs ) )
                            (empty, [])



data Opt a b = EndpointOpt a | BindOpt b


-- |Автоматическое планирование вычислительного процесса, в рамках которого решения принимаются
-- случайным образом. В случае если какой-либо функциональный блок не может быть привязан к
-- вычислительному блоку (например по причине закончившихся внутренних ресурсов), то он просто
-- отбрасывается.
processAlgOnEndpointGen pu0 algGen = do
        alg <- algGen
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



-- |Генерация случайных входных данных для заданного алгорима.
inputsGen (pu, fPassed) = do
    values <- infiniteListOf $ choose (0, 1000 :: Int)
    let vars = elems $ unionsMap inputs fPassed
    let varValues = M.fromList $ zip vars (map (:[]) values)
    return (pu, fPassed, Just def{ cntxVars=varValues })


-- |Проверка вычислительного блока на соответсвие работы аппаратной реализации и его модельного
-- поведения.
coSimulation n (pu, _fbs, values) = monadicIO $ do
    i <- run $ incrCounter 1 externalTestCntr
    let path = joinPath ["hdl", "gen", n ++ "_" ++ show i]
    res <- run $ writeAndRunTestbench $ Project n "../.." path pu values
    assert $ tbStatus res


-- |Формальнаяа проверка полноты выполнения работы вычислительного блока.
isFinished (pu, fPassed)
    = let
        p = process pu
        processVars = unionsMap variables $ getEndpoints p
        algVars = unionsMap variables $ elems fbs
        fbs = fromList fPassed
        processFBs = fromList $ getFBs p
    in processFBs == fbs -- функции в алгоритме соответствуют выполненным функциям в процессе
        && processVars == algVars -- пересылаемые данные в алгоритме соответствуют пересылаемым данным в процессе
        && null (options endpointDT pu)
        || trace (  "delta vars: " ++ show (algVars `difference` processVars) ++ "\n"
                ++ "fbs: " ++ concatMap (\fb -> (if fb `elem` processFBs then "+" else "-") ++ "\t" ++ show fb ++ "\n" ) fbs ++ "\n"
                ++ "fbs: " ++ show processFBs ++ "\n"
                ++ "algVars: " ++ show algVars ++ "\n"
                ++ "processVars: " ++ show processVars ++ "\n"
                ) False


unitTestBench title proxy cntx alg = do
    let
        lib = joinPath ["..", ".."]
        wd = joinPath ["hdl", "gen", title]
        pu = bindAllAndNaiveSchedule alg (def `asProxyTypeOf` proxy)
    (tbStatus <$> writeAndRunTestbench (Project title lib wd pu cntx)) @? title



-- |Выполнить привязку списка функциональных блоков к указанному вычислительному блоку и наивным
-- образом спланировать вычислительный процесс.
bindAllAndNaiveSchedule alg pu0 = naiveSchedule $ foldl (flip bind) pu0 alg
    where
        naiveSchedule pu
            | opt : _ <- options endpointDT pu = naiveSchedule $ decision endpointDT pu $ endpointOptionToDecision opt
            | otherwise = pu
