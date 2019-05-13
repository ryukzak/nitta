{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.Test.FunctionSimulation
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Test.FunctionSimulation
    ( functionSimulationTests
    ) where

import           Data.Default
import           Data.List        (permutations)
import qualified Data.Map         as M
import           Data.Maybe
import           Data.Set         (fromList, intersection)
import qualified Data.Set         as S
import           NITTA.Functions
import           NITTA.Model
import           NITTA.Types
import           Test.QuickCheck
import           Test.Tasty       (TestTree)
import           Test.Tasty.HUnit
import           Test.Tasty.TH


case_reorderAlgorithm = do
    let f = reorderAlgorithm :: [F String Int] -> [F String Int]
    let l1 = loop 0 "b2" ["a1"      ]
    let l2 = loop 1 "c" ["b1", "b2"]
    let a = add "a1" "b1" ["c"]
    mapM_ (([ l1, l2, a ] @=?) . f) $ permutations [ l1, l2, a ]


case_fibonacci = simulateTest def
        "a1" [ 0, 1, 1, 2, 3, 5, 8 ]
        [ loop 0 "b2" ["a1"     ]
        , loop 1 "c"  ["b1", "b2"]
        , add "a1" "b1" ["c"]
        ]


case_receiveAndSend = simulateTest [("a", [1,2,3,4,5])]
        "c" [ 11, 12, 13, 14, 15 ]
        [ receive ["a"]
        , constant 10 ["b"]
        , add "a" "b" ["c"]
        , send "c"
        ]


functionSimulationTests :: TestTree
functionSimulationTests = $(testGroupGenerator)


-- *Utils

simulateTest received v xs alg = let
        dfg = fsToDataFlowGraph (alg :: [F String Int])
        Cntx{ cntxProcess } = simulateDataFlowGraph def received dfg
        cycles = take (length xs) cntxProcess
        xs' = map (\(CycleCntx c) -> fromMaybe (error $ show c) (c M.!? v)) cycles
    in xs @=? xs'


-- *Orphans instances for Arbitrary

varNameSize = 7
outputVarsGen = O . fromList <$> resize 3 (listOf1 $ vectorOf varNameSize $ elements ['a'..'z'])
inputVarGen = I <$> vectorOf varNameSize (elements ['a'..'z'])

-- TODO: Иногда, может получиться вот такой вот функциональный блок: < "qqq" = "joi" * "joi" >, что
-- необходимо исправить. Не очень ясно как решать эту проблему, ведь у нас везде Set, а дубль
-- происходит с одной стороны.
uniqueVars fb = S.null (inputs fb `intersection` outputs fb)

instance Arbitrary (Loop String Int) where
    arbitrary = suchThat (Loop <$> (X <$> choose (0, 256)) <*> outputVarsGen <*> inputVarGen) uniqueVars

instance Arbitrary (Reg String Int) where
    arbitrary = suchThat (Reg <$> inputVarGen <*> outputVarsGen) uniqueVars

instance Arbitrary (Constant String Int) where
    arbitrary = suchThat (Constant <$> (X <$> choose (10, 16)) <*> outputVarsGen) uniqueVars

instance Arbitrary (Multiply String Int) where
    arbitrary = suchThat (Multiply <$> inputVarGen <*> inputVarGen <*> outputVarsGen) uniqueVars

instance Arbitrary (Division String Int) where
    arbitrary = suchThat (Division <$> inputVarGen <*> inputVarGen <*> outputVarsGen <*> outputVarsGen) uniqueVars
