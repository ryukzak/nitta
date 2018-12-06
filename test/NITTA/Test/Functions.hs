{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

{-
Данный модуль реализует функции для генерации функциональных блоков.
-}
module NITTA.Test.Functions
    ( functionTests
    ) where

import           Data.Default
import           Data.List               (permutations)
import qualified Data.Map                as M
import           Data.Set                (fromList, intersection)
import qualified Data.Set                as S
import           NITTA.Functions
import           NITTA.ProcessUnits.Fram
import           NITTA.Types
import           Test.QuickCheck
import           Test.Tasty              (TestTree)
import           Test.Tasty.HUnit
import           Test.Tasty.TH


framDefSize = frSize (def :: Fram () Int ())
framAddrGen = choose (0, framDefSize - 1)


varNameSize = 7

outputVarsGen = O . fromList <$> resize 3 (listOf1 $ vectorOf varNameSize $ elements ['a'..'z'])
inputVarGen = I <$> vectorOf varNameSize (elements ['a'..'z'])


-- TODO: Иногда, может получиться вот такой вот функциональный блок: < "qqq" = "joi" * "joi" >, что
-- необходимо исправить. Не очень ясно как решать эту проблему, ведь у нас везде Set, а дубль
-- происходит с одной стороны.
uniqueVars fb = S.null (inputs fb `intersection` outputs fb)


instance Arbitrary (FramInput String Int) where
    arbitrary = suchThat (FramInput <$> framAddrGen <*> outputVarsGen) uniqueVars

instance Arbitrary (FramOutput String Int) where
    arbitrary = suchThat (FramOutput <$> framAddrGen <*> inputVarGen) uniqueVars

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


case_reorderAlgorithm = do
    let f = reorderAlgorithm :: [F String Int] -> [F String Int]
    let l1 = loop 0 "b2" ["a1"      ]
    let l2 = loop 1 "c" ["b1", "b2"]
    let a = add "a1" "b1" ["c"]
    mapM_ (([ l1, l2, a ] @=?) . f) $ permutations [ l1, l2, a ]


case_simulateFibonacci = do
    let cntxs = simulateAlg (def :: Cntx String Int)
            [ loop 0 "b2" ["a1"      ]
            , loop 1 "c" ["b1", "b2"]
            , add "a1" "b1" ["c"] :: F String Int
            ]
    let fibs = reverse (cntxVars (cntxs !! 20) M.! "a1")
    [0, 1, 1, 2, 3, 5, 8] @=? fibs


functionTests :: TestTree
functionTests = $(testGroupGenerator)
