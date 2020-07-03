{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.Test.FunctionSimulation
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Test.FunctionSimulation
    (
    ) where

import           Data.Set                     (fromList, intersection)
import qualified Data.Set                     as S
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           Test.QuickCheck


-- *Orphans instances for Arbitrary

maxLenght = 8
varNameSize = 7
outputVarsGen = O . fromList <$> resize maxLenght (listOf1 $ vectorOf varNameSize $ elements ['a'..'z'])
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

instance Arbitrary (Acc String Int) where
    arbitrary = suchThat (Acc . concat <$> resize maxLenght (listOf1 $ (++) <$> genPush <*>  genPull) ) uniqueVars
        where
            genPush = resize maxLenght $ listOf1 $ oneof [Push Plus <$> inputVarGen,  Push Minus <$> inputVarGen]
            genPull = resize 1 $ listOf1 $ Pull <$> outputVarsGen
