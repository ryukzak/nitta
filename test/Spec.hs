{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Array                  (elems)
import           Data.Default
import           Data.List                   (nub)
import           Data.Maybe
import           Data.Set                    (fromList, (\\))
import           Data.Typeable
import           NITTA.Base
import           NITTA.Compiler
import           NITTA.FunctionBlocks
import           NITTA.ProcessUnits.FRAM
import           NITTA.ProcessUnits.FRAMSpec
import           NITTA.ProcessUnitsSpec
import           NITTA.Timeline
import           NITTA.Types
import           Test.QuickCheck

-- alg = [FB (Loop "tos" ["mzz"])]
main = do
  -- let vars = concatMap variables alg
  -- let pu = bindAllAndNaiveSteps (def :: FRAM Passive String Int) alg
  -- let steps' = steps $ process $ pu
  -- let vars' = concatMap variables $ catMaybes
              -- $ map (\Step{..} -> (cast info :: Maybe (Effect String))) steps'
  -- let alg' = catMaybes $ map (\Step{..} -> (cast info :: Maybe (FB String))) steps'
  -- putStrLn $ show (fromList vars \\ fromList vars')
  -- putStrLn $ show (fromList alg \\ fromList alg')
  -- putStrLn $ show (length (nub vars') == length vars')
  -- putStrLn $ show (length (nub alg') == length alg')
  -- mapM_ (putStrLn . show) $ elems $ frMemory pu
  -- mapM_ (putStrLn . show) $ frRemains pu
  -- timeline "resource/data.json" pu
  -- testBench pu [("wue",88),("rgc",254),("pfg",254)]

  quickCheck (prop_formalCompletnessNaive :: FramAlg -> Bool)
  quickCheck (prop_simulationNaive :: FramAlg -> Property)
  verboseCheck (prop_formalCompletness :: FramProcess -> Bool)
  -- quickCheck (prop_simulation :: FramProcess -> Property)

  -- quickCheckWith stdArgs { maxSuccess=10000
                         -- } (prop_completeWorkPassivePu :: FramAlg -> Bool)
  -- quickCheckWith stdArgs { maxSuccess=10000
                           -- } (prop_simulatePassivePu :: FramAlg -> Property)


