{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

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
import           NITTA.Types
import           Test.QuickCheck

-- alg = [
  -- FB (FRAMOutput 3 "hwa"),
  -- FB (FRAMOutput 4 "vjf"),
  -- FB (FRAMOutput 5 "zyu"),
  -- FB (FRAMOutput 11 "qxk"),
  -- FB (FRAMOutput 13 "oro"),
  -- FB (FRAMOutput 15 "mgx"),

  -- FB (Loop "was" ["jng","ukv"]),
  -- FB (Loop "cnq" ["vqf","plm"]),
  -- FB (Loop "hrm" ["rwv"]),
  -- FB (Loop "bog" ["lqw","ghu","uhk"]),
  -- FB (Loop "oxh" ["nsk"]),
  -- FB (Loop "rwb" ["oqj","diq"]),
  -- FB (Loop "meo" ["zhw","gvc"]),
  -- FB (Loop "vst" ["fne","fsz","rpk"]),
  -- FB (Loop "dyc" ["gfh"]),
  -- FB (Loop "jhq" ["dod","ghh","uig"]),

  -- FB (Reg "ktq" ["ast","avi","rqb"])
  -- ]


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


  quickCheck (prop_simulatePassivePu :: FramAlg -> Property)
  quickCheck (prop_completeWorkPassivePu :: FramAlg -> Bool)
  -- quickCheckWith stdArgs { maxSuccess=1000
                         -- }  prop_simPassivePu
  -- verboseCheckWith stdArgs { maxSuccess=100
                           -- }  prop_simPassivePu
