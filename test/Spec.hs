{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad
import           Data.Default
import           Data.List               (intersect, notElem, nub, union)
import qualified Data.List               as L
import           Data.Maybe              (catMaybes)
import           Data.Set                (fromList, (\\))
import           Data.Typeable
import           NITTA.Base
import           NITTA.FunctionBlocks
import           NITTA.ProcessUnits.FRAM
import           NITTA.Timeline
import           NITTA.Types
import           Test.QuickCheck
import           Test.QuickCheck.Monadic


addrGen = choose (0, framSize - 1)
forPull = resize 3 $ listOf1 $ vectorOf 3 (elements ['a'..'z'])
forPush = vectorOf 3 (elements ['a'..'z'])

uniqVars fb = let vs = variables fb
              in length vs == length (nub vs)

instance Arbitrary (FRAMInput String) where
  arbitrary = suchThat (FRAMInput <$> addrGen <*> forPull) uniqVars

instance Arbitrary (FRAMOutput String) where
  arbitrary = suchThat (FRAMOutput <$> addrGen <*> forPush) uniqVars

instance Arbitrary (Loop String) where
  arbitrary = suchThat (Loop <$> forPush <*> forPull) uniqVars

instance Arbitrary (Reg String) where
  arbitrary = suchThat (Reg <$> forPush <*> forPull) uniqVars





data ST = ST { acc           :: [FB String]
             , forInput      :: [Int]
             , forOutput     :: [Int]
             , numberOfLoops :: Int
             , usedVariables :: [String]
             , values        :: [(String, Int)]
             } deriving (Show)

instance {-# OVERLAPPING #-} Arbitrary ([FB String], [(String, Int)]) where
  arbitrary = (\ST{..} -> (acc, values)) <$> do
      size <- sized pure -- getSize
      n <- choose (0, size)
      foldM maker (ST [] [] [] 0 [] []) [ 0..n ]
    where
      maker st0@ST{..} _ = nextState st0 <$> do
        fb <- suchThat (oneof [ FB <$> (arbitrary :: Gen (FRAMInput String))
                              , FB <$> (arbitrary :: Gen (FRAMOutput String))
                              , FB <$> (arbitrary :: Gen (Loop String))
                              , FB <$> (arbitrary :: Gen (Reg String))
                              ]
                       ) check
        v <- choose (0 :: Int, 0xFF)
        return (fb, v)
        where
          nextState st (fb, v) = specificUpdate fb v st
            { acc=fb : acc
            , usedVariables=variables fb ++ usedVariables
            }
          specificUpdate (FB fb) value st
            | Just (FRAMInput addr vs) <- cast fb = st{ forInput=addr : forInput
                                                      , values=[ (v, 0x0A00 + addr)
                                                               | v <- vs
                                                               ] ++ values
                                                      }
            | Just (FRAMOutput addr v) <- cast fb = st{ forOutput=addr : forOutput
                                                      , values=(v, 0x0A00 + addr) : values
                                                      }
            | Just (Loop a _bs) <- cast fb = st{ numberOfLoops=numberOfLoops + 1
                                               , values=(a, value) : values
                                               -- bs check making with independently with TestBench
                                              }
            | Just (Reg a bs) <- cast fb = st{ values=(a, value) : [(b, value) | b <- bs] ++ values
                                             }
            | otherwise = error $ "Bad FB: " ++ show fb
          check fb0@(FB fb)
            | not $ null (variables fb0 `intersect` usedVariables) = False
            | Just (Reg _ _ :: Reg String) <- cast fb = True
            | not (length (nub $ forOutput `union` forInput) + numberOfLoops < framSize) = False
            | Just (FRAMInput addr (_ :: [String])) <- cast fb = addr `notElem` forInput
            | Just (FRAMOutput addr (_ :: String)) <- cast fb = addr `notElem` forOutput
            | otherwise = True -- for Loop





prop_simPassivePu :: ([FB String], [(String, Int)]) -> Property
prop_simPassivePu (alg, values) = monadicIO $ do
  assert $ prop_passivePu alg
  let pu = naive (def :: FRAM Passive String Int) alg
  res <- run $ testBench pu values
  assert res

prop_passivePu alg =
  let vars = concatMap variables alg
      pu = naive (def :: FRAM Passive String Int) alg
      steps' = steps $ process $ pu
      vars' = concatMap variables $ catMaybes
              $ map (\Step{..} -> (cast info :: Maybe (Effect String))) steps'
      alg' = catMaybes $ map (\Step{..} -> (cast info :: Maybe (FB String))) steps'
  in and [ fromList vars == fromList vars'
         , length (nub vars') == length vars'
         , length (nub alg') == length alg'
         , fromList alg == fromList alg'
         ]

naive pu0 alg =
  let Just bindedPu = foldl (\(Just s) n -> bind s n) (Just pu0) alg
  in naive' bindedPu
  where
    naive' pu
      | PUVar{ vAt=TimeConstrain{..}, .. }:_ <- variants pu =
          naive' $ step pu (PUAct vEffect $ Event tcFrom tcDuration)
      | otherwise = pu




main = do
  -- let vars = concatMap variables alg
  -- let pu = naive (def :: FRAM Passive String Int) alg
  -- let steps' = steps $ process $ pu
  -- let vars' = concatMap variables $ catMaybes
              -- $ map (\Step{..} -> (cast info :: Maybe (Effect String))) steps'
  -- let alg' = catMaybes $ map (\Step{..} -> (cast info :: Maybe (FB String))) steps'
  -- timeline "resource/data.json" pu
  -- putStrLn $ show (fromList vars \\ fromList vars')
  -- putStrLn $ show (fromList alg \\ fromList alg')
  -- putStrLn $ show (length (nub vars') == length vars')
  -- putStrLn $ show (length (nub alg') == length alg')
  -- putStrLn $ show (vars' L.\\ nub vars')



  quickCheck prop_simPassivePu
  -- quickCheckWith stdArgs { maxSuccess=1000
                         -- }  prop_simPassivePu

  -- verboseCheckWith stdArgs { maxSuccess=100
                           -- }  prop_simPassivePu
