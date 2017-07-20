{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Default
import           Data.List                 (elem, intersect, notElem, nub,
                                            union)
import           Data.Maybe                (catMaybes)
import           Data.Set                  (fromList)
import           Data.Typeable
-- import           Debug.Trace
import           NITTA.Base
import           NITTA.FunctionBlocks
import           NITTA.ProcessUnits.FRAM
import           NITTA.Types
import           System.Random             (next)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen       hiding (listOf, vectorOf)
import           Test.QuickCheck.Monadic



addr = choose (0, 35 :: Int)
forPull = resize 3 $ listOf1 $ vectorOf 3 (elements ['a'..'z'])
forPush = vectorOf 3 (elements ['a'..'z'])

instance Arbitrary (FRAMInput String) where
  arbitrary = FRAMInput <$> addr <*> forPull

instance Arbitrary (FRAMOutput String) where
  arbitrary = FRAMOutput <$> addr <*> forPush

instance Arbitrary (Loop String) where
  arbitrary = Loop <$> forPush <*> forPull

instance Arbitrary (Reg String) where
  arbitrary = Reg <$> forPush <*> forPull





data ST = ST { acc           :: [FB String]
             , forInput      :: [Int]
             , forOutput     :: [Int]
             , numberOfLoops :: Int
             , usedVariables :: [String]
             } deriving (Show)

instance {-# OVERLAPPING #-} Arbitrary [FB String] where
  arbitrary = acc <$> do
      size <- sized pure -- getSize
      n <- choose (0, size)
      foldM maker (ST [] [] [] 0 []) [ 0..n ]
    where
      maker st@ST{..} _ =
        nextState <$> suchThat (oneof [ FB <$> (arbitrary :: Gen (FRAMInput String))
                                      , FB <$> (arbitrary :: Gen (FRAMOutput String))
                                      , FB <$> (arbitrary :: Gen (Loop String))
                                      , FB <$> (arbitrary :: Gen (Reg String))
                                      ]
                               ) check
        where
          nextState fb = specificUpdate fb st
            { acc=fb : acc
            , usedVariables=variables fb ++ usedVariables
            }
          specificUpdate (FB fb) st
            | Just (FRAMInput addr (_ :: [String])) <- cast fb = st{ forInput=addr : forInput }
            | Just (FRAMOutput addr (_ :: String)) <- cast fb = st{ forOutput=addr : forOutput }
            | Just (Loop _ _ :: Loop String) <- cast fb = st{ numberOfLoops=numberOfLoops + 1 }
            | otherwise = st
          check fb0@(FB fb)
            | not $ null (variables fb0 `intersect` usedVariables) = False
            | Just (FRAMInput addr (_ :: [String])) <- cast fb =
                addr `notElem` forInput && (length forInput) + numberOfLoops < 36
            | Just (FRAMOutput addr (_ :: String)) <- cast fb =
                addr `notElem` forOutput && (length forOutput) + numberOfLoops < 36
            | Just (Loop _ _ :: Loop String) <- cast fb =
                length (nub $ forOutput `union` forInput) + numberOfLoops < 36
            | otherwise = True




prop_simPassivePu alg = monadicIO $ do
  assert $ prop_passivePu alg
  let pu = naive (def :: FRAM Passive String Int) alg
  run $ writeTestBench pu []
  res <- run $ evalTestBench pu
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



naive pu alg =
  let Just bindedPu = foldl (\(Just s) n -> bind s n) (Just pu) alg
  in naive' bindedPu
  where
    naive' pu
      | PUVar{ vAt=TimeConstrain{..}, .. }:_ <- variants pu =
          naive' $ step pu (PUAct vEffect $ Event tcFrom tcDuration)
      | otherwise = pu





main = verboseCheck prop_simPassivePu
