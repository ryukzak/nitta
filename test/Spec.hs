{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Default
import           Data.List                 (elem, notElem, nub, union)
import           Data.Maybe                (catMaybes)
import           Data.Set                  (fromList)
import           Data.Typeable
import           Debug.Trace
import           NITTA.Base
import           NITTA.FunctionBlocks
import           NITTA.ProcessUnits.FRAM
import           NITTA.Types
import           System.Random             (next)
import           Test.Hspec
import           Test.QuickCheck           hiding (listOf, vectorOf)
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen       hiding (listOf, vectorOf)





listOf gen = do
  size <- lift $ sized pure -- getSize
  n <- lift $ choose (0, size)
  vectorOf n gen
vectorOf k gen = sequence [ gen | _ <- [1..k] ]
oneOf gens = do
  i <- lift $ choose (0, length gens - 1)
  gens !! i



data AlgMaker = AlgMaker
  { vars       :: [String]
  , inputAddr  :: [Int]
  , outputAddr :: [Int]
  , loopNumber :: Int
  } deriving(Show)

instance Default AlgMaker where
  def = AlgMaker
    { vars=[[a, b] | a <- ['a' .. 'z'], b <- ['a' .. 'z']]
    , inputAddr=[]
    , outputAddr=[]
    , loopNumber=0
    }

variable = do
  st@AlgMaker{ vars=v : vs} <- get
  put st{ vars=vs }
  return v


useForInput addr = modify $ \st@AlgMaker{..} -> st{ inputAddr=addr : inputAddr }
useForOutput addr = modify $ \st@AlgMaker{..} -> st{ outputAddr=addr : outputAddr }

framInputGen = do
  AlgMaker{..} <- get
  addr <- lift $ suchThat (choose (0, 35)) (`notElem` inputAddr)
  useForInput addr
  v <- variable
  return $ framInput addr [v]

framOutputGen = do
  AlgMaker{..} <- get
  addr <- lift $ suchThat (choose (0, 35)) (`notElem` outputAddr)
  useForOutput addr
  v <- variable
  return $ framOutput addr v

regGen = do
  a <- variable
  b <- variable
  return $ reg a [b]

loopGen = do
  a <- variable
  b <- variable
  modify $ \st@AlgMaker{..} -> st{ loopNumber=loopNumber + 1 }
  return $ loop a [b]








genAlg gen = evalStateT gen def


instance {-# OVERLAPPING #-} Arbitrary [FB String] where
  arbitrary = genAlg $ framFB


framFB = do
    size <- lift $ sized pure -- getSize
    n <- lift $ choose (0, size)
    sequence [gen | _ <- [0..n]]
      where
        gen = do
          AlgMaker{..} <- get
          let usedAddr = nub (inputAddr `union` outputAddr)
          if (length usedAddr) + loopNumber < 36
            then oneOf [ framInputGen, framOutputGen, regGen, loopGen ]
            else oneOf [ regGen ]








-- listOf gen = do
--   size <- lift $ sized pure -- getSize
--   n <- lift $ choose (0, size)
--   vectorOf n gen
-- vectorOf k gen = sequence [ gen | _ <- [1..k] ]







  -- AlgMaker{..} <- get

  -- let usedAddr = nub (inputAddr `union` outputAddr)
  -- if (length usedAddr) - loopNumber < 36
  --   then
  --   -- modify $ \st@AlgMaker{..} -> st{ loopNumber=loopNumber + 1 }
  -- else


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





main = verboseCheck prop_passivePu
