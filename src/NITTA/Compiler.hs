-- {-# OPTIONS -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE UndecidableInstances   #-}

module NITTA.Compiler where

import           Control.Monad
import           Data.Array              (elems)
import           Data.Either
import           Data.List               (find, intersect, partition, sortBy)
import qualified Data.Map                as M
import           Data.Maybe              (isJust)
import           Debug.Trace
import           NITTA.BusNetwork
import           NITTA.FunctionBlocks
import           NITTA.ProcessUnits.Fram
import           NITTA.Types
import           NITTA.Utils


bindAll pu alg = fromRight undefined $ foldl nextBind (Right pu) alg
  where
    nextBind (Right s) n = bind s n
    nextBind (Left r) n  = error r

manualSteps pu acts = foldl (\pu' act -> step pu' act) pu acts

bindAllAndNaiveSteps pu0 alg = naive' $ bindAll pu0 alg
  where
    naive' pu
      | var:_ <- variants pu =
          naive'
          -- $ trace (concatMap ((++ "\n") . show) $ elems $ frMemory pu)
          $ step pu $ puVar2puAct var
      | otherwise = pu

puVar2puAct PUVar{ vAt=TimeConstrain{..}, .. } = PUAct vEffect $ Event tcFrom tcDuration



threshhold = 2
naive net = do
  let vs = practicalVariants $ variants net
  let bvs = bindVariants net
  -- putStrLn "-----------------------------------"
  -- mapM_ (putStrLn . show) vs
  -- mapM_ (putStrLn . show) bvs
  if length vs >= threshhold
    then if null vs
         then return net
         else return $ naiveStep net
    else if null bvs
         then if null vs
              then return net
              else return $ naiveStep net
         else return $ autoBind net





practicalVariants = filter
  (\NetworkVariant{..} -> not $ null $ filter isJust $ M.elems vPush)

naiveStep pu@BusNetwork{..} =
  case sortBy (\a b -> start a `compare` start b) $ practicalVariants $ variants pu of
    v:_ -> step pu (v2a v)
    _   -> error "No variants!"
  where
    start = tcFrom . vPullAt
    -- mostly mad implementation
    v2a NetworkVariant{ vPullAt=TimeConstrain{..}, ..} = NetworkAction
      { aPullFrom=vPullFrom
      , aPullAt=Event tcFrom tcDuration
      , aPush=M.map (fmap $ \(title, TimeConstrain{..}) ->
                        (title, Event pushStartAt tcDuration)
                    ) vPush
      }
      where
        pushStartAt = tcFrom + tcDuration




data BindVariant title v = BindVariant
  { fb       :: FB v
  , puTitle  :: title
  , priority :: Maybe BindPriority
  } deriving (Show)

data BindPriority
  = Exclusive
  | Restless Int
  | Input Int
  -- Must be binded before a other, because otherwise can cause runtime error.
  | Critical
  deriving (Show, Eq)

instance Ord BindPriority where
  Critical `compare` _ = GT
  _ `compare` Critical = LT
  (Input    a) `compare` (Input    b) = a `compare` b -- чем больше, тем лучше
  (Input    _) `compare`  _           = GT
  (Restless _) `compare` (Input    _) = LT
  (Restless a) `compare` (Restless b) = b `compare` a -- чем меньше, тем лучше
  (Restless _) `compare`  _           = GT
  Exclusive `compare` Exclusive = EQ
  Exclusive `compare` _ = LT





autoBind net@BusNetwork{..} =
  let prioritized = sortBV $ map mkBV bVars
  in case trace' prioritized of
      (BindVariant fb puTitle _):_ -> subBind fb puTitle net
      _                            -> error "Bind variants is over!"
  where
    bVars = bindVariants net
    mkBV (fb, titles) = prioritize $ BindVariant fb titles Nothing
    sortBV = reverse . sortBy (\a b -> priority a `compare` priority b)

    mergedBVars = foldl (\m (fb, puTitle) -> M.alter
                          (\case
                              Just puTitles -> Just $ puTitle : puTitles
                              Nothing -> Just [puTitle]
                          ) fb m
                   ) (M.fromList []) bVars

    prioritize bv@BindVariant{..}
      -- | isCritical fb = bv{ priority=Just Critical}

      | dependency fb == []
      , pulls <- filter isPull $ variantsAfterBind bv
      , length pulls > 0
      = bv{ priority=Just $ Input $ sum $ map (length . variables) pulls}

      | Just (variable, tcFrom) <- find (\(v, _) -> v `elem` variables fb) restlessVariables
      = bv{ priority=Just $ Restless tcFrom }

      | length (mergedBVars M.! fb) == 1
      = bv{ priority=Just Exclusive }

      | otherwise = bv

    restlessVariables = [ (variable, tcFrom)
      | NetworkVariant{ vPullAt=TimeConstrain{..}, ..} <- variants net
      , (variable, Nothing) <- M.assocs vPush
      ]

    variantsAfterBind BindVariant{..} = case bind (niPus M.! puTitle) fb of
      Right pu' -> filter (\(PUVar act _) -> act `variantOf` fb)  $ variants pu'
      _  -> []
      where
        act `variantOf` fb = not $ null (variables act `intersect` variables fb)

    trace' vs = trace ("---------"++ show (restlessVariables)
               ++ "\n" ++ (concatMap (\v -> show v ++ "\n") vs)) vs
