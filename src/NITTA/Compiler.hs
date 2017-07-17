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
import           Data.List            (find, intersect, partition, sortBy)
import qualified Data.Map             as M
import           Data.Maybe           (isJust)
import           Debug.Trace
import           NITTA.Base
import           NITTA.BusNetwork
import           NITTA.FunctionBlocks
import           NITTA.Types




naive net = do
  if 2 < length (practicalVariants $ variants net)
    then naiveStep net
    else if null $ bindVariants net
         then if null $ practicalVariants $ variants net
              then naiveStep net
              else net
         else autoBind net




practicalVariants = filter
  (\NetworkVariant{..} -> not $ null $ filter isJust $ M.elems vPush)

naiveStep pu@BusNetwork{..} =
  case practicalVariants $ variants pu of
    v:_ -> step pu (v2a v)
    _   -> error "No variants!"
  where
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
  deriving (Show, Eq)

instance Ord BindPriority where
  (Input    a) `compare` (Input    b) = a `compare` b -- чем больше, тем лучше
  (Input    _) `compare`  _           = GT
  (Restless _) `compare` (Input    _) = LT
  (Restless a) `compare` (Restless b) = b `compare` a -- чем меньше, тем лучше
  (Restless _) `compare`  _           = GT
  Exclusive `compare` Exclusive = EQ
  Exclusive `compare` _ = LT
  a `compare`  b           = error (show a ++ "  " ++ show b)





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
      Just pu' -> filter (\(PUVar act _) -> act `variantOf` fb)  $ variants pu'
      Nothing  -> []
      where
        act `variantOf` fb = not $ null (variables act `intersect` variables fb)

    trace' vs = trace ("---------"++ show (restlessVariables)
               ++ "\n" ++ (concatMap (\v -> show v ++ "\n") vs)) vs
