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



naive net = do
  if null $ practicalVariants $ variants net
    then if null $ bindVariants net
         then return net
         else return $ naiveBind net
    else naiveStep net


practicalVariants = filter (\NetworkVariant{..} ->
                                   not $ null $ filter isJust $ M.elems vPush
                                )

naiveStep pu@BusNetwork{..} = do
  let vars = variants pu
  when (length vars == 0) $ error "No variants!"
  -- mapM_ putStrLn $ map ((++) "" . show) vars
  -- putStrLn "-----------------------------"
  let act = v2a $ head $ practicalVariants vars
  -- putStrLn $ show act
  -- putStrLn "============================="
  return $ step pu act
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



naiveBind net@BusNetwork{..} =
  let vars = reverse $ sortBy (\a b -> priority a `compare` priority b)
                   $ map (\(fb, titles) -> prioritize $ BindVariant fb titles Nothing) bvs

      (BindVariant fb puTitle _):_ =
        trace ("---------"++ show (restlessVariables)
               ++ "\n" ++ (concatMap (\v -> show v ++ "\n") vars))
        vars
  in --trace ("bind " ++ show fb ++ " on " ++ puTitle) $
     subBind fb puTitle net
  where
    bvs = bindVariants net
    fb2pus = foldl (\m (fb, puTitle) -> M.alter (\case
                                                    Just puTitles -> Just $ puTitle : puTitles
                                                    Nothing -> Just [puTitle]
                                                ) fb m
                   ) (M.fromList []) bvs

    prioritize bv@BindVariant{..}
      | dependency fb == []
      , pulls <- filter isPull $ variantsAfterBind bv
      , length pulls > 0
      = bv{ priority=Just $ Input $ sum $ map (length . variables) pulls}

      | Just (variable, tcFrom) <- find (\(v, _) -> v `elem` variables fb) restlessVariables
      -- | not $ null ((map fst restlessVariables) `intersect` variables fb)
      -- , let vs = variantsAfterBind bv
      -- , 0 <= (length $ trace ("Restless variants: " ++ show vs) vs)
      = bv{ priority=Just $ Restless tcFrom }

      | length (fb2pus M.! fb) == 1
      = bv{ priority=Just Exclusive }

      | otherwise = bv

    restlessVariables = [ (v, tcFrom)
      | NetworkVariant{ vPullAt=TimeConstrain{..}, ..} <- variants net
      , (v, Nothing) <- M.assocs vPush
      ]

    variantsAfterBind BindVariant{..} = case bind (niPus M.! puTitle) fb of
      Just pu' -> filter (\(PUVar act _) -> act `variantOf` fb)  $ variants pu'
      Nothing  -> []
      where
        act `variantOf` fb = not $ null (variables act `intersect` variables fb)

