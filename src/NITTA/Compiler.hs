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
    nextBind (Right pu') fb = bind fb pu'
    nextBind (Left r) _     = error r

manualSteps pu acts = foldl (\pu' act -> step pu' act) pu acts

bindAllAndNaiveSteps pu0 alg = naive' $ bindAll pu0 alg
  where
    naive' pu
      | var:_ <- options pu =
          naive'
          -- $ trace (concatMap ((++ "\n") . show) $ elems $ frMemory pu)
          $ step pu $ effectVar2act var
      | otherwise = pu

effectVar2act EffectOpt{ eoAt=TimeConstrain{..}, .. } = EffectAct eoEffect $ Event tcFrom tcDuration



threshhold = 2
naive net = do
  let vs = practicalOptions $ options net
  let bvs = bindingOptions net
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





practicalOptions = filter
  (\TransportOpt{..} -> not $ null $ filter isJust $ M.elems toPush)

naiveStep pu@BusNetwork{..} =
  case sortBy (\a b -> start a `compare` start b) $ practicalOptions $ options pu of
    v:_ -> step pu (v2a v)
    _   -> error "No variants!"
  where
    start = tcFrom . toPullAt
    -- mostly mad implementation
    v2a TransportOpt{ toPullAt=TimeConstrain{..}, ..} = TransportAct
      { taPullFrom=toPullFrom
      , taPullAt=Event tcFrom tcDuration
      , taPush=M.map (fmap $ \(title, TimeConstrain{..}) ->
                         (title, Event pushStartAt tcDuration)
                     ) toPush
      }
      where
        pushStartAt = tcFrom + tcDuration




data BindOption title v = BindOption
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
  let prioritized = sortBV $ map mkBV bOpts
  in case trace' prioritized of
      (BindOption fb puTitle _):_ -> subBind fb puTitle net
      _                           -> error "Bind variants is over!"
  where
    bOpts = bindingOptions net
    mkBV (fb, titles) = prioritize $ BindOption fb titles Nothing
    sortBV = reverse . sortBy (\a b -> priority a `compare` priority b)

    mergedBOpts = foldl (\m (fb, puTitle) -> M.alter
                          (\case
                              Just puTitles -> Just $ puTitle : puTitles
                              Nothing -> Just [puTitle]
                          ) fb m
                   ) (M.fromList []) bOpts

    prioritize bv@BindOption{..}
      -- В настоящий момент данная операци приводит к тому, что часть FB перестают быть вычислимыми.
      -- | isCritical fb = bv{ priority=Just Critical }

      | dependency fb == []
      , pulls <- filter isPull $ optionsAfterBind bv
      , length pulls > 0
      = bv{ priority=Just $ Input $ sum $ map (length . variables) pulls}

      | Just (variable, tcFrom) <- find (\(v, _) -> v `elem` variables fb) restlessVariables
      = bv{ priority=Just $ Restless tcFrom }

      | length (mergedBOpts M.! fb) == 1
      = bv{ priority=Just Exclusive }

      | otherwise = bv

    restlessVariables = [ (variable, tcFrom)
      | TransportOpt{ toPullAt=TimeConstrain{..}, ..} <- options net
      , (variable, Nothing) <- M.assocs toPush
      ]

    optionsAfterBind BindOption{..} = case bind fb $ bnPus M.! puTitle of
      Right pu' -> filter (\(EffectOpt act _) -> act `optionOf` fb) $ options pu'
      _  -> []
      where
        act `optionOf` fb = not $ null (variables act `intersect` variables fb)

    trace' vs = trace ("---------"++ show (restlessVariables)
               ++ "\n" ++ (concatMap (\v -> show v ++ "\n") vs)) vs
