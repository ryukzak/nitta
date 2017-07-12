{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module NITTA.BusNetwork where

import           Control.Monad.State
import           Data.Default
import qualified Data.Graph           as G
import           Data.List            (find, intersect, partition, sortBy)
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe, isJust)
import           Data.Typeable
import           NITTA.Base
import           NITTA.FunctionBlocks
import qualified NITTA.FunctionBlocks as FB
import           NITTA.ProcessUnits



data Network title

instance PUType (Network title) where
  data Variant (Network title) v t
    = NetworkVariant
    { vPullFrom :: title
    , vPullAt   :: TimeConstrain t
    , vPush     :: M.Map v (Maybe (title, TimeConstrain t))
    }
  data Action (Network title) v t
    = NetworkAction
    { aPullFrom :: title
    , aPullAt   :: Event t
    , aPush     :: M.Map v (Maybe (title, Event t))
    }





data BusNetwork title ty v t =
  BusNetwork
    { niRemains   :: [FB v]
    , niDelegated :: [FB v]
    , niPus       :: M.Map title (PU Passive v t)
    , niProcess   :: Process v t
    , niWires     :: M.Map Int [(title, S)]
    }



instance (Time t) => Default (BusNetwork title ty v t) where
  def = BusNetwork def def def def def

instance ( Show title, Typeable title, Var v, Time t
         ) => ProcessInfo (Instruction (BusNetwork title) v t)

instance ( Typeable title, Ord title, Show title, Var v, Time t
         ) => PUClass (BusNetwork title) (Network title) v t where
  data Instruction (BusNetwork title) v t = Transport v title title
    deriving (Typeable, Show)

  data Signals (BusNetwork title) = Wire Int

  signal' BusNetwork{..} (Wire i) t = foldl1 (+++) $ map (uncurry subSignal) $ niWires M.! i
    where
      subSignal puTitle s = let pu = niPus M.! puTitle
                            in case pu of
                                 PU pu -> signal pu s t

  evaluate ni@BusNetwork{..} fb = Just ni{ niRemains=fb : niRemains }
  variants = nittaVariants
  step ni@BusNetwork{..} act@NetworkAction{..} = ni
    { niPus=foldl (\s n -> n s) niPus steps
    , niProcess=snd $ modifyProcess niProcess $ do
        mapM_ (\(v, (title, _)) -> add aPullAt $
                (Transport v aPullFrom title :: Instruction (BusNetwork title) v t)) $ M.assocs push'
    }
    where
      pullStep = M.adjust (\dpu -> step dpu $ PUAct (Pull pullVars) aPullAt) aPullFrom
      pushStep (var, (dpuTitle, pushAt)) =
        M.adjust (\dpu -> step dpu $ PUAct (Push var) pushAt) dpuTitle
      pushSteps = map pushStep $ M.assocs push'
      steps = pullStep : pushSteps

      push' = M.map (fromMaybe undefined) $ M.filter isJust aPush
      pullVars = M.keys push'


  process ni@BusNetwork{..} = let
    p'@Process{ steps=steps' } = snd $ modifyProcess niProcess $ do
      let subSteps = [ (puTitle, step)
                     | (puTitle, pu) <- sortBy (\a b -> fst a `compare` fst b) $ M.assocs niPus
                     ,  step <- steps $ process pu
                     ]
      mapM (\(puTitle, step@Step{..}) ->
              add time (Nested uid puTitle info :: Nested title v t)
           ) $ subSteps
    in p'{ steps=reverse steps' }


data Nested title v t where
  Nested :: ( ProcessInfo info ) =>
    { nestedUid  :: ProcessUid
    , nestedTitle :: title
    , info :: info
    } -> Nested title v t

deriving instance ( Show title ) => Show (Nested title v t)

instance ( Typeable title
         , Show title
         , Var v, Time t
         ) => ProcessInfo (Nested title v t)

nittaVariants bn@BusNetwork{..} = justTransport
  where
    avs = availableVars bn

    dpuVariants = concat $ map (\(title, d) -> zip (repeat title) $ variants d)
                  $ M.assocs niPus
    isPull (_, PUVar (Pull _) _) = True
    isPull _                     = False
    -- если pull есть в DPU, то он точно есть в alg.
    (pulls, pushs) = partition isPull dpuVariants
    justTransport =
      [ NetworkVariant title time $ M.fromList
          [ (v, if v `elem` avs
                then (\(title, PUVar _ time) -> (title, time)) <$> findPush v pushs
                else Nothing
            )
          | v <- vs
          ]
      | (title, PUVar (Pull vs) time) <- pulls
      ]
    findPush v = find (\case
                          (title, PUVar (Push v') time) | v == v' -> True
                          _ -> False
                      )





delegatable fb dpu = isJust $ evaluate dpu fb

delegate fb dpuTitle ni@BusNetwork{..} = ni
  { niPus=M.adjust (\dpu -> fromMaybe undefined $ evaluate dpu fb) dpuTitle niPus
  , niRemains=filter (/= fb) niRemains
  , niDelegated=fb : niDelegated
  }

delegationVariants BusNetwork{..} = concatMap delegationVariants' niRemains
  where
    delegationVariants' fb =
      map (uncurry $ variantsAfterDelegation fb) $ M.assocs niPus
    variantsAfterDelegation fb dpuTitle dpu =
      let act `variantOf` fb = not $ null (variables act `intersect` variables fb)
          vs = case evaluate dpu fb of
            -- try to reuse dpu'
            Just dpu' -> filter (\(PUVar act _) -> act `variantOf` fb)  $ variants dpu'
            Nothing   -> []
      in (fb, dpuTitle, vs)


availableVars BusNetwork{..} =
-- only for delegated FB
  let fbs = niRemains ++ niDelegated
      alg = foldl
        (\m (a, b) -> M.adjust ((:) b) a m)
        (M.fromList [(v, []) | v <- concat $ map variables fbs])
        $ concat $ map dependency fbs
  in concat $ map snd $ filter (not . null . snd) $ M.assocs alg

