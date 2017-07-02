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
{-# LANGUAGE TupleSections             #-}

module NITTA.NITTA where

import           Control.Monad.State
import           Data.Default
import qualified Data.Graph           as G
import           Data.List            (find, intersect, partition, sortBy)
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe, isJust)
import           NITTA.Base
import           NITTA.FunctionBlocks
import qualified NITTA.FunctionBlocks as FB
import           NITTA.ProcessUnits




data Transport title mt v t = Transport
  { pullFrom :: title
  , pullAt   :: mt t
  , push     :: M.Map v (Maybe (title, mt t))
                      -- Nothing значит либо, операция ещё не распределена,
                      -- либо DPU не может его принять.
  } deriving (Show, Eq)
type NiVariant title = Transport title TimeConstrain
type NiAction title = Transport title Event




data NiInfo v where
  NiCompiler :: String -> NiInfo v
  NiTransport :: [v] -> NiInfo v
  NiSignal :: (Show sig) => sig -> NiInfo v
type NiStep title = NestedStep NiInfo title (Step PuInfo)

instance (Show v) => Show (NiInfo v) where
  show (NiCompiler s)  = s
  show (NiTransport m) = show m
  show (NiSignal sig)  = "signal: " ++ show sig

instance Level (NiInfo v) where
  level (NiCompiler s)   = "NiCompiler"
  level (NiTransport fb) = "NiTransport"
  level (NiSignal i)     = "NiSignal"




data NITTA title (variant :: * -> * -> *) (action :: * -> * -> *) (step :: * -> * -> * -> *) v t k =
  NITTA
    { niRemains   :: [FB v]
    , niDelegated :: [FB v]
    , niPus       :: M.Map title (PU PuVariant PuAction PuStep v t k)
    , niProcess   :: Process step v t k
    }



instance (Key k, Time t) => Default (NITTA title variant action step v t k) where
  def = NITTA def def def def

instance ( Ord title, Key k, Enum k, Var v, Time t
         ) => PUClass (NITTA title) (NiVariant title) (NiAction title) (NiStep title) v t k where
  evaluate ni@NITTA{..} fb = Just ni{ niRemains=fb : niRemains }
  variants = nittaVariants
  step = nittaStep
  process ni@NITTA{..} = let
    p'@Process{ steps=steps' } = snd $ modifyProcess niProcess $ do
      let subSteps = [ (puTitle, step)
                     | (puTitle, pu) <- sortBy (\a b -> fst a `compare` fst b) $ M.assocs niPus
                     ,  step <- steps $ process pu
                     ]
      mapM (\(puTitle, step) -> add (Nested puTitle step)) $ subSteps
    in p'{ steps=reverse steps' }



nittaVariants nitta@NITTA{..} = justTransport
  where
    avs = availableVars nitta

    dpuVariants = concat $ map (\(title, d) -> zip (repeat title) $ variants d)
                  $ M.assocs niPus
    isPull (_, Interaction (Pull _) _) = True
    isPull _                           = False
    -- если pull есть в DPU, то он точно есть в alg.
    (pulls, pushs) = partition isPull dpuVariants
    justTransport =
      [ Transport
        { pullFrom=title
        , pullAt=time
        , push=M.fromList
          [ (v, if v `elem` avs
                then (\(title, Interaction _ time) -> (title, time)) <$> findPush v pushs
                else Nothing
            )
          | v <- vs
          ]
        }
      | (title, Interaction (Pull vs) time) <- pulls
      ]
    findPush v = find (\case
                          (title, Interaction (Push v') time) | v == v' -> True
                          _ -> False
                      )


delegatable fb dpu = isJust $ evaluate dpu fb

delegate fb dpuTitle ni@NITTA{..} = ni
  { niPus=M.adjust (\dpu -> fromMaybe undefined $ evaluate dpu fb) dpuTitle niPus
  , niRemains=filter (/= fb) niRemains
  , niDelegated=fb : niDelegated
  }

delegationVariants NITTA{..} = concatMap delegationVariants' niRemains
  where
    delegationVariants' fb =
      map (uncurry $ variantsAfterDelegation fb) $ M.assocs niPus
    variantsAfterDelegation fb dpuTitle dpu =
      let act `variantOf` fb = not $ null (variables act `intersect` variables fb)
          vs = case evaluate dpu fb of
            -- try to reuse dpu'
            Just dpu' -> filter (\(Interaction act _) -> act `variantOf` fb)  $ variants dpu'
            Nothing   -> []
      in (fb, dpuTitle, vs)


availableVars NITTA{..} =
-- only for delegated FB
  let fbs = niRemains ++ niDelegated
      alg = foldl
        (\m (a, b) -> M.adjust ((:) b) a m)
        (M.fromList [(v, []) | v <- concat $ map variables fbs])
        $ concat $ map dependency fbs
  in concat $ map snd $ filter (not . null . snd) $ M.assocs alg

nittaStep ni@NITTA{..} Transport{..} = ni
  { niPus=foldl (\s n -> n s) niPus steps
  , niProcess=snd $ modifyProcess niProcess $ do
      add $ NStep pullAt (NiTransport pullVars)
  }
  where
    pullStep = M.adjust (\dpu -> step dpu $ Interaction (Pull pullVars) pullAt) pullFrom
    pushStep (var, (dpuTitle, pushAt)) =
      M.adjust (\dpu -> step dpu $ Interaction (Push var) pushAt) dpuTitle
    pushSteps = map pushStep $ M.assocs push'
    steps = pullStep : pushSteps

    push' = M.map (fromMaybe undefined) $ M.filter isJust push
    pullVars = M.keys push'

