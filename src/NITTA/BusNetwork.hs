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
import           Data.Array
import           Data.Default
import qualified Data.Graph           as G
import           Data.List            (find, intersect, nub, partition, sortBy)
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, fromMaybe, isJust)
import           Data.Typeable
import           Debug.Trace
import           NITTA.Base
import           NITTA.FunctionBlocks
import qualified NITTA.FunctionBlocks as FB
import           NITTA.Types




data BusNetwork title ty v t =
  BusNetwork
    { niRemains            :: [FB v]
    , niForwardedVariables :: [v]
    , niBinded             :: M.Map title [FB v]
    , niPus                :: M.Map title (PU Passive v t)
    , niProcess            :: Process v t
    , niWires              :: Array Int [(title, S)]
    }
busNetwork pus wires = BusNetwork [] [] (M.fromList []) (M.fromList pus) def wires





instance ( Typeable title, Ord title, Show title, Var v, Time t, Ix t
         ) => TestBench (BusNetwork title) (Network title) v t where
  fileName _ = "hdl/fram_net_tb.v"
  processFileName _ = "hdl/fram_net_tb.process.v"

  testBench bn@BusNetwork{ niProcess=Process{..}, ..} =
    let wires = map Wire $ reverse $ range $ bounds niWires
        signalValues t = map (\w -> signal' bn w t) wires
        values = map (concat . (map show) . signalValues) $ range (0, tick + 1)
    in concatMap (\v -> "wires <= 'b" ++ v ++ "; @(negedge clk);\n" ) values



instance ( Typeable title, Ord title, Show title, Var v, Time t
         ) => PUClass (BusNetwork title) (Network title) v t where

  data Instruction (BusNetwork title) v t = Transport v title title
    deriving (Typeable, Show)

  data Signals (BusNetwork title) = Wire Int

  signal' BusNetwork{..} (Wire i) t = foldl (+++) X $ map (uncurry subSignal) $ niWires!i
    where
      subSignal puTitle s = let pu = niPus M.! puTitle
                            in case pu of
                                 PU pu -> signal pu s t

  bind ni@BusNetwork{..} fb = Just ni{ niRemains=fb : niRemains }

  variants = nittaVariants

  step ni@BusNetwork{..} act@NetworkAction{..} = ni
    { niPus=foldl (\s n -> n s) niPus steps
    , niProcess=snd $ modifyProcess niProcess $ do
        mapM_ (\(v, (title, _)) -> add (Event transportStartAt transportDuration)
                (Transport v aPullFrom title :: Instruction (BusNetwork title) v t))
                $ M.assocs push'
        setTime $ transportStartAt + transportDuration
    , niForwardedVariables=pullVars ++ niForwardedVariables
    }
    where
      transportStartAt = eStart aPullAt
      transportDuration = maximum $
        map ((\Event{..} -> (eStart - transportStartAt) + eDuration) . snd) $ M.elems push'

      pullStep = M.adjust (\dpu -> step dpu $ PUAct (Pull pullVars) aPullAt) aPullFrom
      pushStep (var, (dpuTitle, pushAt)) =
        M.adjust (\dpu -> step dpu $ PUAct (Push var) pushAt) dpuTitle
      pushSteps = map pushStep $ M.assocs push'
      steps = pullStep : pushSteps

      push' = M.map (fromMaybe undefined) $ M.filter isJust aPush
      pullVars = M.keys push'


  process ni@BusNetwork{..} = let
    transportKey = M.fromList
      [ (variable, uid)
      | (Just (Transport variable _ _ :: Instruction (BusNetwork title) v t), uid)
        <- map (\Step{..} -> (cast info, uid)) $ steps niProcess
      ]
    p'@Process{ steps=steps' } = snd $ modifyProcess niProcess $ do
      let pus = sortBy (\a b -> fst a `compare` fst b) $ M.assocs niPus
      mapM (addSubProcess transportKey) pus

    in p'{ steps=reverse steps' }
    where
      addSubProcess transportKey (puTitle, pu) = do
        let subSteps = steps $ process pu
        uids' <- foldM (\dict step@Step{..} -> do
                           uid' <- add time (Nested uid puTitle info :: Nested title v t)
                           case cast info of
                             Just (fb :: FB v) ->
                               mapM_ (\v -> when (v `M.member` transportKey)
                                         $ relation $ Vertical (transportKey M.! v) uid'
                                     ) $ variables fb
                             Nothing -> return ()
                           return $ M.insert uid uid' dict
                       ) def subSteps
        let subRelations = relations $ process pu
        mapM (\r -> relation $ case r of
                 Vertical a b -> Vertical (uids' M.! a) (uids' M.! b)
             ) subRelations



nittaVariants bn@BusNetwork{..} = concat $
  [
    [ NetworkVariant fromPu pullAt $ M.fromList pushs
    | pushs <- sequence $ map pushVariantsFor pullVars
    , let pushTo = catMaybes $ map (fmap fst . snd) pushs
    -- , not $ null pushTo
    , length (nub pushTo) == length pushTo
    ]
  | (fromPu, variants) <- puVariants
  , PUVar (Pull pullVars) pullAt <- variants
  ]
  where
    pushVariantsFor v | v `notElem` availableVars = [(v, Nothing)]
    pushVariantsFor v = (v, Nothing) : pushVariantsFor' v

    pushVariantsFor' v = [ (v, Just (pushTo, pushAt))
                         | (pushTo, variants) <- puVariants
                         , PUVar (Push pushVar) pushAt <- variants
                         , pushVar == v
                         ]
    availableVars =
        let functionalBlocks = niRemains ++ (concat $ M.elems niBinded)
            alg = foldl
              (\dict (a, b) -> M.adjust ((:) b) a dict)
              (M.fromList [(v, []) | v <- concatMap variables functionalBlocks])
              $ filter (\(a, b) -> b `notElem` niForwardedVariables)
              $ concatMap dependency functionalBlocks
            notBlockedVariables = map fst $ filter (null . snd) $ M.assocs alg
        in notBlockedVariables L.\\ niForwardedVariables

    puVariants = M.assocs $ M.map variants niPus



bindVariants BusNetwork{..} =
  concatMap (\fb -> bindVariants' fb) niRemains
  where
    bindVariants' fb =
      [ (fb, puTitle) -- , newVariants pu fb)
      | (puTitle, pu) <- sortByLoad $ M.assocs niPus
      , isJust $ bind pu fb
      , not $ selfTransport fb puTitle niBinded
      ]

    sortByLoad = sortBy (\(a, _) (b, _) -> load a `compare` load b)
    load = length . binded

    selfTransport fb puTitle niBinded =
      not $ null $ variables fb `intersect` (concatMap variables $ binded puTitle)

    binded puTitle | puTitle `M.member` niBinded = niBinded M.! puTitle
                   | otherwise = []



subBind fb puTitle ni@BusNetwork{ niProcess=p@Process{..}, ..} = ni
  { niPus=M.adjust (\dpu -> fromMaybe undefined $ bind dpu fb) puTitle niPus
  , niBinded=M.alter (\v -> case v of
                         Just fbs -> Just $ fb : fbs
                         Nothing  -> Just [fb]
                     ) puTitle niBinded
  , niProcess=snd $ modifyProcess p $
      add (Event tick 0) $ "Bind " ++ show fb ++ " to " ++ puTitle
  , niRemains=filter (/= fb) niRemains
  }

