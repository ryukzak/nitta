{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Intermediate.Simulation
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Intermediate.Simulation
    ( -- *Arithmetics
      simulateDataFlowGraph, simulateAlg
    , reorderAlgorithm
    ) where

import           Data.List                (intersect, (\\))
import qualified Data.Map                 as M
import           Data.Set                 (elems)
import           NITTA.Intermediate.Types
import           NITTA.Utils


-- |Functional algorithm simulation
simulateDataFlowGraph cycle0 transmission dfg = simulateAlg cycle0 transmission $ reorderAlgorithm $ functions dfg

simulateAlg cycle0 transmission alg
    | let
        cntxThrown = map ( \f ->
            ( let [v] = elems $ inputs f in v
            , elems $ outputs f
            ) ) $ filter isBreakLoop alg
    = Cntx
        { cntxReceived=M.fromList transmission
        , cntxProcess=simulateAlg' cntxThrown cycle0 transmission alg
        , cntxThrown=cntxThrown
        , cntxCycleNumber=5
        }

simulateAlg' cntxThrown cycleCntx0 transmission alg = let
        (cycleCntx0', transmission') = receive' cycleCntx0 transmission
        cycleCntx = simulateCycle cycleCntx0' alg
    in cycleCntx : simulateAlg' cntxThrown (throwLoop cycleCntx) transmission' alg
    where
        -- TODO: receive data for several IO processor unit.
        receive' CycleCntx{ cycleCntx } trans =
            ( CycleCntx $ foldl (\c (v, xs) ->
                case xs of
                    x:_ -> M.insert v x c
                    _   -> c
                ) cycleCntx trans
            , map (\(v, xs) ->
                case xs of
                    (_:_) -> (v, tail xs)
                    []    -> (v, xs)
                ) trans
            )
        throwLoop (CycleCntx cntx) = CycleCntx $ M.fromList $ foldl
            (\st (thrown, vs) -> map ( \v -> (v, cntx M.! thrown) ) vs ++ st
            ) [] cntxThrown
        simulateCycle cntx00 fs = foldl (\cntx f ->
            case simulate cntx f of
                Left err    -> error $ "functional simulation error: " ++ err
                Right cntx' -> cntx'
            ) cntx00 fs


reorderAlgorithm alg = orderAlgorithm' [] alg
     where
        orderAlgorithm' _ [] = []
        orderAlgorithm' vs fs
            | let insideOuts = filter isBreakLoop fs
            , not $ null insideOuts
            , let insideOutsOutputs = elems $ unionsMap outputs insideOuts
            = case filter (not . null . intersect insideOutsOutputs . elems . inputs) insideOuts of
                [] -> insideOuts ++ orderAlgorithm' (elems (unionsMap variables insideOuts) ++ vs) (fs \\ insideOuts)
                ready -> ready ++ orderAlgorithm' (elems (unionsMap variables ready) ++ vs) (fs \\ ready)
        orderAlgorithm' vs fs
            | let ready = filter (null . (\\ vs) . elems . inputs) fs
            , not $ null ready
            = ready ++ orderAlgorithm' (elems (unionsMap variables ready) ++ vs) (fs \\ ready)
        orderAlgorithm' _ _ = error "Can't sort algorithm."
