{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Intermediate.Simulation
Description : Functional simulation
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Intermediate.Simulation
    ( simulateDataFlowGraph, simulateAlg
    , reorderAlgorithm
    ) where

import           Data.List                    (intersect, (\\))
import qualified Data.Map                     as M
import           Data.Set                     (elems)
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           NITTA.Utils


-- |Functional algorithm simulation
simulateDataFlowGraph
    :: ( Var v, Val x, WithFunctions g (F v x) )
    => Int -> CycleCntx v x -> [(v, [x])] -> g -> Cntx v x
simulateDataFlowGraph cycleN cycle0 transmission dfg
    = simulateAlg cycleN cycle0 transmission $ reorderAlgorithm $ functions dfg

simulateAlg cycleN cycle0 transmission alg
    | let
        cycleConnections [] = []
        cycleConnections (f:fs)
            -- without refactoring
            | Just (Loop _ (O o) (I i)) <- castF f = ( i, elems o ) : cycleConnections fs
            -- after refactoring (BreakLoopD)
            | Just (LoopOut (Loop _ (O o) (I i)) _) <- castF f = ( i, elems o ) : cycleConnections fs
            | otherwise = cycleConnections fs

        fromPrevCycle = cycleConnections alg
    = Cntx
        { cntxReceived=M.fromList transmission
        , cntxProcess=simulateAlg' fromPrevCycle cycle0 transmission alg
        , cntxCycleNumber=cycleN
        }

simulateAlg' fromPrevCycle cycleCntx0 transmission alg = let
        (cycleCntx0', transmission') = receive' cycleCntx0 transmission
        cycleCntx = simulateCycle cycleCntx0' alg
    in cycleCntx : simulateAlg' fromPrevCycle (throwLoop cycleCntx) transmission' alg
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
            ) [] fromPrevCycle
        simulateCycle cntx00 fs = foldl (\cntx f ->
            case simulate cntx f of
                Left err    -> error $ "functional simulation error: " ++ err ++ " function: " ++ show f
                Right cntx' -> cntx'
            ) cntx00 fs


reorderAlgorithm alg = orderAlgorithm' [] alg
     where
        orderAlgorithm' _ [] = []
        orderAlgorithm' vs fs
            | loops@(_:_) <- filter isLoop fs
            , let loopOutputs = elems $ unionsMap outputs loops
            = case filter (not . null . intersect loopOutputs . elems . inputs) loops of
                [] -> loops ++ orderAlgorithm' (elems (unionsMap variables loops) ++ vs) (fs \\ loops)
                ready -> ready ++ orderAlgorithm' (elems (unionsMap variables ready) ++ vs) (fs \\ ready)
        orderAlgorithm' vs fs
            | let ready = filter (null . (\\ vs) . elems . inputs) fs
            , not $ null ready
            = ready ++ orderAlgorithm' (elems (unionsMap variables ready) ++ vs) (fs \\ ready)
        orderAlgorithm' _ remain = error $ "Can't sort algorithm: " ++ show remain
