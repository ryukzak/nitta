{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Intermediate.Simulation
Description : Functional simulation
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Intermediate.Simulation (
    simulateDataFlowGraph,
    simulateAlg,
    reorderAlgorithm,
) where

import qualified Data.HashMap.Strict as HM
import Data.List (intersect, (\\))
import qualified Data.Map.Strict as M
import Data.Set (elems)
import Data.String.Interpolate
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.Utils

-- |Functional algorithm simulation
simulateDataFlowGraph ::
    (Var v, Val x, WithFunctions g (F v x)) =>
    Int ->
    CycleCntx v x ->
    [(v, [x])] ->
    g ->
    Cntx v x
simulateDataFlowGraph cycleN cycle0 transmission dfg =
    simulateAlg cycleN cycle0 transmission $ reorderAlgorithm $ functions dfg

simulateAlg ::
    (Var v, Val x) =>
    Int ->
    CycleCntx v x ->
    [(v, [x])] ->
    [F v x] ->
    Cntx v x
simulateAlg cycleN cycle0 transmission alg
    | let cycleConnections [] = []
          cycleConnections (f : fs)
            -- without refactoring
            | Just (Loop _ (O ov) (I iv)) <- castF f = (iv, elems ov) : cycleConnections fs
            -- after refactoring (BreakLoopD)
            | Just (LoopBegin (Loop _ (O ov) (I iv)) _) <- castF f = (iv, elems ov) : cycleConnections fs
            | otherwise = cycleConnections fs

          fromPrevCycle = cycleConnections alg =
        Cntx
            { cntxReceived = M.fromList transmission
            , cntxProcess = take cycleN $ simulateAlg' fromPrevCycle cycle0 transmission alg
            , cntxCycleNumber = cycleN
            }

simulateAlg' fromPrevCycle cycleCntx0 transmission alg =
    let (cycleCntx0', transmission') = receive' cycleCntx0 transmission
        cycleCntx = simulateCycle cycleCntx0' alg
     in cycleCntx : simulateAlg' fromPrevCycle (throwLoop cycleCntx) transmission' alg
    where
        -- TODO: receive data for several IO processor unit.
        receive' CycleCntx{cycleCntx} trans =
            ( CycleCntx $
                foldl
                    ( \c (v, xs) ->
                        case xs of
                            x : _ -> HM.insert v x c
                            _ -> c
                    )
                    cycleCntx
                    trans
            , map
                ( \(v, xs) ->
                    case xs of
                        (_ : _) -> (v, tail xs)
                        [] -> (v, xs)
                )
                trans
            )
        throwLoop (CycleCntx cntx) =
            CycleCntx $
                HM.fromList $
                    foldl
                        ( \st (thrown, vs) -> map (\v -> (v, cntx HM.! thrown)) vs ++ st
                        )
                        []
                        fromPrevCycle
        simulateCycle cntx00 fs =
            foldl
                ( \cntx f ->
                    case updateCntx cntx $ simulate cntx f of
                        Right cntx' -> cntx'
                        Left e -> error [i|can't simulate #{f} in context #{cntx}: #{e}|]
                )
                cntx00
                fs

reorderAlgorithm alg = orderAlgorithm' [] alg
    where
        orderAlgorithm' _ [] = []
        orderAlgorithm' vs fs
            | loops@(_ : _) <- filter isLoop fs
              , let loopOutputs = elems $ unionsMap outputs loops =
                case filter (not . null . intersect loopOutputs . elems . inputs) loops of
                    [] -> loops ++ orderAlgorithm' (elems (unionsMap variables loops) ++ vs) (fs \\ loops)
                    ready -> ready ++ orderAlgorithm' (elems (unionsMap variables ready) ++ vs) (fs \\ ready)
        orderAlgorithm' vs fs
            | let ready = filter (null . (\\ vs) . elems . inputs) fs
              , not $ null ready =
                ready ++ orderAlgorithm' (elems (unionsMap variables ready) ++ vs) (fs \\ ready)
        orderAlgorithm' _ remain = error $ "Can't sort algorithm: " ++ show remain
