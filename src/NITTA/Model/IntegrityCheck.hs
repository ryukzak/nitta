{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.Model.IntegrityCheck
Description : Module for checking PU model description consistency
Copyright   : (c) Artyom Kostyuchik, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.IntegrityCheck where

import Control.Monad
import Data.Data
import Data.Either
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Debug.Trace
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus (BusNetwork, Instruction (Transport))
import NITTA.Model.ProcessorUnits
import NITTA.Utils

class ProcessConsistent u where
    checkProcessСonsistent :: u -> Either String ()

instance ProcessConsistent (BusNetwork pu v x t) where
    checkProcessСonsistent pu = Left "qc"

checkIntegrity pu =
    let handleLefts l = case partitionEithers l of
            ([], _) -> True
            (a, _) -> Debug.Trace.traceShow (concat a) False
     in -- (a, _) -> False
        handleLefts
            -- TODO: why so much calls(prints) in tests?
            [ checkEndpointToIntermidiateRelation (getEpMap pu) (getInterMap pu) pu
            , checkInstructionToEndpointRelation (getInstrMap pu) (getEpMap pu) $ process pu
            , checkCadToFunctionRelation (getCadFunctions pu) (getCadSteps pu) pu
            ]

checkEndpointToIntermidiateRelation eps ifs pu =
    let genRels = makeRelationList eps ifs
        rels = S.fromList $ filter isVertical $ relations $ process pu
        checkIfsEmpty = M.size eps > 0 && M.size ifs == 0
        checkEpsEmpty = M.size ifs > 0 && M.size eps == 0
     in do
            when checkIfsEmpty $ Left "functions are empty"
            when checkEpsEmpty $ Left "eps are empty"
            if any (`S.isSubsetOf` rels) genRels
                then Right True
                else checkTransportToIntermidiateRelation pu ifs rels eps

-- Lazy variant which don't take into account relation between @PU
-- TODO: add map with endpoints (as Source) to be sure that function is connected to endpoint after all
checkTransportToIntermidiateRelation pu ifs rels eps =
    -- TODO we don't know did we found relation for all variables in function
    let transM = getTransportMap pu
        -- TODO: add smarter error handling
        lookup v = fromMaybe (showErr v) $ transM M.!? v
        makeRelationList =
            map S.fromList $
                concatMap
                    ( \(h, f) ->
                        concatMap
                            ( \v -> [[Vertical h $ fst $ lookup v]]
                            )
                            $ variables f
                    )
                    $ M.toList ifs
        showErr v =
            error $
                show " variable is not present: " <> show v <> " \n" <> show (process pu)
                    <> "\nifs: "
                    <> show ifs
                    <> "\neps: "
                    <> show (length eps)
                    <> "\nrels: "
                    <> show rels
     in if any (`S.isSubsetOf` rels) makeRelationList
            then Right True
            else Left "Endpoint and Transport to Intermideate (function) not consistent"

makeRelationList eps ifs =
    map S.fromList $
        concatMap
            ( \(h, f) ->
                sequence $
                    concatMap
                        ( \v -> [[Vertical h $ fst p | p <- eps M.! v]]
                        )
                        $ variables f
            )
            $ M.toList ifs

checkInstructionToEndpointRelation ins eps pr =
    let checkInsEmpty = M.size eps > 0 && M.size ins == 0
        checkEpsEmpty = M.size ins > 0 && M.size eps == 0
        eps' = M.fromList $ concat $ M.elems eps
        rels = S.fromList $ map (\(Vertical r1 r2) -> (r1, r2)) $ filter isVertical $ relations pr
        consistent =
            and $
                concatMap
                    ( \(r1, r2) -> case eps' M.!? r1 of -- TODO could be two sided relation
                        Just _ | Just (InstructionStep _) <- ins M.!? r2 -> [True]
                        _ -> []
                    )
                    rels
     in do
            when checkInsEmpty $ Left "instructions are empty"
            when checkEpsEmpty $ Left "enpoints are empty"
            if consistent
                then Right True
                else Left "Instruction to Endpoint not consistent"

-- at the moment check LoopBegin/End
checkCadToFunctionRelation cadFs cadSt pu =
    let consistent = S.isSubsetOf makeCadVertical rels
        rels = S.fromList $ filter isVertical $ relations $ process pu
        showLoop f = "bind " <> show f
        makeCadVertical =
            S.fromList $
                concatMap
                    ( \(h, f) ->
                        concatMap
                            ( \v -> [uncurry Vertical (cadSt M.! v, h)]
                            )
                            [showLoop f]
                    )
                    $ M.toList cadFs
     in if consistent
            then Right True
            else
                Left $
                    "CAD functions not consistent. excess:"
                        <> show (S.difference makeCadVertical rels)
                        <> " act: "
                        <> show (process pu)
                        <> " \nfs: "
                        <> show cadFs
                        <> " \nst: "
                        <> show cadSt

getInterMap pu =
    M.fromList
        [ (pID, f)
        | step@Step{pID} <- steps $ process pu
        , isFB step
        , f <- case getFunction step of
            Just f -> [f]
            _ -> []
        ]

getEpMap pu =
    M.fromListWith (++) $
        concat
            [ concatMap (\v -> [(v, [(pID, ep)])]) $ variables ep
            | step@Step{pID} <- steps $ process pu
            , isEndpoint step
            , ep <- case getEndpoint step of
                Just e -> [e]
                _ -> []
            ]

-- Contains instructions
getInstrMap pu =
    M.fromList
        [ (pID, instr)
        | step@Step{pID} <- steps $ process pu
        , isInstruction step
        , instr <- case getInstruction step of
            Just i -> [i]
            _ -> []
        ]

-- M.Map  ProcessStepID (a, (ProcessStepID, Instruction (BusNetwork String a x1 t1)))
getTransportMap pu =
    let getTransport :: (Typeable a, Typeable v, Typeable x, Typeable t) => pu v x t -> a -> Maybe (Instruction (BusNetwork String v x t))
        getTransport _ = cast
        filterTransport pu' pid (InstructionStep ins)
            | Just instr@(Transport v _ _) <- getTransport pu' ins = Just (v, (pid, instr))
            | otherwise = Nothing
        filterTransport _ _ _ = Nothing
     in M.fromList $ mapMaybe (uncurry $ filterTransport pu) $ M.toList $ getInstrMap pu

-- (pid, f)
getCadFunctions pu =
    let filterCad (_, f)
            | Just Loop{} <- castF f = True
            | Just (LoopBegin Loop{} _) <- castF f = True
            | Just (LoopEnd Loop{} _) <- castF f = True
            | otherwise = False
     in M.fromList $ filter filterCad $ M.toList $ getInterMap pu

getCadSteps pu =
    M.fromList
        [ (pDesc', pID)
        | step@Step{pID} <- steps $ process pu
        , pDesc' <- case getCAD step of
            Just msg -> [msg]
            _ -> []
        ]
