{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.Model.IntegrityCheck
Description : Module for checking model description consistency
Copyright   : (c) Artyom Kostyuchik, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.IntegrityCheck where

import Control.Monad
import Data.Data
import Data.Either
import Data.List (find, maximumBy)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import qualified Data.Set as S
import qualified Debug.Trace
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus (BusNetwork, Instruction (Transport))
import NITTA.Model.ProcessorUnits
import NITTA.Utils
import NITTA.Utils.ProcessDescription

class ProcessConsistent u where
    checkProcessСonsistent :: u -> Either String ()

instance ProcessConsistent (BusNetwork pu v x t) where
    checkProcessСonsistent pu = Left "qc"

checkIntegrity pu =
    {-}
    let getInterMap =
            M.fromList
                [ (pID, f)
                | step@Step{pID} <- steps $ process pu
                , isFB step
                , f <- case getFunction step of
                    Just f -> [f]
                    _ -> []
                ]
        getEpMap =
            M.fromListWith (++) $
                concat
                    [ concatMap (\v -> [(v, [(pID, ep)])]) $ variables ep
                    | step@Step{pID} <- steps $ process pu
                    , isEndpoint step
                    , ep <- case getEndpoint step of
                        Just e -> [e]
                        _ -> []
                    ]
        getInstrMap =
            M.fromList
                [ (pID, instr)
                | step@Step{pID} <- steps $ process pu
                , isInstruction step
                , instr <- case getInstruction step of
                    Just i -> [i]
                    _ -> []
                ]

        -- (pid, f)
        getCadFunctions =
            let filterCad (_, f)
                    | Just Loop{} <- castF f = True
                    | Just (LoopBegin Loop{} _) <- castF f = True
                    | Just (LoopEnd Loop{} _) <- castF f = True
                    | otherwise = False
             in M.fromList $ filter filterCad $ M.toList getInterMap

        -- TODO: add Maybe?
        -- (Loop (pid, f)) , where Loop is show instance
        getCadSteps =
            M.fromList $
                concat
                    [ concatMap (\l -> [(l, (pID, step))]) pDesc'
                    | step@Step{pID} <- steps $ process pu
                    , pDesc' <- case getCAD step of
                        Just msg -> [msg]
                        _ -> []
                    ]
        in            -}
    let handleLefts l = case partitionEithers l of
            ([], _) -> True
            (a, _) -> False -- error $ concat a
     in handleLefts
            -- TODO: why so much calls(prints) in tests?
            [ checkEndpointToIntermidiateRelation' (getEpMap pu) (getInterMap pu) pu
            , checkInstructionToEndpointRelation (getInstrMap pu) (getEpMap pu) $ process pu
            , checkCadToFunctionRelation (getCadFunctions pu) (getCadSteps pu) $ process pu
            ]

-- at the moment check LoopBegin/End
checkCadToFunctionRelation cadFs cadSt pr = Right $ S.isSubsetOf makeCadVertical rels
    where
        rels = S.fromList $ filter isVertical $ relations pr
        showLoop f = "bind " <> show f
        makeCadVertical =
            S.fromList $
                concatMap
                    ( \(h, f) ->
                        concatMap
                            ( \v -> [uncurry Vertical (h, fst $ cadSt M.! v)]
                            )
                            $ showLoop f
                    )
                    $ M.toList cadFs

-- checkEndpointToIntermidiateRelation a b c = undefined

checkEndpointToIntermidiateRelation' eps ifs pu =
    let res = any (`S.isSubsetOf` rels) genRels
        genRels = makeRelationList eps ifs
        rels = S.fromList $ filter isVertical $ relations $ process pu
        biggestInter = getBiggestIntersection genRels rels
        --checkIfsEmpty = not (M.size eps > 0 && M.size ifs == 0) || error "Functions are empty. "
        checkIfsEmpty = (M.size eps > 0 && M.size ifs == 0)
        --checkEpsEmpty = not (M.size ifs > 0 && M.size eps == 0) || error "Endpoints are empty. "
        checkEpsEmpty = (M.size ifs > 0 && M.size eps == 0)
     in if res && checkIfsEmpty && checkEpsEmpty
            then Right res
            else checkTransportToIntermidiateRelation pu ifs rels

-- Lazy variant which don't take into account relation between @PU
-- TODO: add map with endpoints (as Source) to be sure that function is connected to endpoint after all
checkTransportToIntermidiateRelation pr ifs rels =
    let res = any (`S.isSubsetOf` rels) makeRelationList
        transM = getTransportMap pr
        makeRelationList =
            map S.fromList $
                concatMap
                    ( \(h, f) ->
                        concatMap
                            ( \v -> [[Vertical h $ fst $ transM M.! v]]
                            )
                            $ variables f
                    )
                    $ M.toList ifs
     in if res
            then Right res
            else Left "Endpoint to Intermideate (function) not consistent"

-- M.Map  ProcessStepID (a, (ProcessStepID, Instruction (BusNetwork String a x1 t1)))
getTransportMap pu =
    let getTransport :: (Typeable a, Typeable v, Typeable x, Typeable t) => pu v x t -> a -> Maybe (Instruction (BusNetwork String v x t))
        getTransport _ = cast
        filterTransport pu' pid (InstructionStep ins)
            | Just instr@(Transport v _ _) <- getTransport pu' ins = Just (v, (pid, instr))
            | otherwise = Nothing
        filterTransport _ _ _ = Nothing
     in M.fromList $ mapMaybe (uncurry $ filterTransport pu) $ M.toList $ getInstrMap pu

-- WHAT IDEA? I forgot why i added it
getBiggestIntersection genRels rels =
    map (maximumBy $ comparing S.size) [map (`S.intersection` rels) genRels]

-- FIX: S.isSubsetOf [] rels   ; produces True

makeRelationList eps ifs =
    let fuu =
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
     in --in Debug.Trace.traceShow fuu fuu
        fuu

checkInstructionToEndpointRelation ins eps pr =
    let rels = S.fromList $ map (\(Vertical r1 r2) -> (r1, r2)) $ filter isVertical $ relations pr
        checkInsEmpty = not (M.size eps > 0 && M.size ins == 0) || error "Instructions are empty. "
        checkEpsEmpty = not (M.size ins > 0 && M.size eps == 0) || error "Endpoints are empty. "
        eps' = M.fromList $ concat $ M.elems eps
        makeRelationList =
            concatMap
                ( \(r1, r2) -> case eps' M.!? r1 of -- TODO could be two sided relation
                    Just _ | Just (InstructionStep _) <- ins M.!? r2 -> [True]
                    _ -> []
                )
                rels
     in Right $ and $ makeRelationList <> [checkInsEmpty, checkEpsEmpty]

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

-- (pid, f)
getCadFunctions pu =
    let filterCad (_, f)
            | Just Loop{} <- castF f = True
            | Just (LoopBegin Loop{} _) <- castF f = True
            | Just (LoopEnd Loop{} _) <- castF f = True
            | otherwise = False
     in M.fromList $ filter filterCad $ M.toList $ getInterMap pu

-- TODO: add Maybe?
-- (Loop (pid, f)) , where Loop is show instance
getCadSteps pu =
    M.fromList $
        concat
            [ concatMap (\l -> [(l, (pID, step))]) pDesc'
            | step@Step{pID} <- steps $ process pu
            , pDesc' <- case getCAD step of
                Just msg -> [msg]
                _ -> []
            ]
