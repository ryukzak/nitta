{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}

{- |
Module      : NITTA.Model.IntegrityCheck
Description : Module for checking model description consistency
Copyright   : (c) Artyom Kostyuchik, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.IntegrityCheck (
    checkIntegrity,
) where

import Data.List (find)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Debug.Trace
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus (Instruction (Transport))
import NITTA.Model.ProcessorUnits
import NITTA.Utils
import NITTA.Utils.ProcessDescription

checkIntegrity pu =
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

        getTransportMap =
            let filterTransport pu' (InstructionStep ins)
                    | Just (Transport v _ _) <- castInstruction pu' ins = Just v
                    | otherwise = Nothing
                filterTransport _ _ = Nothing
             in M.mapMaybe (filterTransport pu) getInstrMap

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
     in and
            -- TODO: why so much calls(prints) in tests?
            [ checkEndpointToIntermidiateRelation getEpMap getInterMap $ process pu
            , checkInstructionToEndpointRelation getInstrMap getEpMap $ process pu
            , checkCadToFunctionRelation getCadFunctions getCadSteps $ process pu
            ]

-- at the moment check LoopBegin/End
checkCadToFunctionRelation cadFs cadSt pr = S.isSubsetOf makeCadVertical rels
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

-- FIX: S.isSubsetOf [] rels   ; produces True
checkEndpointToIntermidiateRelation eps ifs pr = and [checkRels, checkIfsEmpty, checkEpsEmpty]
    where
        checkRels = any (`S.isSubsetOf` rels) $ makeRelationList2 eps ifs
        checkIfsEmpty = not ((M.size eps > 0) && (M.size ifs == 0)) || error "Functions are empty. "
        checkEpsEmpty = not ((M.size ifs > 0) && (M.size eps == 0)) || error "Endpoints are empty. "
        rels = S.fromList $ filter isVertical $ relations pr
        findRel (h, l) =
            map (uncurry Vertical) $
                case find (\(k, _) -> Vertical h k `elem` rels) l of
                    Just res -> [(h, fst res)]
                    Nothing -> error $ "Can't find Endpoint for Function with pID: " <> show [h]
        makeRelationList =
            S.fromList $
                concatMap
                    ( \(h, f) ->
                        concat $
                            concatMap
                                ( \v -> [findRel (h, eps M.! v)]
                                )
                                $ variables f
                    )
                    $ M.toList ifs

makeRelationList2 eps ifs =
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
     in Debug.Trace.traceShow fuu fuu
checkInstructionToEndpointRelation ins eps pr = and $ makeRelationList <> [checkInsEmpty, checkEpsEmpty]
    where
        rels = S.fromList $ map (\(Vertical r1 r2) -> (r1, r2)) $ filter isVertical $ relations pr
        checkInsEmpty = not ((M.size eps > 0) && (M.size ins == 0)) || error "Instructions are empty. "
        checkEpsEmpty = not ((M.size ins > 0) && (M.size eps == 0)) || error "Endpoints are empty. "
        eps' = M.fromList $ concat $ M.elems eps
        makeRelationList =
            concatMap
                ( \(r1, r2) -> case eps' M.!? r1 of -- TODO could be two sided relation
                    Just _ | Just (InstructionStep _) <- ins M.!? r2 -> [True]
                    _ -> []
                )
                rels
