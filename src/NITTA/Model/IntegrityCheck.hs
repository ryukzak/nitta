{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}

{- |
Module      : NITTA.Model.IntegrityCheck
Description : Tests vertical relations in PU
Copyright   : (c) Artyom Kostyuchik, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.IntegrityCheck (
    checkIntegrity,
) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import NITTA.Intermediate.Types
import NITTA.Model.ProcessorUnits
import NITTA.Utils
import Safe

checkIntegrity pu =
    let pr = process pu
        getInterMap =
            M.fromList
                [ (pID, f)
                | step@Step{pID, pDesc} <- steps pr
                , isFB step
                , f <- case pDesc of
                    (FStep f) -> [f]
                    _ -> []
                ]
        getEpMap =
            M.fromListWith (++) $
                concat
                    [ concatMap (\v -> [(v, [(pID, ep)])]) $ variables ep
                    | step@Step{pID, pDesc} <- steps pr
                    , isEndpoint step
                    , ep <- case pDesc of
                        (EndpointRoleStep e) -> [e]
                        _ -> []
                    ]
        getInstrMap =
            M.fromList
                [ (pID, pDesc)
                | Step{pID, pDesc} <- steps pr
                , isInstruction pDesc
                ]
     in and
            [ checkEndpointToIntermidiateRelation getEpMap getInterMap pr
            , checkInstructionToEndpointRelation getInstrMap getEpMap pr
            , True
            , True
            ]

checkEndpointToIntermidiateRelation eps ifs pr = S.isSubsetOf makeRelationList rels
    where
        rels = S.fromList $ filter isVertical $ relations pr
        findRel (h, l) =
            if length l > 1
                then Vertical h $ fst $ findJust (\(k, _) -> Vertical h k `elem` rels) l
                else Vertical h $ fst $ head l
        makeRelationList =
            S.fromList $
                concatMap
                    ( \(h, f) ->
                        concatMap
                            ( \v -> [findRel (h, eps M.! v)]
                            )
                            $ variables f
                    )
                    $ M.toList ifs

checkInstructionToEndpointRelation ins eps pr = and makeRelationList
    where
        rels = S.fromList $ map (\(Vertical r1 r2) -> (r1, r2)) $ filter isVertical $ relations pr
        eps' = M.fromList $ concat $ M.elems eps
        makeRelationList =
            concatMap
                ( \(r1, r2) -> case eps' M.!? r1 of
                    Just _ | (InstructionStep _) <- ins M.! r2 -> [True]
                    _ -> []
                )
                rels
