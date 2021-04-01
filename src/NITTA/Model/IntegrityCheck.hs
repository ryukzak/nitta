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

checkIntegrity pu =
    let pr = process pu
        getInterMap =
            M.fromList
                [ (pID, (pID, f))
                | step@Step{pID, pDesc} <- steps pr
                , isFB step
                , f <- case pDesc of
                    (FStep f) -> [f]
                    _ -> []
                ]
        --- TODO what if we have 2 same variables? The key will be only one
        getEpMap =
            M.fromList $
                concat
                    [ concatMap (\v -> [(v, (pID, ep))]) $ variables ep
                    | step@Step{pID, pDesc} <- steps pr
                    , isEndpoint step
                    , ep <- case pDesc of
                        (EndpointRoleStep e) -> [e]
                        _ -> []
                    ]
        getInstrMap =
            M.fromList
                [ (pID, (pID, pDesc))
                | Step{pID, pDesc} <- steps pr
                , isInstruction pDesc
                ]
        getRels = S.fromList $ map (\(Vertical r1 r2) -> (r1, r2)) $ relations pr
     in and
            [ checkEndpointToIntermidiateRelation getEpMap getInterMap getRels
            , checkInstructionToEndpointRelation getInstrMap getEpMap getRels
            ]

checkEndpointToIntermidiateRelation eps ifs rels = S.isSubsetOf makeRelationList rels
    where
        makeRelationList =
            S.fromList $
                concatMap
                    ( \(h, f) ->
                        concatMap
                            ( \v -> [(h, fst (eps M.! v))]
                            )
                            $ variables f
                    )
                    $ M.elems ifs

checkInstructionToEndpointRelation ins eps rels = and makeRelationList
    where
        eps' = M.fromList $ M.elems eps
        ins' = M.fromList $ M.elems ins
        makeRelationList =
            concatMap
                ( \(r1, r2) -> case eps' M.!? r1 of
                    Just _ | (InstructionStep _) <- ins' M.! r2 -> [True]
                    _ -> []
                )
                rels
