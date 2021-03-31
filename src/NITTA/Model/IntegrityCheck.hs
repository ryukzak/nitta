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
            M.fromList $
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
            M.fromList $
                [ (pID, (pID, pDesc))
                | Step{pID, pDesc} <- steps pr
                , isInstruction pDesc
                ]
        getRels = S.fromList $ map (\(Vertical r1 r2) -> (r1, r2)) $ relations pr
     in and
            [ checkEndpointToIntermidiateRelation getEpMap getInterMap getRels
            , checkInstructionToEndpointRelation getInstrMap getEpMap getRels
            ]

-- TODO: remove?
checkIntermidiateToFunctionRelation pu fs =
    let fsVars = unionsMap variables fs
        puFuncVars = unionsMap variables $ getFBs (process pu)
     in fsVars == transferred pu
            && fsVars == puFuncVars

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

checkEndpointToInstructionRelation eps ins pr =
checkInstructionToEndpointRelation ins eps rels =
    let eps_ = M.fromList $ M.elems eps
        ins_ = M.fromList $ M.elems ins
        checkRel (v1, v2) = case eps_ M.!? v1 of
            Just _ | Just (InstructionStep _) <- ins_ M.!? v2 -> [True]
            _ -> []
     in and $ concatMap checkRel rels