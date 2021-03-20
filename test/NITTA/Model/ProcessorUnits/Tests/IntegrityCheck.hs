{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Tests.IntegrityCheck
Description : Tests vertical relations in PU
Copyright   : (c) co0ll3r, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Tests.IntegrityCheck (
    checkIntegrity,
) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.String.Interpolate
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits

checkIntegrity pu fs =
    let pr = process pu
        vars = foldr (S.union . variables) S.empty fs
        -- TODO: possible to rewrite without unpacking?
        toIntermidiateMap =
            if M.null getIntermidiateMap
                then Nothing
                else Just getIntermidiateMap
        getIntermidiateMap = foldr findIntermidiate M.empty $ steps pr
        findIntermidiate step m = case pDesc step of
            (FStep fun) -> M.insert (pID step) fun m
            _ -> m

        --- TODO pattern match at params
        toEndpointMap =
            let check =
                    if foldr S.union S.empty (M.elems eps) == vars
                        then Just eps
                        else Nothing
                eps = foldr findEndpoint M.empty $ steps pr
             in check
        findEndpoint step m = case pDesc step of
            (EndpointRoleStep (Source t)) -> M.insert (pID step) t m
            (EndpointRoleStep (Target t)) -> M.insert (pID step) (S.fromList [t]) m
            _ -> m
     in checkIntermidiateToEndpointRelation pr toIntermidiateMap toEndpointMap

checkIntermidiateToEndpointRelation _ Nothing _ = error "No function found in steps of PU!"
checkIntermidiateToEndpointRelation _ _ Nothing = error "Not all variables have their Endpoint!"
checkIntermidiateToEndpointRelation pr (Just ieMap) (Just epMap) =
    let checkRelation = foldr compRel S.empty $ relations pr
        compRel (Vertical r1 r2) =
            S.union $
                compRel' r2 (M.lookup r1 ieMap) $ M.lookup r2 epMap
        compRel' pid (Just _) (Just _) = S.fromList [pid]
        compRel' _ _ _ = S.empty
     in if S.size checkRelation == M.size epMap
            then checkRelation
            else --- TODO pretty print

                error
                    [i| Steps #{ M.withoutKeys epMap checkRelation } 
                          not related to any FStep!|]

-- | Print
checkEndpointToInstructionRelation epMap isMap = undefined