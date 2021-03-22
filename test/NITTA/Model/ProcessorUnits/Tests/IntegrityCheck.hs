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
import NITTA.Model.ProcessorUnits

checkIntegrity pu fs =
    let pr = process pu
        vars = foldr (S.union . variables) S.empty fs

        toIntermidiateMap =
            let getInterMap = foldr findIntermidiate M.empty $ steps pr
             in if M.null getInterMap
                    then Nothing
                    else Just getInterMap
        findIntermidiate step m = case pDesc step of
            (FStep fun) -> M.insert (pID step) fun m
            _ -> m

        toEndpointMap =
            let getEpMap = foldr findEndpoint M.empty $ steps pr
             in if foldr S.union S.empty (M.elems getEpMap) == vars
                    then Just getEpMap
                    else Nothing
        findEndpoint step m = case pDesc step of
            (EndpointRoleStep t) -> M.insert (pID step) (variables t) m
            _ -> m

        toInstructionMap =
            let getInstrMap = foldr findInstruction M.empty $ steps pr
             in if M.null getInstrMap
                    then Nothing
                    else Just getInstrMap
        findInstruction step m = case pDesc step of
            instr@(InstructionStep _) -> M.insert (pID step) instr m
            _ -> m
     in not $
            null $
                checkEndpointToInstructionRelation pr toEndpointMap toInstructionMap $
                    checkIntermidiateToEndpointRelation pr toIntermidiateMap toEndpointMap

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
            else printError epMap checkRelation

-- TODO is it possible to combine with checkIntermidiateToEndpointRelation?
checkEndpointToInstructionRelation _ Nothing _ _ = error "Not all variables have their Endpoint!"
checkEndpointToInstructionRelation _ _ Nothing _ = error "Instruction steps not found!"
checkEndpointToInstructionRelation pr (Just epMap) (Just isMap) epPids =
    let checkRelation = foldr compRel S.empty $ relations pr

        compRel (Vertical r1 r2) =
            S.union $
                compRel' r2 (M.lookup r1 epMap) $ M.lookup r2 isMap
        compRel' pid (Just _) (Just _) = S.fromList [pid]
        compRel' _ _ _ = S.empty
     in if S.size checkRelation == S.size epPids
            then checkRelation
            else printError epMap checkRelation

printError epMap foundPids =
    error --- TODO pretty print
        [i| Steps #{ M.withoutKeys epMap foundPids } 
                          not related to any FStep!|]
