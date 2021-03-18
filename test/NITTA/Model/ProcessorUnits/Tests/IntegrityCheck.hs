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
import Data.Maybe
import qualified Data.Set as S
import Data.String.Interpolate
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits

checkIntegrity pu fs =
    let pr = process pu
        -- TODO: possible to rewrite without unpacking?
        -- TODO check whetehr it null
        toIntermidiateMap = foldr findIntermidiate M.empty $ steps pr
        toEndpointMap = foldr findEndpoint M.empty $ steps pr
        --toInstructionMap = foldr findInstruction M.empty $ steps pr
        --- TODO pattern match at params
        findIntermidiate step m = case pDesc step of
            (FStep fun) -> M.insert (pID step) fun m
            _ -> m
        findEndpoint step m = case pDesc step of
            (EndpointRoleStep (Source s)) -> M.insert (pID step) s m
            (EndpointRoleStep (Target t)) -> M.insert (pID step) (S.fromList [t]) m
            _ -> m
     in {-
        findInstruction step m = case pDesc step of
            (InstructionStep instr) -> M.insert (pID step) instr m
            _ -> m
            -}
        checkIntermidiateToEndpointRelation pr fs toIntermidiateMap toEndpointMap

--     in concat [checkEndpoints var pr pid | pid <- pids, var <- vars]

checkCadToIntermidiate = undefined

checkIntermidiateToEndpointRelation pr fs ieMap epMap =
    let exec = foldr compRel S.empty rels
        rels = relations pr
        -- TODO is it work???
        vars = foldr (S.intersection . variables) S.empty fs
        compRel (Vertical r1 r2) s =
            S.union s $
                S.intersection vars $
                    compRel' (M.lookup r1 ieMap) $ M.lookup r2 epMap
        compRel _ _ = S.empty
        compRel' (Just fun) (Just ep) = ep
        -- TODO remove?
        compRel' (Just fun) Nothing = error "fun to Nothing"
        compRel' Nothing (Just ep) = error "Nothing to EP"
        compRel' _ _ = S.empty
     in exec

checkEndpointToInstructionRelation epMap isMap = undefined

{- | For a given pID finds and returns pIDs of Steps which have Endpoints
 | if pids not found then error
-}
checkEndpoints vars pr pid =
    let (foundRel, foundPid) = checkRelationsEp pid (steps pr) vars $ relations pr
     in if vars == foundRel
            then foundPid
            else
                error
                    [__i|
                    Not all variables has related Endpoints, 
                    function with pID=#{pid} should have more endpoints at least: #{S.difference vars foundRel};
                    expected: [ #{ show vars } ];
                       found: [ #{ show foundRel } ]
                        step: [ #{ reverse (steps pr) !! pid } ]
                    |]

-- | Goes through process relations and finds pIDs related to given pID
checkRelationsEp pid stps vars rels =
    let concEp (epVars, epPid) (epVars2, epPid2) = (S.union epVars epVars2, epPid : epPid2)
     in foldr concEp (S.empty, []) $
            [ checkStepsEp vars stps v2
            | (Vertical v1 v2) <- rels
            , pid == v1
            ]

{- | Finds given pid in pDesc of Steps, if it's Source or Target then returns it.
 | if not found gave error
-}
checkStepsEp vars stps pid =
    let combSteps = [S.intersection vars stp | stp <- map stepInfo stps, not $ null stp]
        stepInfo Step{pID, pDesc}
            | pID == pid = pDescInfo pDesc
            | otherwise = S.empty
        pDescInfo descr = case descr of
            (EndpointRoleStep (Source s)) -> s
            (EndpointRoleStep (Target t)) -> S.fromList [t]
            _ -> S.empty
     in if not $ null combSteps
            then (head combSteps, pid)
            else error [i|Endpoint with pid=#{ show pid } not found in Steps: #{ show stps }|]
