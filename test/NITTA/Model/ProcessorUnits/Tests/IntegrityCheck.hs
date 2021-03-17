{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

{- |
Module      : NITTA.Model.Microarchitecture
Description : Create micro architecture functions
Copyright   : (c) Daniil Prohorov, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Tests.IntegrityCheck (
    checkIntegrity,
) where

import qualified Data.Set as S
import Data.String.Interpolate
import qualified Data.Text as T
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits

checkIntegrity pu fs =
    let pr = process pu
        vars = map variables fs
        pids = checkFunction fs $ steps pr
     in concat [checkEndpoints var pr pid | pid <- pids, var <- vars]

{- | Find requested functions fs in steps of a given process
 | if not found then error
-}
checkFunction fs stps =
    let nullSteps f =
            if not . null $ iterSteps f
                then iterSteps f
                else
                    error
                        [__i| Requested function not found: #{ show fs }
                                                  in steps: #{ show stps }
                        |]
        iterSteps f = [pID stp | stp <- stps, compFun f $ pDesc stp]
        compFun f (FStep F{fun}) = T.pack (show f) == T.replace (T.pack "\"") T.empty (T.pack $ show fun)
        compFun _ _ = False
     in concatMap nullSteps fs

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
                          pr: [ #{pr} ]
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
