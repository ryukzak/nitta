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
import NITTA.Utils.Base

checkIntegrity fs pu =
    let pr = process pu
        vars = unionsMap variables fs
        -- TODO remove head
        funcPID = checkFunction (head fs) $ steps pr
     in checkEndpoints vars pr funcPID

-- | Find requested function f in steps of a given process
checkFunction f [] = error [i|Requested function not found: #{ show f }."|]
checkFunction f (stp : stps) =
    let compFun (FStep F{fun}) = T.pack (show f) == T.replace (T.pack "\"") T.empty (T.pack $ show fun)
        compFun _ = False
     in if compFun $ pDesc stp
            then pID stp
            else checkFunction f stps

-- | For a given pID finds and returns pIDs of Steps which have Endpoints
checkEndpoints vars pr pid =
    let (foundRel, foundPid) = checkRelationsEp pid (steps pr) vars $ relations pr
     in if vars == foundRel
            then foundPid
            else
                error
                    [__i|
                    Not all variables has related Endpoints:
                    expected: [ #{ show vars } ];
                       found: [ #{ show foundRel } ]
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

checkTransports f pr pids =
    let concRes (epVars, epPid) (epVars2, epPid2) = (S.union epVars epVars2, epPid ++ epPid2)
     in foldr concRes (S.empty, []) [checkRelationsEp pid (steps pr) (variables f) $ relations pr | pid <- pids]