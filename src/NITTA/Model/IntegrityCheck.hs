{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Tests.IntegrityCheck
Description : Tests vertical relations in PU
Copyright   : (c) co0ll3r, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.IntegrityCheck where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Debug.Trace
import NITTA.Intermediate.Types
import NITTA.Model.ProcessorUnits

--checkIntegrity :: ProcessorUnit (Process t1 i) ProcessStepID x t2 => Process t1 i -> p -> Bool
--checkIntegrity :: (ProcessorUnit u v x t, Suffix ProcessStepID) => u -> p -> Bool
checkIntegrity pu fs =
    let pr = process $ Debug.Trace.traceShow pu pu
        getFuncMap = foldr M.union M.empty filterFunc
        filterFunc = [M.insert pid (pid, f) M.empty | (FStep f) <- map pDesc filterFunc', pid <- map pID filterFunc']
        filterFunc' = filter (\stp -> case pDesc stp of FStep _ -> True; _ -> False) $ steps pr

        getEpMap = foldr M.union M.empty filterEp
        filterEp = [M.insert pid (pid, variables e) M.empty | (EndpointRoleStep e) <- map pDesc filterEp', pid <- map pID filterEp']
        filterEp' = filter (\stp -> case pDesc stp of EndpointRoleStep _ -> True; _ -> False) $ steps pr
     in and [checkIntermidiateToEndpointRelation getFuncMap getEpMap pr]

toIntermidiateMap pu =
    let getInterMap = foldr findIntermidiate M.empty $ steps pr
        pr = process pu
     in if M.null getInterMap
            then Nothing
            else Just getInterMap
findIntermidiate step m = case pDesc step of
    (FStep fun) -> M.insert (pID step) fun m
    _ -> m

getFuncMap pu = foldr M.union M.empty $ filterFunc $ process pu
filterFunc pr = [M.insert pid (pid, f) M.empty | (FStep f) <- map pDesc $ filterFunc' pr, pid <- map pID $ filterFunc' pr]
filterFunc' pr = filter (\stp -> case pDesc stp of FStep _ -> True; _ -> False) $ steps pr

getEpMap pu = foldr M.union M.empty $ filterEp $ process pu
filterEp pr = [M.insert pid (pid, variables e) M.empty | step <- filterEp' pr, let e = getEp $ pDesc step, let pid = pID step]
filterEp' pr = filter (\stp -> case pDesc stp of EndpointRoleStep _ -> True; _ -> False) $ steps pr

getEp (EndpointRoleStep e) = e
getEp _ = error ""

-- checkIntermidiateToEndpointRelation :: (Ord k1, Variables a k1) => M.Map k2 (ProcessStepID, a) -> M.Map k1 (ProcessStepID, b) -> Process t i -> Bool

checkIntermidiateToEndpointRelation fs eps pu = S.isSubsetOf makeRelationList fromRelation
    where
        fromRelation = S.fromList $ map (\(Vertical r1 r2) -> (r1, r2)) $ relations pu
        makeRelationList =
            S.fromList $
                concatMap
                    ( \(h, f) ->
                        concatMap
                            ( \v -> [(h, fst (eps M.! v))]
                            )
                            $ variables f
                    )
                    $ M.elems fs