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
module NITTA.Model.ProcessorUnits.Tests.IntegrityCheck (
    checkIntegrity,
) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import NITTA.Intermediate.Types
import NITTA.Model.ProcessorUnits

checkIntegrity pu fs =
    let pr = process pu

        getFuncMap = foldr M.union M.empty filterFunc
        filterFunc = [M.insert pid (pid, f) M.empty | (FStep f) <- map pDesc $ steps pr, pid <- map pID $ steps pr]

        getEpMap = foldr M.union M.empty filterEp
        filterEp = [M.insert pid (pid, variables e) M.empty | (EndpointRoleStep e) <- map pDesc $ steps pr, pid <- map pID $ steps pr]
     in and [checkIntermidiateToEndpointRelation getFuncMap getEpMap pu]

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