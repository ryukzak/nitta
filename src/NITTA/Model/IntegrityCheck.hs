{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Tests.IntegrityCheck
Description : Tests vertical relations in PU
Copyright   : (c) co0ll3r, 2021
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

checkIntegrity pu fs =
    let pr = process pu
        getFuncMap =
            M.fromList $
                [ (pID, (pID, f))
                | step@Step{pID, pDesc} <- steps pr
                , isFB step
                , f <- case pDesc of
                    (FStep f) -> [f]
                    _ -> []
                ]
        --- TODO what if we have 2 same variables?
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
     in and [checkIntermidiateToEndpointRelation getFuncMap getEpMap pr]

checkIntermidiateToEndpointRelation fs eps pr = S.isSubsetOf makeRelationList fromRelation
    where
        fromRelation = S.fromList $ map (\(Vertical r1 r2) -> (r1, r2)) $ relations pr
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
