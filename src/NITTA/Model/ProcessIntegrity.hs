{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Model.ProcessIntegrity
Description : Checking the target process integrity
Copyright   : (c) Artyom Kostyuchik, Aleksandr Penskoi, 2022
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessIntegrity (
    ProcessIntegrity (checkProcessIntegrity),
    isProcessIntegrity,
) where

import Data.Either
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import qualified Data.String.Utils as S
import NITTA.Model.ProcessorUnits
import NITTA.Utils

class ProcessIntegrity u where
    checkProcessIntegrity :: u -> Either String ()

isProcessIntegrity u = isRight $ checkProcessIntegrity u

instance (ProcessorUnit (pu v x t) v x t) => ProcessIntegrity (pu v x t) where
    checkProcessIntegrity pu =
        collectChecks
            [ checkVerticalRelations (up2down pu) (pid2intermediate pu) (pid2endpoint pu) "intermediate not related to endpoint"
            , checkVerticalRelations (down2up pu) (pid2endpoint pu) (pid2intermediate pu) "endpoint not related to intermediate"
            , checkVerticalRelations (up2down pu) (pid2endpoint pu) (pid2instruction pu) "endpoint not related to instruction"
            , checkVerticalRelations (down2up pu) (pid2instruction pu) (pid2endpoint pu) "instruction not related to endpoint"
            ]

checkVerticalRelations f dom codom errmsg =
    collectChecks $
        map
            ( \x ->
                let ys = M.findWithDefault S.empty x f
                 in if any (`M.member` codom) $ S.elems ys
                        then Right ()
                        else Left $ errmsg <> ": " <> show (dom M.! x)
            )
            $ M.keys dom

-- TODO: #205 Divider: missing vertical relation between Do instruction and Endpoint
skipIntegrityErrors = ["instruction not related to endpoint: Instruction: Do"]

collectChecks checks = case lefts checks of
    [] -> Right ()
    errs -> case filter (`L.notElem` skipIntegrityErrors) errs of
        [] -> Right ()
        errs' -> Left $ S.join "; " errs'

relationsMap pairs = M.fromList $ map merge $ L.groupBy (\a b -> fst a == fst b) $ L.sortOn fst pairs
    where
        merge xs@((a, _) : _) = (a, S.fromList $ map snd xs)
        merge _ = error "internal error"

up2down pu = relationsMap $ mapMaybe get $ relations $ process pu
    where
        get Vertical{vUp, vDown} = Just (vUp, vDown)
        get _ = Nothing

down2up pu = relationsMap $ mapMaybe get $ relations $ process pu
    where
        get Vertical{vUp, vDown} = Just (vDown, vUp)
        get _ = Nothing

pid2intermediate pu = M.fromList $ mapMaybe get $ steps $ process pu
    where
        get s@Step{pID}
            | Just f <- getIntermediate s = Just (pID, f)
            | otherwise = Nothing

pid2endpoint pu = M.fromList $ mapMaybe get $ steps $ process pu
    where
        get s@Step{pID}
            | Just ep <- getEndpoint s = Just (pID, ep)
            | otherwise = Nothing

pid2instruction pu = M.fromList $ mapMaybe get $ steps $ process pu
    where
        get s@Step{pID}
            | Just instr <- getInstruction s = Just (pID, instr)
            | otherwise = Nothing
