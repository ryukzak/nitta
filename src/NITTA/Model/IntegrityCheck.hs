{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Model.IntegrityCheck
Description : Module for checking PU model description consistency
Copyright   : (c) Artyom Kostyuchik, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.IntegrityCheck where

import Control.Monad
import Data.Data
import Data.Either
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.String (IsString)
import Data.String.ToString (ToString)
import qualified Debug.Trace
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus (BusNetwork, Instruction (Transport))
import NITTA.Model.ProcessorUnits
import NITTA.Utils

class ProcessConsistent u where
    checkProcessСonsistent :: u -> Either String ()

instance (Typeable t, Typeable x, ProcessorUnit (pu v x t) v x2 t2) => ProcessConsistent (pu v x t) where
    checkProcessСonsistent pu =
        let consistent = checkIntegrity' pu
         in if any isLeft consistent
                then Left $ concat $ lefts consistent
                else Right ()

instance (ProcessorUnit u v x t, Typeable u, Ord u, ToString u, IsString u) => ProcessConsistent (BusNetwork u v x t) where
    checkProcessСonsistent pu =
        let consistent = checkIntegrity' pu
         in if any isLeft consistent
                then Left $ concat $ lefts consistent
                else Right ()

handleLefts l = case partitionEithers l of
    ([], _) -> True
    -- ([], _) -> Debug.Trace.traceShow "Success" True
    -- (a, _) -> Debug.Trace.traceShow ("Err msg: " <> concat a) False
    (a, _) -> False

checkIntegrity pu = handleLefts $ checkIntegrity' pu

checkIntegrity' pu =
    -- TODO: why so much calls(prints) in tests?
    [ checkEndpointToIntermidiateRelation (getEpMap pu) (getInterMap pu) pu
    , checkInstructionToEndpointRelation (getInstrMap pu) (getEpMap pu) $ process pu
    , checkCadToFunctionRelation (getCadFunctions pu) (getCadSteps pu) pu
    ]

checkEndpointToIntermidiateRelation eps ifs pu =
    let rels = S.fromList $ filter isVertical $ relations $ process pu
        checkIfsEmpty = M.size eps > 0 && M.size ifs == 0
        checkEpsEmpty = M.size ifs > 0 && M.size eps == 0
        lookup' v = fromMaybe (showError "Endpoint to Intermidiate" "enpoint" v) $ eps M.!? v
        makeRelationList =
            map S.fromList $
                concatMap
                    ( \(h, f) ->
                        sequence $
                            concatMap
                                ( \v -> [[Vertical h $ fst p | p <- lookup' v]]
                                )
                                $ variables f
                    )
                    $ M.toList ifs
     in do
            when checkIfsEmpty $ Left "functions are empty"
            when checkEpsEmpty $ Left "eps are empty"
            if any (`S.isSubsetOf` rels) makeRelationList
                then Right True
                else checkTransportToIntermidiateRelation pu ifs rels

-- TODO: add map with endpoints (as Source) to be sure that function is connected to endpoint after all
--       it means: Endpoint (Source) -> Transport -> Function
checkTransportToIntermidiateRelation pu ifs rels =
    let transMap = getTransportMap pu
        lookup' v = fromMaybe (showError "Transport to Intermidiate" "transport" v) $ transMap M.!? v
        makeRelationList =
            map S.fromList $
                concatMap
                    ( \(h, f) ->
                        concatMap
                            ( \v -> [[Vertical h $ fst $ lookup' v]]
                            )
                            $ variables f
                    )
                    $ M.toList ifs
     in if any (`S.isSubsetOf` rels) makeRelationList
            then Right True
            else Left "Endpoint and Transport to Intermideate (function) not consistent"

checkInstructionToEndpointRelation ins eps pr =
    let checkInsEmpty = M.size eps > 0 && M.size ins == 0
        checkEpsEmpty = M.size ins > 0 && M.size eps == 0
        eps' = M.fromList $ concat $ M.elems eps
        rels = S.fromList $ map (\(Vertical r1 r2) -> (r1, r2)) $ filter isVertical $ relations pr
        consistent =
            and $
                concatMap
                    ( \(r1, r2) -> case eps' M.!? r1 of
                        Just _ | Just (InstructionStep _) <- ins M.!? r2 -> [True]
                        _ -> []
                    )
                    rels
     in do
            when checkInsEmpty $ Left "instructions are empty"
            when checkEpsEmpty $ Left "enpoints are empty"
            if consistent
                then Right True
                else Left "Instruction to Endpoint not consistent"

-- now it checks LoopBegin/End
checkCadToFunctionRelation cadFs cadSteps pu =
    let consistent = S.isSubsetOf makeCadVertical rels
        rels = S.fromList $ filter isVertical $ relations $ process pu
        showLoop f = "bind " <> show f
        lookup' v = fromMaybe (showError "CAD" "steps" v) $ cadSteps M.!? v
        makeCadVertical =
            S.fromList $
                concatMap
                    ( \(h, f) ->
                        concatMap
                            ( \v -> [uncurry Vertical (lookup' v, h)]
                            )
                            [showLoop f]
                    )
                    $ M.toList cadFs
     in if consistent
            then Right True
            else Left $ "CAD functions not consistent. Excess:" <> show (S.difference makeCadVertical rels)

getInterMap pu =
    M.fromList
        [ (pID, f)
        | step@Step{pID} <- steps $ process pu
        , isFB step
        , f <- case getFunction step of
            Just f -> [f]
            _ -> []
        ]

getEpMap pu =
    M.fromListWith (++) $
        concat
            [ concatMap (\v -> [(v, [(pID, ep)])]) $ variables ep
            | step@Step{pID} <- steps $ process pu
            , isEndpoint step
            , ep <- case getEndpoint step of
                Just e -> [e]
                _ -> []
            ]

getInstrMap pu =
    M.fromList
        [ (pID, instr)
        | step@Step{pID} <- steps $ process pu
        , isInstruction step
        , instr <- case getInstruction step of
            Just i -> [i]
            _ -> []
        ]

getTransportMap pu =
    let getTransport :: (Typeable a, Typeable v, Typeable x, Typeable t) => pu v x t -> a -> Maybe (Instruction (BusNetwork String v x t))
        getTransport _ = cast
        filterTransport pu' pid (InstructionStep ins)
            | Just instr@(Transport v _ _) <- getTransport pu' ins = Just (v, (pid, instr))
            | otherwise = Nothing
        filterTransport _ _ _ = Nothing
     in M.fromList $ mapMaybe (uncurry $ filterTransport pu) $ M.toList $ getInstrMap pu

getCadFunctions pu =
    let filterCad (_, f)
            | Just Loop{} <- castF f = True
            | Just (LoopBegin Loop{} _) <- castF f = True
            | Just (LoopEnd Loop{} _) <- castF f = True
            | otherwise = False
     in M.fromList $ filter filterCad $ M.toList $ getInterMap pu

getCadSteps pu =
    M.fromList
        [ (pDesc', pID)
        | step@Step{pID} <- steps $ process pu
        , pDesc' <- case getCAD step of
            Just msg -> [msg]
            _ -> []
        ]

showError name mapName v =
    error $
        name
            <> " relations contain error: "
            <> show v
            <> " is not present in "
            <> mapName
            <> " map."
