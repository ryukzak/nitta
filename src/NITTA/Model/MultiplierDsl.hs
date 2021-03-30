{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}

{- |
Module      : NITTA.Model.MultiplierDsl
Description : Provides functions to make decisions in Multiplier
Copyright   : (c) Artyom Kostyuchik, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.MultiplierDsl (
    evalMultiplier,
    bindFunc,
    doDecisionSafe,
    doFstDecision,
    doNDecision,
    beTarget,
    beTargetAt,
    beSource,
    beSourceAt,
    assertBindFullness,
    assertProcessDone,
    assertExecute,
) where

import Control.Monad.State.Lazy
import qualified Data.Set as S
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Utils
import Numeric.Interval.NonEmpty hiding (elem)

data UnitTestState pu v x = UnitTestState
    { unit :: pu
    , functs :: [F v x]
    }
    deriving (Show)

evalMultiplier st alg = evalState alg (UnitTestState st [])

bindFunc f = do
    st@UnitTestState{unit, functs} <- get
    case tryBind f unit of
        Right unit_ -> put st{unit = unit_, functs = f : functs}
        Left err -> error err

doDecisionSafe endpSt = do
    st@UnitTestState{unit} <- get
    let isAvailable = isEpOptionAvailable endpSt unit
    if isAvailable
        then put st{unit = endpointDecision unit endpSt}
        else error $ "Such option isn't available: " <> show endpSt <> "; from list: " <> show (endpointOptions unit)

isEpOptionAvailable (EndpointSt v interv) pu =
    let compEpRoles = case v of
            (Target _) -> v `elem` map epRole (endpointOptions pu)
            (Source s) -> S.isSubsetOf s $ unionsMap (variables . epRole) $ endpointOptions pu
        compIntervs = singleton (nextTick $ process pu) <=! interv
     in compEpRoles && compIntervs

doFstDecision :: (MonadState (UnitTestState pu v1 x) m, EndpointProblem pu v2 t2, Num t2, Ord t2) => m ()
doFstDecision = doNDecision 0

doNDecision n = do
    UnitTestState{unit} <- get
    doDecision $ nDecision unit n

nDecision pu i =
    let opts = endpointOptions pu
        i_ =
            if i < length opts
                then i
                else error $ "nDecision out of bound: provided i=" <> show i <> "; options lenght=" <> show (length opts)
     in endpointOptionToDecision $ opts !! i_

doDecision endpSt = do
    st@UnitTestState{unit} <- get
    put st{unit = endpointDecision unit endpSt}

-- TODO FIx
beTarget t = do
    UnitTestState{unit} <- get
    return $ EndpointSt (Target t) $ getInterval' unit

-- TODO FIx
beSource ss = do
    UnitTestState{unit} <- get
    return $ EndpointSt (Source $ S.fromList ss) $ getInterval' unit

getInterval' pu = singleton $ nextTick $ process pu

beTargetAt a b t = EndpointSt (Target t) (a ... b)

beSourceAt a b ss = EndpointSt (Source $ S.fromList ss) (a ... b)

assertBindFullness :: (MonadState (UnitTestState b v x) m, ProcessorUnit b v x t, WithFunctions b (F v x)) => m Bool
assertBindFullness = do
    UnitTestState{unit, functs} <- get
    if isFullyBinded unit functs
        then return True
        else error $ "Function is not binded to process! expected: " <> show functs <> "; actual: " <> show (functions unit)

isFullyBinded :: (WithFunctions a1 a2, Ord v2, Function a2 v2, Label a2, Function f v2, Label f) => a1 -> [f] -> Bool
isFullyBinded pu fs =
    let fu = functions pu
        outs = S.fromList $ map outputs fu
        inps = S.fromList $ map inputs fu
        sign = S.fromList $ map label fu
     in not (null fu)
            && outs == S.fromList (map outputs fs)
            && inps == S.fromList (map inputs fs)
            && sign == S.fromList (map label fs)

assertProcessDone :: (ProcessorUnit pu v x t, EndpointProblem pu v x) => State (UnitTestState pu v x) Bool
assertProcessDone =
    do
        UnitTestState{unit, functs} <- get
        if isProcessComplete' unit functs
            && null (endpointOptions unit)
            then return True
            else error "Process is not complete"

assertExecute :: (ProcessorUnit pu v x t) => State (UnitTestState pu v x) Bool
assertExecute = do
    UnitTestState{unit} <- get
    let nT = nextTick $ process unit
        nU = nextUid $ process unit
    return $ nT >= 0 && nU >= 0

-- TODO: clean/combine with utils
isProcessComplete' pu fs = unionsMap variables fs == processedVars' pu
processedVars' pu = unionsMap variables $ getEndpoints $ process pu