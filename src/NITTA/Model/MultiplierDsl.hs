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
    doDecision,
    doDecisionSafe,
    doFstDecision,
    doNDecision,
    beTarget,
    beSource,
    assertProcessDone,
    fDef,
) where

import Control.Monad.State.Lazy
import qualified Data.Set as S
import qualified NITTA.Intermediate.Functions as F
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Utils
import Numeric.Interval.NonEmpty hiding (elem)

data UnitTestState pu x t = UnitTestState
    { unit :: pu
    , functs :: [F x t]
    }
    deriving (Show)

evalMultiplier st alg = evalState alg (UnitTestState st [])

evalMultiplierGet st alg = flip evalState (UnitTestState st []) $ do
    _ <- alg
    return <$ get

bindFunc f = do
    st@UnitTestState{unit, functs} <- get
    case tryBind f unit of
        Right unit_ -> put st{unit = unit_, functs = f : functs}
        Left err -> error err

doDecisionSafe endpSt = do
    isAvailable <- isEpOptionAvailable endpSt
    if isAvailable
        then doDecision endpSt
        else error ("Such option isn't available: " <> show endpSt)

isEpOptionAvailable (EndpointSt v inter) = do
    UnitTestState{unit} <- get
    return $
        v `elem` map epRole (endpointOptions unit)
            && nextTick (process unit) <=! inter

doDecision endpSt = do
    st@UnitTestState{unit} <- get
    put st{unit = endpointDecision unit endpSt}

doFstDecision = do
    UnitTestState{unit} <- get
    doDecision $ fstDecision unit

doNDecision n = do
    UnitTestState{unit} <- get
    doDecision $ nDecision unit n

fstDecision pu = endpointOptionToDecision $ head $ endpointOptions pu

nDecision pu n =
    let opts = endpointOptions pu
        calcN =
            if n <= length opts
                then n - 1
                else length (endpointOptions pu) - 1
     in endpointOptionToDecision $ opts !! calcN

beTarget a b t = EndpointSt (Target t) (a ... b)

beSource a b ss = EndpointSt (Source $ S.fromList ss) (a ... b)

assertBindFullness = do
    UnitTestState{unit, functs} <- get
    if isFullyBinded unit functs
        then return $ Right unit
        else error "Function is not binded to process!"

isFullyBinded pu fs =
    let fu = S.fromList $ functions pu
     in not (null fu) && fu == S.fromList fs

assertProcessDone =
    do
        UnitTestState{unit, functs} <- get
        if isProcessComplete' unit functs
            && null (endpointOptions unit)
            then return $ Right unit
            else error "Process is not complete"

-- TODO: clean/combine with utils
isProcessComplete' pu fs = unionsMap variables fs == processedVars' pu
processedVars' pu = unionsMap variables $ getEndpoints $ process pu

------------------REMOVE AFTER TESTS------------------------
example = evalMultiplier st0 $ do
    _ <- bindFunc fDef
    _ <- assertBindFullness
    doDecision $ beTarget 1 2 "a"
    doNDecision 2
    doDecision $ beSource 5 5 ["c"]
    doFstDecision
    assertProcessDone

fDef = F.multiply "a" "b" ["c", "d"] :: F String Int

st0 = multiplier True :: Multiplier String Int Int