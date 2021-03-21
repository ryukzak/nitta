{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}

{- |
Module      : NITTA.Model.MultiplierDsl
Description : Provides functions to make decisions in Multiplier
Copyright   : (c) co0ll3r, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.MultiplierDsl (
    evalMultiplier,
    bindFunc,
    doDecision,
    doFstDecision,
    doNDecision,
    beTarget,
    beSource,
    isProcessDone,
    fDef
) where

import Control.Monad.State.Lazy
import Data.Either
import qualified Data.Set as S
import Data.String.Interpolate
import qualified NITTA.Intermediate.Functions as F
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Utils
import Numeric.Interval.NonEmpty

data UnitTestState v x t = UnitTestState
    { unit :: Multiplier v x t
    , functs :: [F v x]
    }
    deriving (Show)

evalMultiplier st alg = evalState alg (UnitTestState st [])

bindFunc f = do
    UnitTestState{unit, functs} <- get
    case tryBind f unit of
        Right v -> do
            put $ UnitTestState v $ f : functs
            return $ Right v
        Left err -> return $ Left err

doDecision endpSt = do
    UnitTestState{unit, functs} <- get
    put $ UnitTestState (endpointDecision unit endpSt) functs

doFstDecision = do
    UnitTestState{unit, functs} <- get
    doDecision $ fstDecision unit

doNDecision n = do
    UnitTestState{unit, functs} <- get
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

isBinded = do
    UnitTestState{unit, functs} <- get
    if isFuncsBinded unit functs
        then return $ Right unit
        else return $ Left $ error "Function is not binded to process!"

isFuncsBinded pu fs =
    let fu = S.fromList $ functions pu
     in not (null fu) && fu == S.fromList fs

isProcessDone =
    do
        UnitTestState{unit, functs} <- get
        if isProcessComplete' unit functs
            && null (endpointOptions unit)
            then return $ Right unit
            else return $ Left $ error "Process is not complete"

-- TODO: clean/combine with utils
isProcessComplete' pu fs = unionsMap variables fs == processedVars' pu
processedVars' pu = unionsMap variables $ getEndpoints $ process pu

------------------REMOVE AFTER TESTS------------------------
example = evalMultiplier st0 $ do
    _ <- bindFunc fDef
    _ <- isBinded
    doDecision $ beTarget 1 2 "a"
    doNDecision 2
    doDecision $ beSource 5 5 ["c"]
    doFstDecision
    isProcessDone

fDef = F.multiply "a" "b" ["c", "d"] :: F String Int

st0 = multiplier True :: Multiplier String Int Int