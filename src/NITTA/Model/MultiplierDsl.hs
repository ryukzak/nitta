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
module NITTA.Model.MultiplierDsl (
    ) where

import Control.Monad.State.Lazy
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

execMultiplier st decs = flip execState (UnitTestState st []) $ do
    _ <- decs
    unit <$ get

bindFunc f = do
    UnitTestState{unit, functs} <- get
    case tryBind f unit of
        Right v -> put $ UnitTestState v $ f : functs
        Left err ->
            error
                [i| tryBind func returned error: #{ err } 
                                   for function: #{ f }
                                       for unit: #{ unit }
                |]

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

beTarget t a b = EndpointSt (Target t) (a ... b)

beSource a b ss = EndpointSt (Source $ S.fromList ss) (a ... b)

doDecision endpSt = do
    UnitTestState{unit, functs} <- get
    put $ UnitTestState (endpointDecision unit endpSt) functs

isFunctionsBinded pu fs =
    let fu = S.fromList $ functions pu
     in not (null fu) && fu == S.fromList fs

isBinded = do
    UnitTestState{unit, functs} <- get
    if isFunctionsBinded unit functs
        then return ()
        else error "Function is not binded to process!"

isProcessComplete = do
    UnitTestState{unit, functs} <- get
    if isProcessComplete' unit functs
        && null (endpointOptions unit)
        then return ()
        else
            error
                [__i| Process is not complete!
                        decisions available: #{ endpointOptions unit }
                                     functs: #{ functs }
                                         pu: #{ unit }
                  |]

-- TODO: clean/combine with utils
isProcessComplete' pu fs = unionsMap variables fs == processedVars pu

processedVars pu = unionsMap variables $ getEndpoints $ process pu

------------------REMOVE AFTER TESTS------------------------
example f f2 = example' f f2 st0
example' f1 f2 st = execMultiplier st $ do
    bindFunc f1
    bindFunc f2
    isBinded
    doDecision $ beTarget "a" 1 2
    doNDecision 2
    --doFstDecision
    doFstDecision
    ----
    doFstDecision
    doFstDecision
    doFstDecision
    isProcessComplete

fDef = F.multiply "a" "b" ["c", "d"] :: F String Int
f2Def = F.multiply "c" "q" ["m"] :: F String Int

st0 = multiplier True :: Multiplier String Int Int