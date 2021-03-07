{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}

{- |
Module      : NITTA.Model.Microarchitecture
Description : Create micro architecture functions
Copyright   : (c) Daniil Prohorov, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.TestDslProt (
    ) where

import Control.Monad.State.Lazy
import qualified NITTA.Intermediate.Functions as F
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Utils
import Numeric.Interval.NonEmpty

-- bind shoud put State
-- bindFunc :: ProcessorUnit u v x t => F v x -> u ->
bindFunc f st =
    case tryBind f st of
        Right v -> put [v]
        --- TODO add assertFalse when wrong bind
        Left err -> error $ "can't get report: " ++ err

-- TODO: fix not only first decision?
--       Check for null
getDecision st = endpointOptionToDecision . head . endpointOptions $ st

-- what if [] is empty??
doDecision endpSt = do
    sts <- get
    put $ endpointDecision (head sts) endpSt : sts

-- doDecision endpSt = modify (endpointDecision (head <$> get) endpSt :)

getAndDoDecision st = doDecision . getDecision . head $ st

getAndDoDecision2 = runState $ doDecision . getDecision . head <$> get

-- TODO: rewrite, because `functions a` has `F [Char] Int` type
isBinded f pu =
    let fu = functions pu
     in not (null fu) && (head fu == f)

evalMultiplierChain st bindF [] = error ""
evalMultiplierChain st bindF fs = head $
    flip execState [] $ do
        modify (bindFunc bindF st :)
        --isBinded f st
        evalMultiplierChain' fs

evalMultiplierChain' [] = get
evalMultiplierChain' fs = do
    modify (head fs get :)
    evalMultiplierChain' $ tail fs

--- To check whether piping work
--testMultChain = evalMultiplierChain st0 fDef []

checkPipe f st = flip runState [] $ do
    bindFunc f st
    --isBinded st

    -- TODO assert between steps
    let ep = EndpointSt (Target "a") (1 ... 2)
    doDecision ep

    --    isProcessComplete (head sts) $ variables f

    sts <- get
    getAndDoDecision sts

    sts <- get
    getAndDoDecision sts

    sts <- get
    return $ isProcessComplete (head sts) [f]

fDef = F.multiply "a" "b" ["c", "d"] :: F String Int

st0 = multiplier True :: Multiplier String Int Int
Right st1def = tryBind fDef st0

-- TODO: clean/combine with utils
isProcessComplete pu fs = unionsMap variables fs == processedVars pu

processedVars pu = unionsMap variables $ getEndpoints $ process pu