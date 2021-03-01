{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Default (def)
import Debug.Trace
import qualified NITTA.Intermediate.Functions as F
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Networks.Types
import NITTA.Model.Problems
import NITTA.Model.Problems.Endpoint
import NITTA.Model.ProcessorUnits
import NITTA.Project
import Numeric.Interval.NonEmpty

-- Store state - st; and endpointOptions -- (endpointOpt, state)
evalProcessUnit = flip runState []

-- bind shoud put State
-- bindFunc :: ProcessorUnit u v x t => F v x -> u ->
bindFunc f st =
    let res = tryBind f st
     in case res of
            Right v -> put [v]
            --- TODO add assertFalse when wrong bind
            Left err -> error $ "can't get report: " ++ err

-- what if [] is empty??
makeDecision endpSt = do
    sts <- get
    put $ endpointDecision (head sts) endpSt : sts

-- TODO: fix not only first decision?
--       Check for null
provideDecision st = endpointOptionToDecision $ head $ endpointOptions st

--- To check whether piping work
checkPipe f st = evalProcessUnit $ do
    bindFunc f st

    -- TODO assert between steps
    let ep = EndpointSt (Target "a") (1 ... 2)
    makeDecision ep

    sts <- get
    makeDecision $ provideDecision $ head sts

    sts <- get
    makeDecision $ provideDecision $ head sts

    return $ head sts

fDef = F.multiply "a" "b" ["c", "d"] :: F String Int

st0 = multiplier True :: Multiplier String Int Int
Right st1def = tryBind fDef st0
