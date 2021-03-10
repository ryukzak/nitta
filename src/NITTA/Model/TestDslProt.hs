{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Data.Default
import qualified Data.Set as S

-- TODO remove debug
import qualified Debug.Trace as DebugTrace
import qualified NITTA.Intermediate.Functions as F
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits
import NITTA.Utils
import Numeric.Interval.NonEmpty

data UnitTestState v x t = UnitTestState
    { unit :: Multiplier v x t
    , functs :: [F v x]
    -- , decisions :: EndpointSt
    }
    deriving (Show)

-- TODO: make func use previous state [F v x]
bindFunc f st =
    case tryBind f st of
        Right v -> put $ UnitTestState v [f] -- : functs
        --- TODO add assertFalse when wrong bind
        Left err -> error $ "can't get report: " ++ err

-- TODO: fix not only first decision?
--       Check for null
-- TODO fix bug when sources are taken at one decision
doFstDecision = do
    UnitTestState{unit, functs} <- get
    doDecision $ DebugTrace.traceShow (fstDecision unit) (fstDecision unit)

fstDecision st = endpointOptionToDecision . head . endpointOptions $ st

-- TODO: add version without time via Maybe
beTarget a b t = EndpointSt (Target t) (a ... b)

-- TODO: add version without time
beSource a b ss = EndpointSt (Source $ S.fromList ss) (a ... b)

-- what if [] is empty??
-- Todo make endpSt Maybe, it will allow to get it from State
doDecision endpSt = do
    UnitTestState{unit, functs} <- get
    put $ UnitTestState (endpointDecision unit endpSt) functs

-- TODO: rewrite with State
isBinded f pu =
    let fu = functions pu
     in not (null fu) && (head fu == f)

isBindedSt f =
    let fu = functions unit
        UnitTestState{unit, functs} = get
     in not (null fu) && fu == f

-- TODO: do I need this func for single functions when we have 2 binded
isProcessComplete = do
    UnitTestState{unit, functs} <- get
    if isProcessComplete' unit functs
        then return ()
        else error "Process is not complete!"

checkPipe f st = flip execState defUTS $ do
    bindFunc f st
    --    if isBindedSt [f] then doFstDecision else error "[Char]"
    doDecision $ beTarget 1 2 "a"
    doFstDecision
    doDecision $ beSource 5 5 ["c", "d"]
    isProcessComplete

------------------------------------------------------------------
defUTS = UnitTestState st0 []

fDef = F.multiply "a" "b" ["c", "d"] :: F String Int

st0 = multiplier True :: Multiplier String Int Int
Right st1def = tryBind fDef st0

-- TODO: clean/combine with utils
isProcessComplete' pu fs = unionsMap variables fs == processedVars pu

processedVars pu = unionsMap variables $ getEndpoints $ process pu