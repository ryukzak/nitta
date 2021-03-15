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
import qualified Data.Set as S

-- TODO remove debug

import qualified Data.String.Utils as S
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
    }
    deriving (Show)

execMultiplier st decs = flip execState (UnitTestState st []) $ do
    _ <- decs
    unit <$ get

bindFunc f = do
    UnitTestState{unit, functs} <- get
    case tryBind f unit of
        Right v -> put $ UnitTestState v $ f : functs
        Left err -> error $ "tryBind func returned error: " ++ err

-- TODO: fix not only first decision?
-- doNDecision = do

-- TODO fix bug when sources are taken at one decision
doFstDecision = do
    UnitTestState{unit, functs} <- get
    doDecision $ fstDecision unit

fstDecision = endpointOptionToDecision . head . endpointOptions

-- TODO: add version without time via Maybe
beTarget a b t = EndpointSt (Target t) (a ... b)

-- TODO: add version without time
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

-- TODO: do I need this func for single functions when we have 2 binded
isProcessComplete = do
    UnitTestState{unit, functs} <- get
    if isProcessComplete' unit functs
        then return ()
        else -- TODO: more info in errro?
            error "Process is not complete!"

-----------------INTEGRITY ----------------------
{-
    if isProcessComplete' pu f
        then checkIntegrity pu f
        else error "Process is not complete!"
-}

--checkIntegrity pu [] = integrity -- output
checkIntegrity f pu = checkIntegrity' f pu

--checkIntegrity' f steps [] = integrity
checkIntegrity f pu =
    let pr = process pu
        funcPID = checkFunction f $ steps pr
     in checkEndpoints f pr funcPID

-- | Find requested function f in steps of a given process
checkFunction f [] = error $ "Requested function: " <> show f <> ", not found."
checkFunction f (stp : stps) =
    let compFun (FStep F{fun}) = show f == S.replace "\"" "" (DebugTrace.traceShow ("CHECK FUNC|" <> show fun) (show fun))
        compFun _ = False
     in if compFun $ pDesc stp
            then pID stp
            else checkFunction f stps

-- | For a given pID finds and returns pIDs of Steps which has all Endpoints
checkEndpoints f pr pid =
    let vars = variables f
        (foundRel, foundPid) = checkRelationsEp pid (steps pr) vars $ relations pr
     in if vars == foundRel
            then foundPid
            else error $ "Not all variables has related Endpoints, expected: [" <> show vars <> "]; found: [" <> show foundRel <> "]."

                    -- TODO use [i|] ???

checkTransports f pr pids =
    let concRes (epVars, epPid) (epVars2, epPid2) = (S.union epVars epVars2, epPid ++ epPid2)
     in foldr concRes (S.empty, []) [checkRelationsEp pid (steps pr) (variables f) $ relations pr | pid <- pids]

checkRelationsEp pid stps vars rels =
    let concEp (epVars, epPid) (epVars2, epPid2) = (S.union epVars epVars2, epPid : epPid2)
     in foldr concEp (S.empty, []) $
            [ checkStepsEp vars stps v2
            | (Vertical v1 v2) <- rels
            , pid == v1
            ]

-- | Finds given pid in pDesc of Steps, if it's Source or Target then returns it.
checkStepsEp vars stps pid =
    let combSteps = [S.intersection vars stp | stp <- map stepInfo stps, not $ null stp]
        stepInfo Step{pID, pDesc}
            | pID == pid = pDescInfo pDesc
            | otherwise = S.empty
        pDescInfo descr = case descr of
            (EndpointRoleStep (Source s)) -> s
            (EndpointRoleStep (Target t)) -> S.fromList [t]
            _ -> S.empty
     in if not $ null combSteps
            then (head combSteps, pid)
            else error $ "Endpoint with pid=" <> show pid <> " not found in Steps [" <> show stps <> "]!"

------------------REMOVE AFTER TESTS------------------------
checkPipe f f2 st = execMultiplier st $ do
    bindFunc f
    bindFunc f2
    isBinded
    doDecision $ beTarget 1 2 "q"
    doFstDecision
    doFstDecision
    --doDecision $ beSource 5 5 ["c", "d"]
    ----
    doFstDecision
    doFstDecision
    doFstDecision
    isProcessComplete

getSmth f st = checkIntegrity f (unit $ checkPipe2 f st)

stability2 = getSmth fDef st0
test2 f = checkIntegrity f $ unit $ checkPipe fDef f2Def st0

checkPipe2 f st = execMultiplier st $ do
    bindFunc f
    isBinded
    doDecision $ beTarget 1 2 "a"
    doFstDecision
    doDecision $ beSource 5 5 ["c", "d"]
    isProcessComplete
checkPipe3 f st = execMultiplier st $ do
    bindFunc f
    isBinded
    doDecision $ beTarget 1 2 "a"
    doFstDecision
    doDecision $ beSource 5 5 ["d"]
    doDecision $ beSource 6 6 ["c"]
    isProcessComplete

-- TODO make example to hold signatures
stability f f2 = checkPipe f f2 st0

------------------------------------------------------------------
fDef = F.multiply "a" "b" ["c", "d"] :: F String Int
f2Def = F.multiply "c" "q" ["m"] :: F String Int

st0 = multiplier True :: Multiplier String Int Int
Right st1def = tryBind fDef st0

-- TODO: clean/combine with utils
isProcessComplete' pu fs = unionsMap variables fs == processedVars pu

processedVars pu = unionsMap variables $ getEndpoints $ process pu