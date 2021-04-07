{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

{- |
Module      : NITTA.Utils.ProcessDescription
Description : Utilities for process description.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

A multilevel process is an object with a complex internal structure. Process description should
contain every step (including start and finish time), and relations between them (Vertical or
sequence). It is possible to define process manually, but, in practice, is preferred to use 'State'
based builder from that module.

It also agreed to the process inspection.
-}
module NITTA.Utils.ProcessDescription (
    runSchedule,
    execSchedule,
    execScheduleWithProcess,
    scheduleEndpoint,
    scheduleFunctionBind,
    scheduleFunctionRevoke,
    scheduleFunction,
    scheduleFunctionFinish,
    scheduleInstruction,
    scheduleNestedStep,
    establishVerticalRelations,
    establishVerticalRelation,
    establishHorizontalRelations,
    getProcessSlice,
    relatedEndpoints,
    castInstruction,
    updateTick, -- TODO: must be hidded
) where

import Control.Monad.State
import Data.Proxy (asProxyTypeOf)
import qualified Data.Set as S
import Data.Typeable
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import Numeric.Interval.NonEmpty (singleton)

-- |Process builder state.
data Schedule pu v x t = Schedule
    { -- |Defining process.
      schProcess :: Process t (StepInfo v x t)
    , -- |Proxy for process unit instruction, which is needed for API simplify. Without that,
      -- for some function, the user needs to describe type explicitly.
      iProxy :: Proxy (Instruction pu)
    }

{- |Execute process builder and return new process description. The initial process state is getting
from the PU by the 'process' function.
-}
execSchedule pu st = snd $ runSchedule pu st

{- |Execute process builder and return new process description. The initial process state is passed
explicetly.
-}
execScheduleWithProcess pu p st = snd $ runScheduleWithProcess pu p st

{- |Execute process builder and return list of new step UID and new process description. The initial
process state is getting from the PU by the 'process' function.
-}
runSchedule pu st = runScheduleWithProcess pu (process pu) st

{- |Execute process builder and return list of new step UID and new process description. The initial
process state is passed explicetly.
-}
runScheduleWithProcess pu p st =
    let (a, s) =
            runState
                st
                Schedule
                    { schProcess = p
                    , iProxy = ip pu
                    }
     in (a, schProcess s)
    where
        ip :: pu -> Proxy (Instruction pu)
        ip _ = Proxy

-- |Add process step with passed the time and info.
scheduleStep placeInTime stepInfo =
    scheduleStep' (\uid -> Step uid placeInTime stepInfo)

scheduleStep' mkStep = do
    sch@Schedule{schProcess = p@Process{nextUid, steps}} <- get
    put
        sch
            { schProcess =
                p
                    { nextUid = succ nextUid
                    , steps = mkStep nextUid : steps
                    }
            }
    return [nextUid]

{- |Add to the process description information about vertical relations, which are defined by the
Cartesian product of high and low lists.
-}
establishVerticalRelations high low = do
    sch@Schedule{schProcess = p@Process{relations}} <- get
    put
        sch
            { schProcess =
                p
                    { relations = [Vertical h l | h <- high, l <- low] ++ relations
                    }
            }

-- |Add to the process description information about vertical relation.
establishVerticalRelation h l = do
    sch@Schedule{schProcess = p@Process{relations}} <- get
    put
        sch
            { schProcess =
                p
                    { relations = Vertical h l : relations
                    }
            }

{- |Add to the process description information about horizontal relations (inside
level), which are defined by the Cartesian product of high and low lists.
-}
establishHorizontalRelations high low = do
    sch@Schedule{schProcess = p@Process{relations}} <- get
    put
        sch
            { schProcess =
                p
                    { relations = [Horizontal h l | h <- high, l <- low] ++ relations
                    }
            }

scheduleFunctionBind f = do
    Schedule{schProcess = Process{nextTick}} <- get
    scheduleStep (singleton nextTick) $ CADStep $ "bind " ++ show f

scheduleFunctionRevoke f = do
    Schedule{schProcess = Process{nextTick}} <- get
    scheduleStep (singleton nextTick) $ CADStep $ "revoke " ++ show f

-- |Add to the process description information about function evaluation.
scheduleFunction ti f = scheduleStep ti $ FStep f

{- |Schedule function and establish vertical relations between bind step,
function step, and all related endpoints.
-}
scheduleFunctionFinish bPID function at = do
    fPID <- scheduleFunction at function
    establishVerticalRelations bPID fPID
    process_ <- getProcessSlice
    let low = map pID $ relatedEndpoints process_ $ variables function
    establishVerticalRelations fPID low

{- |Add to the process description information about endpoint behaviour, and it's low-level
implementation (on instruction level). Vertical relations connect endpoint level and instruction
level steps.
-}
scheduleEndpoint EndpointSt{epAt, epRole} codeGen = do
    high <- scheduleStep epAt $ EndpointRoleStep epRole
    low <- codeGen
    establishVerticalRelations high low
    return high

-- |Add to the process description information about instruction evaluation.
scheduleInstruction ti instr = do
    Schedule{iProxy} <- get
    scheduleStep ti $ InstructionStep (instr `asProxyTypeOf` iProxy)

-- |Add to the process description information about nested step.
scheduleNestedStep tag step@Step{pInterval} = do
    pID <- scheduleStep' (\uid -> Step uid pInterval $ NestedStep tag step)
    when (length pID /= 1) $ error "scheduleNestedStep internal error."
    return $ head pID

-- |Get a current slice of the computational process.
getProcessSlice :: State (Schedule pu v x t) (Process t (StepInfo v x t))
getProcessSlice = do
    Schedule{schProcess} <- get
    return schProcess

relatedEndpoints process_ vs =
    filter
        ( \case
            Step{pDesc = EndpointRoleStep role} -> not $ null (variables role `S.intersection` vs)
            _ -> False
        )
        $ steps process_

-- |Helper for instruction extraction from a rigid type variable.
castInstruction :: (Typeable a, Typeable pu) => pu -> a -> Maybe (Instruction pu)
castInstruction _pu i = cast i

-- FIXME: deprecated, should be done as part of scheduling functions.
updateTick tick = do
    sch@Schedule{schProcess} <- get
    put
        sch
            { schProcess =
                schProcess
                    { nextTick = tick
                    }
            }
