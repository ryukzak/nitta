{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures -fno-warn-type-defaults #-}

{-|
Module      : NITTA.Utils.Process
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
module NITTA.Utils.Process
    ( runSchedule
    , execSchedule, execScheduleWithProcess
    , scheduleEndpoint
    , scheduleFunctoinBind, scheduleFunction
    , scheduleInstruction
    , scheduleNestedStep
    , establishVerticalRelations, establishVerticalRelation
    , getProcessSlice
    , updateTick -- TODO: must be hidded
    ) where

import           Control.Monad.State
import           Data.Proxy                       (asProxyTypeOf)
import           Data.Typeable
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.ProcessorUnits.Types
import           Numeric.Interval                 (inf, sup, (...))

-- |Process builder state.
data Schedule pu v x t
    = Schedule
        { -- |Defining process.
          schProcess :: Process v x t
          -- |Proxy for process unit instruction, which is needed for API simplify. Without that,
          -- for some function, the user needs to describe type explicitly.
        , iProxy     :: Proxy (Instruction pu)
        }

-- |Execute process builder and return new process description. The initial process state is getting
-- from the PU by the 'process' function.
execSchedule pu st = snd $ runSchedule pu st

-- |Execute process builder and return new process description. The initial process state is passed
-- explicetly.
execScheduleWithProcess pu p st = snd $ runScheduleWithProcess pu p st

-- |Execute process builder and return list of new step UID and new process description. The initial
-- process state is getting from the PU by the 'process' function.
runSchedule pu st = runScheduleWithProcess pu (process pu) st

-- |Execute process builder and return list of new step UID and new process description. The initial
-- process state is passed explicetly.
runScheduleWithProcess pu p st
    = let (a, s) = runState st Schedule
            { schProcess=p
            , iProxy=ip pu
            }
    in (a, schProcess s)
    where
        ip :: pu -> Proxy (Instruction pu)
        ip _ = Proxy

-- |Add process step with passed the time and info.
scheduleStep placeInTime stepInfo
    = scheduleStep' (\uid -> Step uid placeInTime stepInfo)

scheduleStep' mkStep = do
    sch@Schedule{ schProcess=p@Process{ nextUid, steps } } <- get
    put sch
        { schProcess=p
            { nextUid=succ nextUid
            , steps=mkStep nextUid : steps
            }
        }
    return [ nextUid ]

-- |Add to the process description information about vertical relations, which are defined by the
-- Cartesian product of high and low lists.
establishVerticalRelations high low = do
    sch@Schedule{ schProcess=p@Process{ relations } } <- get
    put sch
        { schProcess=p
            { relations=[ Vertical h l | h <- high, l <- low ] ++ relations
            }
        }

-- |Add to the process description information about vertical relation.
establishVerticalRelation h l = do
    sch@Schedule{ schProcess=p@Process{ relations } } <- get
    put sch
        { schProcess=p
            { relations=Vertical h l : relations
            }
        }

scheduleFunctoinBind f = do
    Schedule{ schProcess=Process{ nextTick } } <- get
    scheduleStep (Event nextTick) $ CADStep $ "bind " ++ show f

-- |Add to the process description information about function evaluation.
scheduleFunction a b f = scheduleStep (Activity $ a ... b) $ FStep f

-- |Add to the process description information about endpoint behaviour, and it's low-level
-- implementation (on instruction level). Vertical relations connect endpoint level and instruction
-- level steps.
scheduleEndpoint EndpointD{ epdAt, epdRole } codeGen = do
    high <- scheduleStep (Activity $ inf epdAt ... sup epdAt) $ EndpointRoleStep epdRole
    low <- codeGen
    establishVerticalRelations high low
    return high

-- |Add to the process description information about instruction evaluation.
scheduleInstruction start finish instr = do
    Schedule{ iProxy } <- get
    scheduleStep (Activity $ start ... finish) $ InstructionStep (instr `asProxyTypeOf` iProxy)

-- |Add to the process description information about nested step.
scheduleNestedStep tag step@Step{ sTime } = do
    sKey <- scheduleStep' (\uid -> Step uid sTime $ NestedStep tag step)
    when (length sKey /= 1) $ error "scheduleNestedStep internal error."
    return $ head sKey

-- |Get a current slice of the computational process.
getProcessSlice :: State (Schedule pu v x t) (Process v x t)
getProcessSlice = do
    Schedule{ schProcess } <- get
    return schProcess

-- FIXME: deprecated, but why&
updateTick tick = do
    sch@Schedule{ schProcess } <- get
    put sch
        { schProcess=schProcess
            { nextTick=tick
            }
        }
