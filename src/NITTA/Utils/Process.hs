{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}

{-|
Module      : NITTA.Utils.Process
Description : Utilities for process description.
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Utils.Process
    ( Schedule(..) -- TODO: must be hidded
    , runSchedule
    , execSchedule
    , scheduleEndpoint
    , scheduleFunction
    , scheduleInstruction
    , establishVerticalRelations
    , updateTick -- TODO: must be hidded
    ) where

import           Control.Monad.State
import           Data.Proxy          (asProxyTypeOf)
import           Data.Typeable
import           NITTA.Types
import           Numeric.Interval    (inf, sup, (...))


data Schedule pu v x t
    = Schedule
        { schProcess :: Process (Parcel v x) t
        , iProxy     :: Proxy (Instruction pu)
        }


execSchedule pu st = snd $ runSchedule pu st

runSchedule pu st
    = let (a, s) = runState st Schedule
            { schProcess=process pu
            , iProxy=ip pu
            }
    in (a, schProcess s)
    where
        ip :: pu -> Proxy (Instruction pu)
        ip _ = Proxy


scheduleStep placeInTime stepInfo = do
    sch@Schedule{ schProcess=p@Process{ nextUid, steps } } <- get
    put sch
        { schProcess=p
            { nextUid=succ nextUid
            , steps=Step nextUid placeInTime stepInfo : steps
            }
        }
    return [ nextUid ]

establishVerticalRelations high low = do
    sch@Schedule{ schProcess=p@Process{ relations } } <- get
    put sch
        { schProcess=p
            { relations=[ Vertical h l | h <- high, l <- low ]++ relations
            }
        }



scheduleFunction a b f = scheduleStep (Activity $ a ... b) $ FStep f

scheduleEndpoint EndpointD{ epdAt, epdRole } codeGen = do
    high <- scheduleStep (Activity $ inf epdAt ... sup epdAt) $ EndpointRoleStep epdRole
    low <- codeGen
    establishVerticalRelations high low
    return high

scheduleInstruction start finish instr = do
    Schedule{ iProxy } <- get
    scheduleStep (Activity $ start ... finish) $ InstructionStep (instr `asProxyTypeOf` iProxy)


-- depricated
updateTick tick = do
    sch@Schedule{ schProcess } <- get
    put sch
        { schProcess=schProcess
            { nextTick=tick
            }
        }
