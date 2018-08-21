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
    , schedule
    , scheduleEndpoint
    , scheduleFunction
    , scheduleInstruction
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

schedule pu st
    = schProcess $ execState st Schedule
        { schProcess=process pu
        , iProxy=ip pu
        }
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

scheduleInstruction start finish instr = do
    Schedule{ iProxy } <- get
    scheduleStep (Activity $ start ... finish) $ InstructionStep (instr `asProxyTypeOf` iProxy)

scheduleEndpoint EndpointD{ epdAt, epdRole } codeGen = do
    scheduleStep (Activity $ inf epdAt ... sup epdAt) $ EndpointRoleStep epdRole
    codeGen

scheduleFunction a b f = scheduleStep (Activity $ a ... b) $ FStep f

-- depricated
updateTick tick = do
    sch@Schedule{ schProcess } <- get
    put sch
        { schProcess=schProcess
            { nextTick=tick
            }
        }
