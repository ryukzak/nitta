{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module NITTA.Utils.Process where

import           Control.Monad.State
import           Data.Proxy           (asProxyTypeOf)
import           Data.Typeable
import           NITTA.Types
import           Numeric.Interval     (inf, sup, (...))



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

nextScheduledTick :: ( Show t ) => State (Schedule pu v x t) t
nextScheduledTick = do
    Schedule{ schProcess=Process{ nextTick } } <- get
    return nextTick

updateTick tick = do
    sch@Schedule{ schProcess } <- get
    put sch
        { schProcess=schProcess
            { nextTick=tick
            }
        }

scheduleStep placeInTime stepInfo = do
    sch@Schedule{ schProcess=p@Process{ nextUid, steps } } <- get
    put sch
        { schProcess=p
            { nextUid=succ nextUid
            , steps=Step nextUid placeInTime stepInfo : steps
            }
        }

scheduleInstructionAndUpdateTick start finish instr = do
    Schedule{ iProxy } <- get
    scheduleStep (Activity $ start ... finish) $ InstructionStep (instr `asProxyTypeOf` iProxy)
    updateTick $ finish + 1

scheduleNopAndUpdateTick start finish =
    when (start <= finish) $ do
        Schedule{ iProxy } <- get
        scheduleStep (Activity $ start ... finish) $ InstructionStep (nop `asProxyTypeOf` iProxy)
        updateTick $ finish + 1

scheduleEndpoint EndpointD{ epdAt, epdRole } codeGen = do
    scheduleStep (Activity $ inf epdAt ... sup epdAt) $ EndpointRoleStep epdRole
    codeGen
