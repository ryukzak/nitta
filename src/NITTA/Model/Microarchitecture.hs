{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : NITTA.Model.Microarchitecture
Description : Create micro architecture functions
Copyright   : (c) Daniil Prohorov, Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Microarchitecture (
    defineNetwork,
    add,
    addCustom,
) where

import Control.Monad.State.Lazy
import Data.Default
import qualified Data.List as L
import qualified Data.Map.Strict as M
import NITTA.Model.Networks.Bus
import NITTA.Model.Networks.Types
import NITTA.Model.ProcessorUnits
import NITTA.Project

data BuilderSt tag v x t = BuilderSt
    { signalBusWidth :: Int
    , availPorts :: [SignalTag]
    , puProtos :: [(tag, PU v x t)]
    , netEnv :: UnitEnv (BusNetwork tag v x t)
    }

-- |Define microarchitecture with BusNetwork
defineNetwork bnName ioSync builder =
    let netEnv0 =
            UnitEnv
                { sigClk = "clk"
                , sigRst = "rst"
                , sigCycleBegin = "flag_cycle_begin"
                , sigInCycle = "flag_in_cycle"
                , sigCycleEnd = "flag_cycle_end"
                , ctrlPorts = Nothing
                , ioPorts = Nothing
                , valueIn = Nothing
                , valueOut = Nothing
                }
        st0 =
            BuilderSt
                { signalBusWidth = 0
                , availPorts = map (SignalTag . controlSignalLiteral) [0 :: Int ..]
                , puProtos = []
                , netEnv = netEnv0
                }
        BuilderSt{signalBusWidth, puProtos} = flip execState st0 $ void builder
        netIOPorts =
            BusNetworkIO
                { extInputs = L.nub $ concatMap (puInputPorts . snd) puProtos
                , extOutputs = L.nub $ concatMap (puOutputPorts . snd) puProtos
                , extInOuts = L.nub $ concatMap (puInOutPorts . snd) puProtos
                }
     in BusNetwork
            { bnName
            , bnRemains = []
            , bnBinded = M.empty
            , bnProcess = def
            , bnPus = M.fromList puProtos
            , bnSignalBusWidth = signalBusWidth
            , ioSync
            , bnEnv = netEnv0{ioPorts = Just netIOPorts}
            }

-- |Add PU with the default initial state. Type specify by IOPorts.
add tag ioport = addCustom tag def ioport

-- |Add PU with the custom initial state. Type specify by IOPorts.
addCustom tag pu ioports = do
    st@BuilderSt{signalBusWidth, availPorts, puProtos, netEnv} <- get
    let ports = takePortTags availPorts pu
        pu' =
            PU
                pu
                def
                netEnv
                    { ctrlPorts = Just ports
                    , ioPorts = Just ioports
                    , valueIn = Just ("data_bus", "attr_bus")
                    , valueOut = Just (tag <> "_data_out", tag <> "_attr_out")
                    }
        usedPorts = usedPortTags ports
    put
        st
            { signalBusWidth = signalBusWidth + length usedPorts
            , availPorts = drop (length usedPorts) availPorts
            , puProtos = (tag, pu') : puProtos
            }
