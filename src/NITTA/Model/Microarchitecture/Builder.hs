{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Module      : NITTA.Model.Microarchitecture.Builder
Description : Create micro architecture functions
Copyright   : (c) Daniil Prohorov, Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Microarchitecture.Builder (
    defineNetwork,
    modifyNetwork,
    add,
    addCustom,
    MicroarchitectureDesc (..),
    NetworkDesc (..),
    UnitDesc (..),
    microarchitectureDesc,
) where

import Control.Monad.State.Lazy
import Data.Aeson
import Data.Default
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics
import NITTA.Model.Networks.Bus
import NITTA.Model.Networks.Types
import NITTA.Model.ProcessorUnits
import NITTA.Project
import NITTA.Utils.Base

data BuilderSt tag v x t = BuilderSt
    { signalBusWidth :: Int
    , availSignals :: [SignalTag]
    , pus :: [(tag, PU v x t)]
    }

netIOPorts pus =
    BusNetworkIO
        { extInputs = setOf puInputPorts
        , extOutputs = setOf puOutputPorts
        , extInOuts = setOf puInOutPorts
        }
    where
        setOf ioPorts = unionsMap ioPorts pus

modifyNetwork net@BusNetwork{bnPus, bnSignalBusWidth, bnEnv} builder =
    let st0 =
            BuilderSt
                { signalBusWidth = bnSignalBusWidth
                , availSignals = map (SignalTag . controlSignalLiteral) [bnSignalBusWidth :: Int ..]
                , pus = M.toList bnPus
                }
        BuilderSt{signalBusWidth, pus} = execState builder st0
     in net
            { bnPus = M.fromList pus
            , bnSignalBusWidth = signalBusWidth
            , bnEnv = bnEnv{ioPorts = Just $ netIOPorts $ map snd pus}
            }

defineNetwork bnName ioSync builder =
    let net =
            BusNetwork
                { bnName
                , bnRemains = []
                , bnBinded = M.empty
                , bnProcess = def
                , bnPus = M.empty
                , bnSignalBusWidth = 0
                , ioSync
                , bnEnv = def
                }
     in modifyNetwork net builder

puEnv tag ctrlPorts ioPorts =
    def
        { ctrlPorts = Just ctrlPorts
        , ioPorts = Just ioPorts
        , valueIn = Just ("data_bus", "attr_bus")
        , valueOut = Just (toText tag <> "_data_out", toText tag <> "_attr_out")
        }

-- |Add PU with the custom initial state. Type specify by IOPorts.
addCustom tag pu ioPorts = do
    st@BuilderSt{signalBusWidth, availSignals, pus} <- get
    let ctrlPorts = takePortTags availSignals pu
        pu' = PU pu def $ puEnv tag ctrlPorts ioPorts
        usedPortsLen = length $ usedPortTags ctrlPorts
    put
        st
            { signalBusWidth = signalBusWidth + usedPortsLen
            , availSignals = drop usedPortsLen availSignals
            , pus = (tag, pu') : pus
            }

-- |Add PU with the default initial state. Type specify by IOPorts.
add tag ioport = addCustom tag def ioport

data MicroarchitectureDesc tag = MicroarchitectureDesc
    { networks :: [NetworkDesc tag]
    , ioSyncMode :: IOSynchronization
    }
    deriving (Generic)

instance (ToJSON tag) => ToJSON (MicroarchitectureDesc tag)

data NetworkDesc tag = NetworkDesc
    { networkTag :: tag
    , valueType :: T.Text
    , units :: [UnitDesc tag]
    }
    deriving (Generic)

instance (ToJSON tag) => ToJSON (NetworkDesc tag)

data UnitDesc tag = UnitDesc
    { unitTag :: tag
    , unitType :: T.Text
    }
    deriving (Generic)

instance (ToJSON tag) => ToJSON (UnitDesc tag)

microarchitectureDesc :: forall tag v x t. (Typeable x) => BusNetwork tag v x t -> MicroarchitectureDesc tag
microarchitectureDesc BusNetwork{bnName, bnPus, ioSync} =
    MicroarchitectureDesc
        { networks =
            [ NetworkDesc
                { networkTag = bnName
                , valueType = showText $ typeRep (Proxy :: Proxy x)
                , units =
                    map
                        ( \(tag, PU{unit}) ->
                            UnitDesc
                                { unitTag = tag
                                , unitType = T.pack $ takeWhile (' ' /=) $ show $ typeOf unit
                                }
                        )
                        $ M.assocs bnPus
                }
            ]
        , ioSyncMode = ioSync
        }
