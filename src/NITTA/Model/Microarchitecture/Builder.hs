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
import qualified Data.Set as S
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
    , availPorts :: [SignalTag]
    , puProtos :: [(tag, PU v x t)]
    }

netIOPorts puProtos =
    let setOfPorts ports = foldl (flip $ S.union . ports . snd) S.empty puProtos
     in BusNetworkIO
            { extInputs = setOfPorts puInputPorts
            , extOutputs = setOfPorts puOutputPorts
            , extInOuts = setOfPorts puInOutPorts
            }

puEnv tag ports ioports =
    def
        { ctrlPorts = Just ports
        , ioPorts = Just ioports
        , valueIn = Just ("data_bus", "attr_bus")
        , valueOut = Just (toText tag <> "_data_out", toText tag <> "_attr_out")
        }

-- |Define microarchitecture with BusNetwork
defineNetwork bnName ioSync builder =
    let st0 =
            BuilderSt
                { signalBusWidth = 0
                , availPorts = map (SignalTag . controlSignalLiteral) [0 :: Int ..]
                , puProtos = []
                }
        BuilderSt{signalBusWidth, puProtos, availPorts} = flip execState st0 $ void builder
     in BusNetwork
            { bnName
            , bnRemains = []
            , bnBinded = M.empty
            , bnProcess = def
            , bnPus = M.fromList puProtos
            , bnSignalBusWidth = signalBusWidth
            , ioSync
            , bnEnv = def{ioPorts = Just $ netIOPorts puProtos}
            , bnAvailPorts = availPorts
            }

modifyNetwork net@BusNetwork{bnPus, bnSignalBusWidth, bnAvailPorts, bnEnv} builder =
    let st0 =
            BuilderSt
                { signalBusWidth = bnSignalBusWidth
                , availPorts = bnAvailPorts
                , puProtos = M.toList bnPus
                }
        BuilderSt{signalBusWidth, puProtos, availPorts} = flip execState st0 $ void builder
     in net
            { bnPus = M.fromList puProtos
            , bnSignalBusWidth = signalBusWidth
            , bnAvailPorts = availPorts
            , bnEnv = bnEnv{ioPorts = Just $ netIOPorts puProtos}
            }

-- |Add PU with the default initial state. Type specify by IOPorts.
add tag ioport = addCustom tag def ioport

-- |Add PU with the custom initial state. Type specify by IOPorts.
addCustom tag pu ioports = do
    st@BuilderSt{signalBusWidth, availPorts, puProtos} <- get
    let ports = takePortTags availPorts pu
        pu' = PU pu def $ puEnv tag ports ioports
        usedPortsLen = length $ usedPortTags ports
    put
        st
            { signalBusWidth = signalBusWidth + usedPortsLen
            , availPorts = drop usedPortsLen availPorts
            , puProtos = (tag, pu') : puProtos
            }

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
