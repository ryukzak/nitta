{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
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
    MicroarchitectureDesc (..),
    NetworkDesc (..),
    UnitDesc (..),
    microarchitectureDesc,
) where

import Control.Monad.State.Lazy
import Data.Aeson
import Data.Default
import qualified Data.List as L
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
    , availPorts :: [SignalTag]
    , puProtos :: [(tag, PU v x t)]
    , netEnv :: UnitEnv (BusNetwork tag v x t)
    }

-- |Define microarchitecture with BusNetwork
defineNetwork bnName ioSync builder =
    let defEnv = def
        st0 =
            BuilderSt
                { signalBusWidth = 0
                , availPorts = map (SignalTag . controlSignalLiteral) [0 :: Int ..]
                , puProtos = []
                , netEnv = defEnv
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
            , bnEnv = defEnv{ioPorts = Just netIOPorts}
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
                    , valueOut = Just (toText tag <> "_data_out", toText tag <> "_attr_out")
                    }
        usedPorts = usedPortTags ports
    put
        st
            { signalBusWidth = signalBusWidth + length usedPorts
            , availPorts = drop (length usedPorts) availPorts
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
