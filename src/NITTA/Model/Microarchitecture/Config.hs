{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NITTA.Model.Microarchitecture.Config (
    MKMicro (..),
) where

import Data.Aeson (
    FromJSON (parseJSON),
    Options (sumEncoding),
    SumEncoding (TaggedObject, contentsFieldName, tagFieldName),
    ToJSON (toJSON),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
 )
import Data.Default (Default (def))
import Data.HashMap.Internal.Strict (HashMap)
import Data.Text qualified as T
import GHC.Generics (Generic)
import NITTA.Intermediate.Value (Attr, FixedPointCompatible, Val)
import NITTA.Intermediate.Variable (Var)
import NITTA.Model.Networks.Bus (
    BusNetwork,
    add,
    addCustom,
    addCustomPrototype,
    addPrototype,
    busNetwork,
    defineNetwork,
    modifyNetwork,
 )
import NITTA.Model.Networks.Types (IOSynchronization)
import NITTA.Model.ProcessorUnits qualified as PU
import NITTA.Utils (getFromToml)

data PUConf
    = Accum
        { name :: T.Text
        }
    | Divider
        { name :: T.Text
        , pipeline :: Int
        , mock :: Bool
        }
    | Multiplier
        { name :: T.Text
        , mock :: Bool
        }
    | Fram
        { name :: T.Text
        , size :: Int
        }
    | SPI
        { name :: T.Text
        , mosi :: T.Text
        , miso :: T.Text
        , sclk :: T.Text
        , cs :: T.Text
        , isSlave :: Bool
        , bufferSize :: Maybe Int
        , bounceFilter :: Int
        }
    | Shift
        { name :: T.Text
        , sRight :: Maybe Bool
        }
    | Comparator
        { name :: T.Text
        }
    | LogicalUnit
        { name :: T.Text
        }
    | Multiplexer
        { name :: T.Text
        }
    deriving (Generic, Show)

puConfJsonOptions =
    defaultOptions
        { sumEncoding = TaggedObject{tagFieldName = "type", contentsFieldName = "type"}
        }

instance ToJSON PUConf where
    toJSON = genericToJSON puConfJsonOptions

instance FromJSON PUConf where
    parseJSON = genericParseJSON puConfJsonOptions

data NetworkConf = NetworkConf
    { name :: T.Text
    , pus :: [PUConf]
    , protos :: [PUConf]
    }
    deriving (Generic, Show)

instance FromJSON NetworkConf
instance ToJSON NetworkConf

newtype MicroarchitectureConf = MicroarchitectureConf
    { networks :: [NetworkConf]
    }
    deriving (Generic, Show)

instance FromJSON MicroarchitectureConf
instance ToJSON MicroarchitectureConf

class MKMicro v x where
    mkMicroarchitecture :: (Var v, Val x, ToJSON a, ToJSON v) => IOSynchronization -> HashMap T.Text a -> BusNetwork T.Text v x Int
    microarchWithProtos :: (PU.VarValTime v x t, PU.UnitTag tag, ToJSON v) => IOSynchronization -> BusNetwork tag v x t
    defMicroarch :: (PU.VarValTime v x t, PU.UnitTag tag, ToJSON v) => IOSynchronization -> BusNetwork tag v x t

instance {-# OVERLAPPABLE #-} (FixedPointCompatible x) => MKMicro v x where
    mkMicroarchitecture ioSync toml =
        let addPU proto
                | proto = addCustomPrototype
                | otherwise = addCustom
            build NetworkConf{pus, protos} = do
                mapM_ (configure False) pus
                mapM_ (configure True) protos
                where
                    configure proto Accum{name} = addPU proto name def PU.AccumIO
                    configure proto Divider{name, pipeline, mock} = addPU proto name (PU.divider pipeline mock) PU.DividerIO
                    configure proto Multiplier{name, mock} = addPU proto name (PU.multiplier mock) PU.MultiplierIO
                    configure proto Fram{name, size} = addPU proto name (PU.framWithSize size) PU.FramIO
                    configure proto Shift{name, sRight} = addPU proto name (PU.shift $ Just False /= sRight) PU.ShiftIO
                    configure proto SPI{name, mosi, miso, sclk, cs, isSlave, bounceFilter, bufferSize} =
                        addPU proto name (PU.anySPI bounceFilter bufferSize) $
                            if isSlave
                                then
                                    PU.SPISlave
                                        { slave_mosi = PU.InputPortTag mosi
                                        , slave_miso = PU.OutputPortTag miso
                                        , slave_sclk = PU.InputPortTag sclk
                                        , slave_cs = PU.InputPortTag cs
                                        }
                                else
                                    PU.SPIMaster
                                        { master_mosi = PU.OutputPortTag mosi
                                        , master_miso = PU.InputPortTag miso
                                        , master_sclk = PU.OutputPortTag sclk
                                        , master_cs = PU.OutputPortTag cs
                                        }
            nets = networks (getFromToml toml :: MicroarchitectureConf)
            mkNetwork net@NetworkConf{name} = modifyNetwork (busNetwork name ioSync) (build net)
         in case nets of
                [n] -> mkNetwork n
                _ -> error "multi-networks are not currently supported"

    microarchWithProtos ioSync = defineNetwork "net1" ioSync $ do
        addCustomPrototype "fram{x}" (PU.framWithSize 32) PU.FramIO
        addPrototype "shift{x}" PU.ShiftIO
        addPrototype "mul{x}" PU.MultiplierIO
        addPrototype "accum{x}" PU.AccumIO
        addPrototype "div{x}" PU.DividerIO
        add "spi" $ -- FIXME: use addPrototype when https://github.com/ryukzak/nitta/issues/194 will be fixed
            PU.SPISlave
                { slave_mosi = PU.InputPortTag "mosi"
                , slave_miso = PU.OutputPortTag "miso"
                , slave_sclk = PU.InputPortTag "sclk"
                , slave_cs = PU.InputPortTag "cs"
                }
        addPrototype "compare{x}" CompareIO
        addPrototype "logicalUnit{x}" LogicalUnitIO
        addPrototype "mux{x}" MultiplexerIO

    defMicroarch ioSync = defineNetwork "net1" ioSync $ do
        addCustom "fram1" (PU.framWithSize 16) PU.FramIO
        addCustom "fram2" (PU.framWithSize 32) PU.FramIO
        add "shift" PU.ShiftIO
        add "mul" PU.MultiplierIO
        add "accum" PU.AccumIO
        add "div" PU.DividerIO
        add "spi" $
            PU.SPISlave
                { slave_mosi = PU.InputPortTag "mosi"
                , slave_miso = PU.OutputPortTag "miso"
                , slave_sclk = PU.InputPortTag "sclk"
                , slave_cs = PU.InputPortTag "cs"
                }
        add "compare" CompareIO
        add "logicalUnit" LogicalUnitIO
        add "mux" MultiplexerIO

mkMicroarchitectureFloat ioSync toml =
    let addPU proto
            | proto = addCustomPrototype
            | otherwise = addCustom
        build NetworkConf{pus, protos} = do
            mapM_ (configure False) pus
            mapM_ (configure True) protos
            where
                configure proto Accum{name} = addPU proto name def PU.AccumIO
                configure proto Divider{name, pipeline, mock} = addPU proto name (PU.divider pipeline mock) PU.DividerIO
                configure proto Multiplier{name, mock} = addPU proto name (PU.multiplier mock) PU.MultiplierIO
                configure proto Fram{name, size} = addPU proto name (PU.framWithSize size) PU.FramIO
                configure proto Shift{name, sRight} = addPU proto name (PU.shift $ Just False /= sRight) PU.ShiftIO
                configure proto Comparator{name} = addPU proto name def PU.CompareIO
                configure proto LogicalUnit{name} = addPU proto name def PU.LogicalUnitIO
                configure proto Multiplexer{name} = addPU proto name def PU.MultiplexerIO
                configure proto SPI{name, mosi, miso, sclk, cs, isSlave, bounceFilter, bufferSize} =
                    addPU proto name (PU.anySPI bounceFilter bufferSize) $
                        if isSlave
                            then
                                PU.SPISlave
                                    { slave_mosi = PU.InputPortTag mosi
                                    , slave_miso = PU.OutputPortTag miso
                                    , slave_sclk = PU.InputPortTag sclk
                                    , slave_cs = PU.InputPortTag cs
                                    }
                            else
                                PU.SPIMaster
                                    { master_mosi = PU.OutputPortTag mosi
                                    , master_miso = PU.InputPortTag miso
                                    , master_sclk = PU.OutputPortTag sclk
                                    , master_cs = PU.OutputPortTag cs
                                    }
        nets = networks (getFromToml toml :: MicroarchitectureConf)
        mkNetwork net@NetworkConf{name} = modifyNetwork (busNetwork name ioSync) (build net)
     in case nets of
            [n] -> mkNetwork n
            _ -> error "multi-networks are not currently supported"

microarchWithProtosFloat ioSync = defineNetwork "net1" ioSync $ do
    addCustomPrototype "fram{x}" (PU.framWithSize 32) PU.FramIO
    addPrototype "shift{x}" PU.ShiftIO
    addPrototype "mul{x}" PU.MultiplierIO
    addPrototype "accum{x}" PU.AccumIO
    addPrototype "div{x}" PU.DividerIO
    add "spi" $ -- FIXME: use addPrototype when https://github.com/ryukzak/nitta/issues/194 will be fixed
        PU.SPISlave
            { slave_mosi = PU.InputPortTag "mosi"
            , slave_miso = PU.OutputPortTag "miso"
            , slave_sclk = PU.InputPortTag "sclk"
            , slave_cs = PU.InputPortTag "cs"
            }
    addPrototype "compare{x}" CompareIO
    addPrototype "logicalUnit{x}" LogicalUnitIO
    addPrototype "mux{x}" MultiplexerIO

defMicroarchFloat ioSync = defineNetwork "net1" ioSync $ do
    addCustom "fram1" (PU.framWithSize 16) PU.FramIO
    addCustom "fram2" (PU.framWithSize 32) PU.FramIO
    add "shift" PU.ShiftIO
    add "mul" PU.MultiplierIO
    add "accum" PU.AccumIO
    add "div" PU.DividerIO
    add "spi" $
        PU.SPISlave
            { slave_mosi = PU.InputPortTag "mosi"
            , slave_miso = PU.OutputPortTag "miso"
            , slave_sclk = PU.InputPortTag "sclk"
            , slave_cs = PU.InputPortTag "cs"
            }
    add "compare" CompareIO
    add "logicalUnit" LogicalUnitIO
    add "mux" MultiplexerIO

instance {-# OVERLAPPING #-} MKMicro v Float where
    mkMicroarchitecture = mkMicroarchitectureFloat
    microarchWithProtos = microarchWithProtosFloat
    defMicroarch = defMicroarchFloat
instance {-# OVERLAPPING #-} MKMicro v (Attr Float) where
    mkMicroarchitecture = mkMicroarchitectureFloat
    microarchWithProtos = microarchWithProtosFloat
    defMicroarch = defMicroarchFloat
