{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module NITTA.Model.Microarchitecture.Config (
    mkMicroarchitecture,
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
import NITTA.Intermediate.Value (Val)
import NITTA.Intermediate.Variable (Var)
import NITTA.Model.Networks.Bus (
    BusNetwork,
    addCustom,
    addCustomPrototype,
    busNetwork,
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
    | LUT
        { name :: T.Text
        }
    | Compare
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

mkMicroarchitecture :: (Val v, Var x, ToJSON a, ToJSON x) => IOSynchronization -> HashMap T.Text a -> BusNetwork T.Text x v Int
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
                configure proto LUT{name} = addPU proto name def PU.LUTIO
                configure proto Compare{name} = addPU proto name def PU.CompareIO
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
