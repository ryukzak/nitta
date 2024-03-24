{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module NITTA.Model.Microarchitecture.Config (
    MicroarchitectureConf (valueType, valueIoSync),
    parseConfig,
    mkMicroarchitecture,
) where

import Data.Aeson (
    Options (sumEncoding),
    SumEncoding (TaggedObject, contentsFieldName, tagFieldName),
    defaultOptions,
    genericParseJSON,
    genericToJSON,
 )
import Data.Default (Default (def))
import Data.Text qualified as T
import Data.Yaml (
    FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (Object),
    decodeFileThrow,
    (.:),
 )
import Data.Map as M (
    Map,
    toList
 )
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
    { pus :: [PUConf]
    , protos :: [PUConf]
    }
    deriving (Generic, Show)

instance FromJSON NetworkConf
instance ToJSON NetworkConf

data MicroarchitectureConf = MicroarchitectureConf
    { valueType :: T.Text
    , valueIoSync :: IOSynchronization
    , networks :: Map T.Text NetworkConf
    }
    deriving (Generic, Show)

instance FromJSON MicroarchitectureConf where
    parseJSON (Object v) = do
        valueType <- v .: "type"
        valueIoSync <- v .: "ioSync"
        networks <- v .: "networks"
        return MicroarchitectureConf{valueType, valueIoSync, networks}
    parseJSON v = fail $ show v
instance ToJSON MicroarchitectureConf

parseConfig :: FilePath -> IO MicroarchitectureConf
parseConfig path = do
    decodeFileThrow path :: IO MicroarchitectureConf

mkMicroarchitecture :: (Val v, Var x, ToJSON x) => MicroarchitectureConf -> BusNetwork T.Text x v Int
mkMicroarchitecture conf =
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
        nets = networks conf
        mkNetwork name net = modifyNetwork (busNetwork name $ valueIoSync conf) (build net)
     in case M.toList nets of
            [(name, net)] -> mkNetwork name net
            _ -> error "multi-networks are not currently supported"
