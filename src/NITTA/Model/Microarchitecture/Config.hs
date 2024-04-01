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

data PULibrary = PULibrary
    { isSlave :: Bool
    , bufferSize :: Maybe Int
    , bounceFilter :: Int
    }
    deriving (Generic, Show)

instance FromJSON PULibrary
instance ToJSON PULibrary

data PUConf
    = Accum
    | Divider
        { pipeline :: Int
        }
    | Multiplier
    | Fram
        { size :: Int
        }
    | SPI
        { mosi :: T.Text
        , miso :: T.Text
        , sclk :: T.Text
        , cs :: T.Text
        }
    | Shift
        { sRight :: Maybe Bool
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
    { pus :: Map T.Text PUConf
    , protos :: Map T.Text PUConf
    }
    deriving (Generic, Show)

instance FromJSON NetworkConf
instance ToJSON NetworkConf

data MicroarchitectureConf = MicroarchitectureConf
    { mock :: Bool
    , valueType :: T.Text
    , valueIoSync :: IOSynchronization
    , puLibrary :: PULibrary
    , networks :: Map T.Text NetworkConf
    }
    deriving (Generic, Show)

instance FromJSON MicroarchitectureConf where
    parseJSON (Object v) = do
        mock <- v .: "mock"
        valueType <- v .: "type"
        valueIoSync <- v .: "ioSync"
        puLibrary <- v .: "puLibrary"
        networks <- v .: "networks"
        return MicroarchitectureConf{mock, valueType, valueIoSync, puLibrary, networks}
    parseJSON v = fail $ show v
instance ToJSON MicroarchitectureConf

parseConfig :: FilePath -> IO MicroarchitectureConf
parseConfig path = do
    decodeFileThrow path :: IO MicroarchitectureConf

mkMicroarchitecture :: (Val v, Var x, ToJSON x) => MicroarchitectureConf -> BusNetwork T.Text x v Int
mkMicroarchitecture MicroarchitectureConf{mock, valueIoSync, puLibrary, networks} =
    let addPU proto
            | proto = addCustomPrototype
            | otherwise = addCustom
        isSlave_ = isSlave puLibrary
        bufferSize_ = bufferSize puLibrary
        bounceFilter_ = bounceFilter puLibrary
        build NetworkConf{pus, protos} = do
            mapM_ (configure_ False) $ M.toList pus
            mapM_ (configure_ True) $ M.toList protos
            where
                configure_ proto (name, pu) = configure proto name pu
                configure proto name Accum = addPU proto name def PU.AccumIO
                configure proto name Divider{pipeline} = addPU proto name (PU.divider pipeline mock) PU.DividerIO
                configure proto name Multiplier = addPU proto name (PU.multiplier mock) PU.MultiplierIO
                configure proto name Fram{size} = addPU proto name (PU.framWithSize size) PU.FramIO
                configure proto name Shift{sRight} = addPU proto name (PU.shift $ Just False /= sRight) PU.ShiftIO
                configure proto name SPI{mosi, miso, sclk, cs} =
                    addPU proto name (PU.anySPI bounceFilter_ bufferSize_) $
                        if isSlave_
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
        mkNetwork name net = modifyNetwork (busNetwork name $ valueIoSync) (build net)
     in case M.toList networks of
            [(name, net)] -> mkNetwork name net
            _ -> error "multi-networks are not currently supported"
