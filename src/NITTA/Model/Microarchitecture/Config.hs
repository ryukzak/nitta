{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module NITTA.Model.Microarchitecture.Config (
    MicroarchitectureConf (..),
    NetworkConf (..),
    PUConf (..),
    parseConfig,
    saveConfig,
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
import Data.Map as M (
    Map,
    toList,
 )
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Yaml (
    FromJSON (parseJSON),
    ToJSON (toJSON),
    decodeFileThrow,
    encodeFile
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
        , isSlave :: Bool
        , bufferSize :: Maybe Int
        , bounceFilter :: Int
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
    { pus :: Maybe (Map T.Text PUConf)
    , protos :: Maybe (Map T.Text PUConf)
    }
    deriving (Generic, Show)

instance FromJSON NetworkConf
instance ToJSON NetworkConf

data MicroarchitectureConf = MicroarchitectureConf
    { mock :: Bool
    , ioSync :: IOSynchronization
    , valueType :: T.Text
    , library :: Maybe (Map T.Text PUConf)
    , networks :: Map T.Text NetworkConf
    }
    deriving (Generic, Show)

instance FromJSON MicroarchitectureConf where
instance ToJSON MicroarchitectureConf

parseConfig :: FilePath -> IO MicroarchitectureConf
parseConfig path = do
    decodeFileThrow path :: IO MicroarchitectureConf

saveConfig :: FilePath -> MicroarchitectureConf -> IO ()
saveConfig path conf = do
    encodeFile (path <> "/microarch.yml") conf

mkMicroarchitecture :: (Val v, Var x, ToJSON x) => MicroarchitectureConf -> BusNetwork T.Text x v Int
mkMicroarchitecture MicroarchitectureConf{mock, ioSync, library, networks} =
    let addPU proto
            | proto = addCustomPrototype
            | otherwise = addCustom
        build NetworkConf{pus, protos} = do
            mapM_ (configure_ False) $ M.toList $ fromMaybe def pus
            mapM_ (configure_ True) $ M.toList $ fromMaybe def protos
            mapM_ (configure_ True) $ M.toList $ fromMaybe def library
            where
                configure_ proto (name, pu) = configure proto name pu
                configure proto name Accum = addPU proto name def PU.AccumIO
                configure proto name Divider{pipeline} = addPU proto name (PU.divider pipeline mock) PU.DividerIO
                configure proto name Multiplier = addPU proto name (PU.multiplier mock) PU.MultiplierIO
                configure proto name Fram{size} = addPU proto name (PU.framWithSize size) PU.FramIO
                configure proto name Shift{sRight} = addPU proto name (PU.shift $ Just False /= sRight) PU.ShiftIO
                configure proto name SPI{mosi, miso, sclk, cs, isSlave, bufferSize, bounceFilter} =
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
        mkNetwork name net = modifyNetwork (busNetwork name ioSync) (build net)
     in case M.toList networks of
            [(name, net)] -> mkNetwork name net
            _ -> error "multi-networks are not currently supported"
