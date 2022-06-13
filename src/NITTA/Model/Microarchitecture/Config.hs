{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module NITTA.Model.Microarchitecture.Config (
    mkMicroarchitecture,
) where

import Data.Aeson
import Data.Default
import Data.Text qualified as T
import GHC.Generics
import NITTA.Model.Microarchitecture.Builder hiding (networks)
import NITTA.Model.ProcessorUnits qualified as PU
import NITTA.Utils

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

build NetworkConf{pus} = do mapM_ addPU pus
    where
        addPU Accum{name} = addCustom name def PU.AccumIO
        addPU Divider{name, pipeline, mock} = addCustom name (PU.divider pipeline mock) PU.DividerIO
        addPU Multiplier{name, mock} = addCustom name (PU.multiplier mock) PU.MultiplierIO
        addPU Fram{name, size} = addCustom name (PU.framWithSize size) PU.FramIO
        addPU Shift{name, sRight} = addCustom name (PU.shift $ Just False /= sRight) PU.ShiftIO
        addPU SPI{name, mosi, miso, sclk, cs, isSlave, bounceFilter, bufferSize} =
            addCustom name (PU.anySPI bounceFilter bufferSize) $
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

data NetworkConf = NetworkConf
    { name :: T.Text
    , pus :: [PUConf]
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

mkMicroarchitecture ioSync toml =
    let ns = networks (getFromToml toml :: MicroarchitectureConf)
        mkNetwork net@NetworkConf{name} = defineNetwork name ioSync $ build net
     in if length ns > 1
            then error "multi-networks are not currently supported"
            else mkNetwork (head ns)
