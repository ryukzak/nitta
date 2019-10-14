{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS -fno-warn-overlapping-patterns -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-cse #-}

{-|
Module      : Main
Description : NITTA CAD executable
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module Main ( main ) where

import           Control.Monad                    (void, when)
import           Data.Default                     (def)
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text.IO                     as T
import           GHC.TypeLits
import           NITTA.Frontend
import           NITTA.Intermediate.Types
import           NITTA.Model.MicroArchitecture
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Networks.Types
import           NITTA.Model.ProcessorUnits
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Project                    (TargetSynthesis (..),
                                                   mkModelWithOneNetwork,
                                                   runTargetSynthesis)
import           NITTA.UIBackend
import           System.Console.CmdArgs           hiding (def)
import           Text.InterpolatedString.Perl6    (qc)
import           Text.Regex


-- |Command line interface.
data Nitta
    = Nitta
        { web       :: Bool
        , port      :: Int
        , npm_build :: Bool
        , type_     :: String
        , io_sync   :: IOSynchronization
        , file      :: FilePath
        }
    deriving ( Show, Data, Typeable )

deriving instance Data IOSynchronization

nittaArgs = Nitta
    { web=False &= help "Run web server"
    , port=8080 &= help "WebUI port"
    , npm_build=False &= help "No regenerate WebUI static files"
    , type_="fx32.32" &= help "Bus type, default value: \"fx32.32\""
    , io_sync=Sync &= help "IO synchronization mode: sync, async, onboard"
    , file=def &= args &= typFile
    }


parseFX input = let
        typePattern = mkRegex "fx([0-9]+).([0-9]+)"
        [ m, b ] = fromMaybe (error "incorrect Bus type input") $ matchRegex typePattern input
        convert = fromJust . someNatVal . read
    in ( convert m, convert b )


main = do
    Nitta{ web, port, npm_build, file, type_, io_sync } <- cmdArgs nittaArgs
    when npm_build prepareStaticFiles
    putStrLn [qc|> readFile: { file }|]
    when (null file) $ error "input file not specified"
    src <- T.readFile file
    let cadDesc = if web then Just port else Nothing

    ( \( SomeNat (_ :: Proxy m), SomeNat (_ :: Proxy b) ) ->
          selectCAD cadDesc src ( microarch io_sync :: BusNetwork String String (FX m b) Int)
        ) $ parseFX type_


selectCAD (Just port) src ma = backendServer port $ mkModelWithOneNetwork ma $ lua2functions src

selectCAD Nothing src ma = void $ runTargetSynthesis def
        { tName="main"
        , tMicroArch=ma
        , tDFG=lua2functions src
        , tVerbose=True
        , tReceivedValues=def
        }


-- FIXME: В настоящее время при испытании на стенде сигнал rst не приводит к сбросу вычислителя в начальное состояние.

-- TODO: Необходимо иметь возможность указать, какая именно частота будет у целевого вычислителя. Данная задача связана
-- с задачей о целевой платформе.

microarch ioSync = evalNetwork ioSync $ do
        addManual "fram1" (PU def def FramPorts{ oe=SignalTag 0, wr=SignalTag 1, addr=map SignalTag [2, 3, 4, 5] } FramIO )
        add "fram2" FramIO
        add "shift" ShiftIO
        add "mul" MultiplierIO
        add "accum" AccumIO
        add "div" DividerIO
        add "spi" $ SPISlave
            { slave_mosi = InputPortTag "mosi"
            , slave_miso = OutputPortTag "miso"
            , slave_sclk = InputPortTag "sclk"
            , slave_cs   = InputPortTag "cs"
            }

