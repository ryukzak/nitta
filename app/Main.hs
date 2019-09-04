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
import qualified Data.Text.IO                     as T
import           NITTA.Conf
import           NITTA.Frontend
import           NITTA.Intermediate.Types
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
    , io_sync=Sync &= help "IO synchronization mode: sync, async, onboatd"
    , file=def &= args &= typFile
    }

main = do
    Nitta{ web, port, npm_build, file, type_, io_sync } <- cmdArgs nittaArgs
    when npm_build prepareStaticFiles
    putStrLn [qc|> readFile: { file }|]
    when (null file) $ error "input file not specified"
    src <- T.readFile file
    let runner = if web
        then runWebUI port $ lua2functions src
        else runTestbench $ lua2functions src
    case type_ of
        "fx24.32" -> runner (microarch io_sync :: BusNetwork String String (FX 24 32) Int)
        "fx32.32" -> runner (microarch io_sync :: BusNetwork String String (FX 32 32) Int)
        _ -> error "Wrong bus type"

runWebUI port alg ma = backendServer port $ mkModelWithOneNetwork ma alg
runTestbench tDFG tMicroArch
    = void $ runTargetSynthesis (def :: TargetSynthesis _ _ _ Int)
        { tName="main"
        , tMicroArch
        , tDFG
        , tVerbose=True
        , tReceivedValues=filter (\(v, _x) -> notElem v $ variables tDFG)
            [ ( "a:0", [1, 2, 3, 4, 5, 6] )
            , ( "b:0", [1, 1, 1, 1, 1, 1] )
            ]
        }


-- FIXME: В настоящее время при испытании на стенде сигнал rst не приводит к сбросу вычислителя в начальное состояние.

-- TODO: Необходимо иметь возможность указать, какая именно частота будет у целевого вычислителя. Данная задача связана
-- с задачей о целевой платформе.

microarch = evalNetwork $ do
        addManual "fram1" (PU def def FramPorts{ oe=SignalTag (10), wr=SignalTag (11), addr=map SignalTag [66, 44, 55, 49] } FramIO )
        addFram "fram2"
        addAccum "accum"
        addDiv "div"
        addMul "mul"
        addSPI "spi" "slave" "mosi" "miso" "sclk" "cs"

